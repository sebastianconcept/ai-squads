---
description: Technical Design - Itau PDF Parser Implementation
type: technical-design
status: planned
priority: high
---

# Technical Design: Itau PDF Parser Implementation

## Architecture Overview

### Design Principles
- **Consistency**: Follow BancoBrasilPdfParser pattern exactly
- **Maintainability**: Use established patterns for easy maintenance
- **Performance**: Optimize for <30 second processing time
- **Accuracy**: Achieve >99% parsing accuracy
- **Extensibility**: Enable easy addition of other Brazilian banks

### High-Level Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   PDF Upload    │    │  Text Extraction│    │ Transaction     │
│   (Itau PDF)    │───▶│   (OCR/Text)    │───▶│ Detection       │
│                 │    │                 │    │ (Itau Patterns) │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                                       │
                                                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  Standardized   │◀───│  Validation &   │◀───│  ItauTransaction│
│  Output (JSON/ │    │  Confidence     │    │   Conversion     │
│  CSV/Normalized)│    │   Scoring       │    │  to Normalized   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Data Structures

### ItauTransaction
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ItauTransaction {
    /// Transaction date
    pub data: NaiveDate,
    /// Transaction description
    pub descricao: String,
    /// Transaction amount (positive for credits, negative for debits)
    pub valor: Decimal,
    /// Transaction type (C for Credit, D for Debit)
    pub tipo: String,
    /// Balance after transaction (if available)
    pub saldo: Option<Decimal>,
    /// Additional reference information
    pub referencia: Option<String>,
    /// Transaction category (if available)
    pub categoria: Option<String>,
}
```

### BancoItauExtrato
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BancoItauExtrato {
    /// List of transactions in Itau format
    pub transacoes: Vec<ItauTransaction>,
    /// Balance information from the statement
    pub saldos: Vec<ItauSaldo>,
    /// Statement period information
    pub periodo: ItauPeriodo,
    /// Account information
    pub conta: ItauConta,
}
```

### Supporting Structures
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ItauSaldo {
    /// Balance date
    pub data: NaiveDate,
    /// Balance amount
    pub valor: Decimal,
    /// Balance type
    pub tipo: String,
    /// Additional balance information
    pub descricao: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ItauPeriodo {
    /// Start date of the statement period
    pub data_inicio: NaiveDate,
    /// End date of the statement period
    pub data_fim: NaiveDate,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ItauConta {
    /// Account number
    pub conta: String,
    /// Account holder name
    pub titular: Option<String>,
    /// Agency (if available)
    pub agencia: Option<String>,
}
```

## Parser Implementation

### ItauPdfParser Structure
```rust
#[derive(Debug, Clone)]
pub struct ItauPdfParser {
    bank_info: BankInfo,
}

impl Default for ItauPdfParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ItauPdfParser {
    pub fn new() -> Self {
        Self {
            bank_info: BankInfo {
                name: "Banco Itau".to_string(),
                code: "341".to_string(),
                supported_formats: vec!["PDF".to_string()],
                parser_version: "1.0.0".to_string(),
                confidence_score: 0.95,
            },
        }
    }
}
```

### IndividualBankParser Implementation
```rust
#[async_trait]
impl IndividualBankParser for ItauPdfParser {
    async fn score_content(&self, extracted_text: &str) -> Result<f32, ParseError> {
        // Itau-specific confidence scoring
        let itau_indicators = [
            "BANCO ITAU",
            "ITAU UNIBANCO",
            "EXTRATO DE CONTA",
            "EXTRATO CONTA",
            "AGENCIA",
            "CONTA",
            "SALDO",
            "LANCAMENTOS",
            "MOVIMENTACAO",
            "TRANSACAO",
            "ITAU",
            "341", // Itau bank code
            "UNIBANCO",
            "EXTRATO",
            "CORRENTE",
            "DEPOSITO",
            "SAQUE",
            "TRANSFERENCIA",
            "PAGAMENTO",
            "RECEBIMENTO",
            "TARIFA",
            "JUROS",
            "RENDIMENTO",
            "APLICACAO",
        ];

        // Calculate confidence based on found indicators
        let text_upper = extracted_text.to_uppercase();
        let mut matches = 0;
        let mut found_indicators = Vec::new();

        for indicator in &itau_indicators {
            let indicator_upper = indicator.to_uppercase();
            if text_upper.contains(&indicator_upper) {
                matches += 1;
                found_indicators.push(*indicator);
            }
        }

        // Calculate confidence score
        let confidence: f32 = if matches >= 5 {
            0.9 // High confidence for 5+ matches
        } else if matches >= 3 {
            0.7 // Good confidence for 3+ matches
        } else if matches >= 2 {
            0.5 // Minimum confidence for 2+ matches
        } else if matches >= 1 {
            0.3 // Low confidence for 1+ match
        } else {
            0.0 // No confidence
        };

        // Boost confidence for key indicators
        let boosted_confidence = if text_upper.contains("BANCO ITAU") && text_upper.contains("EXTRATO") {
            confidence.max(0.8)
        } else if text_upper.contains("BANCO ITAU") {
            confidence.max(0.6)
        } else {
            confidence
        };

        Ok(boosted_confidence.min(1.0))
    }

    async fn parse_content(&self, extracted_text: &str) -> Result<Vec<NormalizedTransaction>, ParseError> {
        // Parse Itau transactions and convert to NormalizedTransaction
        let itau_transactions = self.parse_itau_transactions(extracted_text)?;
        let normalized_transactions: Vec<NormalizedTransaction> = itau_transactions
            .into_iter()
            .map(NormalizedTransaction::from)
            .collect();
        Ok(normalized_transactions)
    }

    fn bank_info(&self) -> BankInfo {
        self.bank_info.clone()
    }
}
```

## Conversion Implementations

### From<ItauTransaction> for NormalizedTransaction
```rust
impl From<ItauTransaction> for NormalizedTransaction {
    fn from(transacao: ItauTransaction) -> Self {
        // Itau uses positive numbers for credits, negative for debits
        let transaction_type = if transacao.valor >= Decimal::ZERO {
            TransactionType::Credit
        } else {
            TransactionType::Debit
        };

        NormalizedTransaction {
            date: transacao.data,
            description: transacao.descricao,
            amount: transacao.valor.abs(), // Use absolute value for amount
            balance: transacao.saldo,
            transaction_type,
            category: transacao.categoria,
            reference: transacao.referencia,
        }
    }
}
```

### From<ItauSaldo> for BalanceInfo
```rust
impl From<ItauSaldo> for BalanceInfo {
    fn from(saldo: ItauSaldo) -> Self {
        let balance_type = match saldo.tipo.as_str() {
            "Saldo Inicial" => BalanceType::Opening,
            "Saldo Final" => BalanceType::Closing,
            "SALDO" => BalanceType::Closing,
            _ => BalanceType::Unknown,
        };

        BalanceInfo {
            amount: saldo.valor,
            date: Some(saldo.data),
            balance_type,
            description: saldo.descricao,
        }
    }
}
```

### From<BancoItauExtrato> for NormalizedBankStatement
```rust
impl From<BancoItauExtrato> for NormalizedBankStatement {
    fn from(extrato: BancoItauExtrato) -> Self {
        let transactions: Vec<NormalizedTransaction> = extrato.transacoes
            .into_iter()
            .map(NormalizedTransaction::from)
            .collect();
        let balances: Vec<BalanceInfo> = extrato.saldos
            .into_iter()
            .map(BalanceInfo::from)
            .collect();
        
        Self {
            transactions,
            balances,
            period_start: Some(extrato.periodo.data_inicio),
            period_end: Some(extrato.periodo.data_fim),
            account_info: Some(format!("Conta: {}", extrato.conta.conta)),
            bank_name: "Banco Itau".to_string(),
        }
    }
}
```

## Parsing Logic

### Transaction Parsing
```rust
impl ItauPdfParser {
    /// Parse Itau transactions from extracted text
    fn parse_itau_transactions(&self, text: &str) -> Result<Vec<ItauTransaction>, ParseError> {
        let mut transactions = Vec::new();
        let lines: Vec<&str> = text.lines().collect();

        // Find transaction start (after "Lançamentos" or similar)
        let mut transaction_start = 0;
        for (i, line) in lines.iter().enumerate() {
            if line.contains("Lançamentos") || 
               line.contains("Lancamentos") || 
               line.contains("MOVIMENTACAO") ||
               line.contains("TRANSACOES") {
                transaction_start = i + 1;
                break;
            }
        }

        // Parse transactions
        for line in &lines[transaction_start..] {
            let trimmed_line = line.trim();
            if trimmed_line.is_empty() {
                continue;
            }

            // Check if this is a balance line (after last transaction of a day)
            if self.is_itau_balance_line(trimmed_line) {
                // Extract balance information and continue
                continue;
            }

            if let Ok(transaction) = self.parse_itau_transaction_line(trimmed_line) {
                transactions.push(transaction);
            }
        }

        Ok(transactions)
    }

    /// Check if this is an Itau balance line
    /// Itau adds balance lines after the last transaction of each day
    fn is_itau_balance_line(&self, line: &str) -> bool {
        // Look for patterns that indicate balance lines in Itau statements
        line.contains("Saldo Disponível") ||
        line.contains("SALDO DISPONIVEL") ||
        line.contains("Saldo Atual") ||
        line.contains("SALDO ATUAL") ||
        line.contains("Saldo em") ||
        line.contains("SALDO EM")
    }

    /// Parse a single Itau transaction line
    fn parse_itau_transaction_line(&self, line: &str) -> Result<ItauTransaction, ParseError> {
        // Itau transaction format analysis needed
        // This is a placeholder implementation
        // Real implementation will depend on actual Itau statement format
        
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 3 {
            return Err(ParseError::ParsingFailed(
                "Insufficient parts in Itau transaction line".to_string(),
            ));
        }

        // Parse date (first part: DD/MM/YYYY)
        let date_str = parts[0];
        let date = self.parse_itau_date(date_str)?;

        // Parse amount and type
        let (amount, transaction_type) = self.parse_itau_amount_and_type(&parts[1..])?;

        // Parse description
        let description = self.parse_itau_description(&parts[2..])?;

        Ok(ItauTransaction {
            data: date,
            descricao: description,
            valor: amount,
            tipo: transaction_type,
            saldo: None, // Will be extracted if available
            referencia: None,
            categoria: None,
        })
    }

    /// Parse Itau date format
    fn parse_itau_date(&self, date_str: &str) -> Result<NaiveDate, ParseError> {
        // Handle DD/MM/YYYY format
        if date_str.len() == 10 && date_str.matches('/').count() == 2 {
            let parts: Vec<&str> = date_str.split('/').collect();
            if parts.len() == 3 {
                if let (Ok(day), Ok(month), Ok(year)) = (
                    parts[0].parse::<u32>(),
                    parts[1].parse::<u32>(),
                    parts[2].parse::<i32>(),
                ) {
                    return chrono::NaiveDate::from_ymd_opt(year, month, day)
                        .ok_or_else(|| ParseError::ParsingFailed(format!("Invalid date: {}", date_str)));
                }
            }
        }
        Err(ParseError::ParsingFailed(format!("Invalid date format: {}", date_str)))
    }

    /// Parse Itau amount and transaction type
    /// Itau uses positive numbers for credits, negative numbers for debits
    fn parse_itau_amount_and_type(&self, parts: &[&str]) -> Result<(Decimal, String), ParseError> {
        // Parse Brazilian amount format (number.number,number)
        // Look for amount pattern - Itau uses positive/negative numbers
        for part in parts {
            if part.contains(',') {
                let amount = self.parse_brazilian_amount(part)?;
                // Determine transaction type based on sign
                let transaction_type = if amount >= Decimal::ZERO {
                    "C".to_string() // Credit (positive)
                } else {
                    "D".to_string() // Debit (negative)
                };
                return Ok((amount, transaction_type));
            }
        }
        Err(ParseError::ParsingFailed("No amount found in transaction line".to_string()))
    }

    /// Parse Itau transaction description
    fn parse_itau_description(&self, parts: &[&str]) -> Result<String, ParseError> {
        // Extract description from remaining parts
        // Skip amount and type parts
        let description_parts: Vec<&str> = parts.iter()
            .filter(|part| !part.contains(',') && *part != &"C" && *part != &"D")
            .copied()
            .collect();
        
        Ok(description_parts.join(" "))
    }

    /// Parse Brazilian amount format
    /// Handles positive and negative numbers (Itau uses negative for debits)
    fn parse_brazilian_amount(&self, amount_str: &str) -> Result<Decimal, ParseError> {
        let trimmed = amount_str.trim();
        
        // Check for negative sign
        let is_negative = trimmed.starts_with('-');
        let cleaned = if is_negative { &trimmed[1..] } else { trimmed };
        
        if cleaned.contains(',') {
            let parts: Vec<&str> = cleaned.split(',').collect();
            if parts.len() == 2 {
                let integer_part = parts[0].replace('.', ""); // Remove thousands separators
                let decimal_part = parts[1];
                let normalized = format!("{}.{}", integer_part, decimal_part);
                
                let amount = Decimal::from_str(&normalized).map_err(|e| {
                    ParseError::ParsingFailed(format!("Failed to parse Brazilian amount '{}': {}", amount_str, e))
                })?;
                
                // Apply negative sign if needed
                Ok(if is_negative { -amount } else { amount })
            } else {
                let normalized = cleaned.replace(',', ".");
                let amount = Decimal::from_str(&normalized).map_err(|e| {
                    ParseError::ParsingFailed(format!("Failed to parse amount '{}': {}", amount_str, e))
                })?;
                Ok(if is_negative { -amount } else { amount })
            }
        } else {
            let normalized = cleaned.replace('.', "");
            let amount = Decimal::from_str(&normalized).map_err(|e| {
                ParseError::ParsingFailed(format!("Failed to parse amount '{}': {}", amount_str, e))
            })?;
            Ok(if is_negative { -amount } else { amount })
        }
    }
}
```

## Integration Points

### Parser Registry Integration
```rust
// In bank_parser.rs
pub enum BankParser {
    // ... existing parsers
    ItauPdf(ItauPdfParser),
}

impl BankParser {
    pub fn itau_pdf() -> Self {
        Self::ItauPdf(ItauPdfParser::new())
    }
}
```

### Bank Results Integration
```rust
// In bank_results.rs
// Add Itau structures alongside existing BancoBrasil structures
pub use crate::models::bank_results::{
    BancoBrasilConta, BancoBrasilExtrato, BancoBrasilPeriodo, BancoBrasilSaldo, BancoBrasilTransacao,
    ItauConta, BancoItauExtrato, ItauPeriodo, ItauSaldo, ItauTransaction, // New Itau structures
    NormalizedBankStatement,
};
```

## Testing Strategy

### Unit Tests
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_itau_transaction_parsing() {
        let parser = ItauPdfParser::new();
        
        let test_text = r#"EXTRATO BANCO ITAU
CONTA CORRENTE
AGENCIA: 1234 CONTA: 56789-0
PERIODO: 01/12/2024 A 31/12/2024

Lancamentos

15/12/2024 Pagamento de Boleto -1.500,00 ENERGIA ELETRICA
16/12/2024 Transferencia PIX 2.000,00 JOÃO DA SILVA
17/12/2024 Saque ATM -500,00"#;

        let transactions = parser.parse_itau_transactions(test_text).unwrap();
        
        assert_eq!(transactions.len(), 3);
        
        // Test first transaction
        let first_transaction = &transactions[0];
        assert_eq!(first_transaction.data.to_string(), "2024-12-15");
        assert_eq!(first_transaction.valor.to_string(), "-1500.00"); // Negative for debit
        assert_eq!(first_transaction.tipo, "D"); // Debit
        assert!(first_transaction.descricao.contains("Pagamento de Boleto"));
    }

    #[tokio::test]
    async fn test_itau_confidence_scoring() {
        let parser = ItauPdfParser::new();
        
        let itau_text = "BANCO ITAU EXTRATO DE CONTA CORRENTE";
        let confidence = parser.score_content(itau_text).await.unwrap();
        
        assert!(confidence > 0.8);
    }

    #[test]
    fn test_itau_transaction_conversion() {
        let itau_transaction = ItauTransaction {
            data: NaiveDate::from_ymd_opt(2024, 12, 15).unwrap(),
            descricao: "Test transaction".to_string(),
            valor: Decimal::new(-150000, 2), // -1500.00 (negative for debit)
            tipo: "D".to_string(),
            saldo: Some(Decimal::new(500000, 2)), // 5000.00
            referencia: None,
            categoria: None,
        };

        let normalized: NormalizedTransaction = itau_transaction.into();
        
        assert_eq!(normalized.date.to_string(), "2024-12-15");
        assert_eq!(normalized.amount.to_string(), "1500.00"); // Absolute value
        assert_eq!(normalized.transaction_type, TransactionType::Debit);
        assert_eq!(normalized.balance, Some(Decimal::new(500000, 2)));
    }
}
```

## Performance Considerations

### Optimization Strategies
- **Efficient Parsing**: Use regex patterns for fast text matching
- **Memory Management**: Process transactions in batches to avoid memory issues
- **Caching**: Cache parsed patterns and confidence scores
- **Parallel Processing**: Support concurrent processing of multiple statements

### Performance Targets
- **Processing Time**: <30 seconds per Itau statement
- **Memory Usage**: <500MB during processing
- **Accuracy**: >99% parsing accuracy
- **Concurrent Users**: Support 50+ concurrent users

## Security Considerations

### Data Protection
- **Input Validation**: Comprehensive validation of Itau PDF inputs
- **Error Handling**: Secure error messages without data leakage
- **Access Control**: Proper authentication and authorization
- **Data Encryption**: Secure handling of Itau financial data

### Compliance
- **LGPD Compliance**: Brazilian data protection compliance
- **Financial Data**: Secure handling of sensitive financial information
- **Audit Trail**: Comprehensive logging for compliance

## Deployment Considerations

### Environment Requirements
- **OCR Service**: Existing Tesseract OCR with Portuguese language support
- **Database**: Existing PostgreSQL schema
- **File Storage**: Existing temporary file storage system
- **Dependencies**: Existing Rust dependencies and libraries

### Configuration
- **Parser Settings**: Configurable confidence thresholds
- **Performance Tuning**: Configurable processing timeouts
- **Error Handling**: Configurable error recovery strategies
- **Logging**: Configurable logging levels and outputs

## Notes

This technical design follows the established Banco do Brasil pattern exactly, ensuring consistency and maintainability. The implementation will provide comprehensive Itau PDF support while maintaining the same quality standards and user experience as existing bank parsers.

---
description: Feature Solution - Universal Bank Detection System
type: feature-solution
status: planned
---

# Solution: Universal Bank Detection System

## Solution Overview

Implement a **universal bank detection system** that automatically detects banks and formats, then routes to the appropriate specialized parser. This approach eliminates the need for bank-specific unified parsers and provides a single, clean interface for all banks. The solution builds upon existing individual parsers to provide seamless user experience and complete format coverage.

### Core Components
1. **Universal Bank Detection**: Smart bank and format detection system (NEW)
2. **Individual Parser Integration**: Direct integration of existing parsers into main system (NEW)
3. **Mock Parser Implementation**: Mock parsers for Banco Brasil CSV/OFX (no samples available) (NEW)
4. **Enhanced Testing**: Comprehensive test coverage for all formats (NEW)
5. **Enhanced Error Handling**: Bank-specific error handling (NEW)

## Technical Approach

### Architecture Design

#### 1. Universal Bank Detection System (NEW)
```rust
/// Universal bank detector that works for all banks
pub struct BankDetector {
    parsers: Vec<Box<dyn IndividualBankParser>>,
    confidence_threshold: f32,
}

impl BankDetector {
    pub async fn detect_bank(&self, content: &[u8]) -> Result<BankDetectionResult, ParseError> {
        let mut results = Vec::new();
        
        // Run all parsers and collect confidence scores
        for parser in &self.parsers {
            let confidence = parser.score_content(content).await?;
            results.push(BankDetectionCandidate {
                bank_info: parser.bank_info(),
                confidence,
                parser: parser.clone(),
            });
        }
        
        // Sort by confidence
        results.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap());
        
        let best_match = &results[0];
        
        if best_match.confidence >= self.confidence_threshold {
            Ok(BankDetectionResult::Detected(best_match.clone()))
        } else {
            Ok(BankDetectionResult::Ambiguous(results))
        }
    }
}

pub enum BankDetectionResult {
    Detected(BankDetectionCandidate),
    Ambiguous(Vec<BankDetectionCandidate>), // UI shows list for user selection
}

#[derive(Debug, Clone)]
pub struct BankDetectionCandidate {
    pub bank_info: BankInfo,
    pub confidence: f32,
    pub parser: Box<dyn IndividualBankParser>,
}
```

#### 2. Individual Parser Integration
```rust
// Direct integration of existing parsers into BankParser enum
pub enum BankParser {
    /// Parser for Banco do Brasil PDF statements
    BancoBrasilPdf(BancoBrasilPdfParser),
    /// Parser for Banco do Brasil CSV statements (MOCK - no samples)
    BancoBrasilCsv(BancoBrasilCsvParser),
    /// Parser for Banco do Brasil OFX statements (MOCK - no samples)
    BancoBrasilOfx(BancoBrasilOfxParser),
    /// Parser for Itau PDF statements
    ItauPdf(ItauPdfParser),
    /// Parser for Itau CSV statements
    ItauCsv(ItauCsvParser),
    /// Parser for Banco Inter CSV statements
    BancoInterCsv(BancoInterCsvParser),
    /// Parser for Banco Inter PDF statements
    BancoInterPdf(BancoInterPdfParser),
    /// Parser for Banco Inter OFX statements
    BancoInterOfx(BancoInterOfxParser),
    // ... other banks
}

impl BankParser {
    pub fn all_parsers() -> Vec<Box<dyn IndividualBankParser>> {
        vec![
            Box::new(BancoBrasilPdfParser::new()),
            Box::new(BancoBrasilCsvParser::new()),
            Box::new(BancoBrasilOfxParser::new()),
            Box::new(ItauPdfParser::new()),
            Box::new(ItauCsvParser::new()),
            Box::new(BancoInterCsvParser::new()),
            Box::new(BancoInterPdfParser::new()),
            Box::new(BancoInterOfxParser::new()),
            // ... other parsers
        ]
    }
}
```

#### 3. Mock Parser Implementation (Banco Brasil CSV/OFX)
```rust
/// Mock parser for Banco Brasil CSV (no samples available)
pub struct BancoBrasilCsvParser;

impl IndividualBankParser for BancoBrasilCsvParser {
    async fn score_content(&self, content: &[u8]) -> Result<f32, ParseError> {
        // Mock implementation - returns low confidence
        Ok(0.3) // Low confidence since we don't have real samples
    }
    
    async fn parse_content(&self, content: &[u8]) -> Result<Vec<NormalizedTransaction>, ParseError> {
        // Mock implementation - returns empty or sample data
        Err(ParseError::UnsupportedFormat("Banco Brasil CSV parser not yet implemented - no samples available".to_string()))
    }
    
    fn bank_info(&self) -> BankInfo {
        BankInfo {
            name: "Banco do Brasil".to_string(),
            code: "001".to_string(),
            formats: vec!["CSV".to_string()],
            confidence_threshold: 0.5,
        }
    }
}

/// Mock parser for Banco Brasil OFX (no samples available)
pub struct BancoBrasilOfxParser;

impl IndividualBankParser for BancoBrasilOfxParser {
    async fn score_content(&self, content: &[u8]) -> Result<f32, ParseError> {
        // Mock implementation - returns low confidence
        Ok(0.3) // Low confidence since we don't have real samples
    }
    
    async fn parse_content(&self, content: &[u8]) -> Result<Vec<NormalizedTransaction>, ParseError> {
        // Mock implementation - returns empty or sample data
        Err(ParseError::UnsupportedFormat("Banco Brasil OFX parser not yet implemented - no samples available".to_string()))
    }
    
    fn bank_info(&self) -> BankInfo {
        BankInfo {
            name: "Banco do Brasil".to_string(),
            code: "001".to_string(),
            formats: vec!["OFX".to_string()],
            confidence_threshold: 0.5,
        }
    }
}
```

### Enhanced Architecture: Universal Bank Detection & Parser Versioning

#### Universal Bank Detection Flow
```rust
pub struct BankDetector {
    parsers: Vec<Box<dyn IndividualBankParser>>,
    confidence_threshold: f32,
}

impl BankDetector {
    pub async fn detect_bank(&self, content: &[u8]) -> Result<BankDetectionResult, ParseError> {
        let mut results = Vec::new();
        
        // Run all parsers and collect confidence scores
        for parser in &self.parsers {
            let confidence = parser.score_content(content).await?;
            results.push(BankDetectionCandidate {
                bank_info: parser.bank_info(),
                confidence,
                parser: parser.clone(),
            });
        }
        
        // Sort by confidence
        results.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap());
        
        let best_match = &results[0];
        
        if best_match.confidence >= self.confidence_threshold {
            Ok(BankDetectionResult::Detected(best_match.clone()))
        } else {
            Ok(BankDetectionResult::Ambiguous(results))
        }
    }
}

pub enum BankDetectionResult {
    Detected(BankDetectionCandidate),
    Ambiguous(Vec<BankDetectionCandidate>), // UI shows list for user selection
}
```

#### Parser Versioning System
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParserVersion {
    pub id: Uuid,
    pub bank_code: String,
    pub format: String, // CSV, PDF, OFX
    pub version: String, // e.g., "1.0.0", "2.1.0"
    pub created_at: DateTime<Utc>,
    pub is_active: bool,
    pub confidence_threshold: f32,
    pub sample_files: Vec<String>, // File hashes for reference
    pub changelog: String,
}

pub struct VersionedParser {
    pub version: ParserVersion,
    pub parser: Box<dyn IndividualBankParser>,
}

impl VersionedParser {
    pub async fn parse_with_fallback(&self, content: &[u8]) -> Result<Vec<NormalizedTransaction>, ParseError> {
        // Try current version first
        match self.parser.parse(content).await {
            Ok(transactions) => Ok(transactions),
            Err(_) => {
                // Try previous versions for backward compatibility
                self.try_previous_versions(content).await
            }
        }
    }
}
```

#### Database Schema for Parser Versioning
```sql
CREATE TABLE parser_versions (
    id UUID PRIMARY KEY,
    bank_code VARCHAR(10) NOT NULL,
    format VARCHAR(10) NOT NULL,
    version VARCHAR(20) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL,
    is_active BOOLEAN DEFAULT true,
    confidence_threshold DECIMAL(3,2) NOT NULL,
    sample_file_hashes TEXT[],
    changelog TEXT,
    parser_config JSONB,
    UNIQUE(bank_code, format, version)
);

CREATE TABLE parser_performance (
    id UUID PRIMARY KEY,
    parser_version_id UUID REFERENCES parser_versions(id),
    file_hash VARCHAR(64) NOT NULL,
    confidence_score DECIMAL(3,2) NOT NULL,
    parse_time_ms INTEGER NOT NULL,
    success BOOLEAN NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE parser_requests (
    id UUID PRIMARY KEY,
    email VARCHAR(255) NOT NULL,
    bank_name VARCHAR(100) NOT NULL,
    file_hash VARCHAR(64) NOT NULL,
    file_format VARCHAR(10) NOT NULL,
    status VARCHAR(20) DEFAULT 'pending',
    created_at TIMESTAMP WITH TIME ZONE NOT NULL,
    implemented_at TIMESTAMP WITH TIME ZONE
);
```

### PDF Parser Redesign (NO OCR APPROACH)

#### Native PDF Text Extraction
```rust
use pdf_extract::extract_text;

impl BancoInterPdfParser {
    async fn extract_text_from_pdf(&self, content: &[u8]) -> Result<String, ParseError> {
        // Use pdf-extract for native PDF text extraction (no OCR)
        let text = extract_text(content)
            .map_err(|e| ParseError::TextExtractionFailed(format!("PDF text extraction failed: {}", e)))?;
        
        Ok(self.clean_pdf_text(&text))
    }
    
    /// Clean PDF text to handle spacing issues from PDF extraction
    fn clean_pdf_text(&self, text: &str) -> String {
        let mut cleaned = text.to_string();
        
        // Remove excessive whitespace and normalize spacing
        cleaned = Regex::new(r"\s+").unwrap().replace_all(&cleaned, " ").to_string();
        
        // Fix common PDF extraction issues
        cleaned = cleaned.replace("R$ ", "R$");
        cleaned = cleaned.replace("-R$", "-R$");
        cleaned = cleaned.replace("+R$", "+R$");
        
        // Clean up date patterns
        cleaned = Regex::new(r"(\d{1,2})\s+/\s+(\d{1,2})\s+/\s+(\d{4})")
            .unwrap()
            .replace_all(&cleaned, "$1/$2/$3")
            .to_string();
            
        // Clean up amount patterns
        cleaned = Regex::new(r"R\$\s*(\d{1,3}(?:\.\d{3})*(?:,\d{2})?)")
            .unwrap()
            .replace_all(&cleaned, "R$$1")
            .to_string();
            
        cleaned
    }
}
```

## Implementation Benefits

### 1. **Simplified Architecture**
- **No Unified Parsers**: Eliminates unnecessary abstraction layers
- **Direct Integration**: Individual parsers integrate directly into main system
- **Universal Detection**: Single detection system works for all banks

### 2. **Enhanced User Experience**
- **Automatic Detection**: Users don't need to specify bank or format
- **Confidence Feedback**: Clear indication of detection quality
- **Manual Fallback**: Smart fallback for ambiguous files
- **Lead Capture**: Opportunity to capture users needing new parsers

### 3. **Developer Experience**
- **Mock Parsers**: Easy to add placeholder parsers for missing formats
- **Version Management**: Built-in versioning for format changes
- **Performance Tracking**: Monitor parser performance over time
- **Extensibility**: Easy to add new banks and formats

### 4. **Business Benefits**
- **Lead Generation**: Capture users who need unsupported banks
- **Backward Compatibility**: Maintain support for old formats
- **Data-Driven Development**: Use real user files for parser development
- **Scalable Growth**: Easy to expand to new banks and markets

## Success Criteria

### Technical Success
- [ ] Universal bank detection system implemented
- [ ] All existing parsers integrated into main system
- [ ] Mock parsers for Banco Brasil CSV/OFX implemented
- [ ] Confidence scoring accuracy >80% for supported banks
- [ ] Parser versioning system operational
- [ ] Lead capture system functional

### User Experience Success
- [ ] Seamless file upload experience
- [ ] Clear feedback for detection confidence
- [ ] Manual bank selection for ambiguous files
- [ ] Lead capture for unsupported banks
- [ ] Backward compatibility for format changes

### Business Success
- [ ] Increased user engagement through better UX
- [ ] Lead generation for new parser development
- [ ] Reduced support tickets through better error handling
- [ ] Scalable architecture for future growth
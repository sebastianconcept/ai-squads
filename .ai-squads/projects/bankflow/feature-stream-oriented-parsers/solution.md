---
description: Feature Solution - Stream-Oriented Parsers
type: feature-solution
status: planned
---

# Solution: Stream-Oriented Parsers

## Solution Overview

Transform the parser architecture from memory-intensive full-content processing to efficient stream-oriented processing that handles files of any size while maintaining backward compatibility and performance.

## Technical Approach

### 1. Stream Processing Architecture

**New Stream Interface:**
```rust
pub trait TextStreamProcessor: Send + Sync {
    async fn process_line(&self, line: &str) -> Result<Option<ProcessedLine>, ParseError>;
    async fn process_stream<R: AsyncRead + Unpin>(&self, reader: R) -> Result<Vec<ProcessedLine>, ParseError>;
}
```

**Line-Level Processing:**
- Replace `clean_pdf_text(&str)` with `clean_pdf_text_line(&str)`
- Process individual lines instead of entire content
- Maintain line context for multi-line transactions

### 2. Enhanced IndividualBankParser Trait

**New Stream-Only Interface:**
```rust
#[async_trait]
pub trait IndividualBankParser: Send + Sync {
    // Stream-oriented methods (primary API)
    async fn score_stream<R: AsyncRead + Unpin>(&self, reader: R) -> Result<f32, ParseError>;
    async fn parse_stream<R: AsyncRead + Unpin>(&self, reader: R) -> Result<FormatSpecificBankStatement, ParseError>;
    
    // Line-level processing
    fn clean_text_line(&self, line: &str) -> String;
    fn process_line(&self, line: &str, context: &mut ParserContext) -> Result<Option<NormalizedTransaction>, ParseError>;
    
    fn bank_info(&self) -> BankInfo;
}
```

### 3. Two-Pass Processing Strategy

**Pass 1: Bank Detection**
- Stream through file content for bank scoring
- Use line-by-line analysis for bank identification
- Maintain minimal memory footprint during detection

**Pass 2: Content Parsing**
- Use detected bank parser for stream-based parsing
- Process transactions line-by-line
- Build result incrementally without loading full content

### 4. Parser Context Management

**Context Structure:**
```rust
pub struct ParserContext {
    pub current_date: Option<NaiveDate>,
    pub account_info: Option<AccountInfo>,
    pub balance_info: Option<BalanceInfo>,
    pub transaction_buffer: Vec<NormalizedTransaction>,
    pub line_number: usize,
}
```

**Context Benefits:**
- Maintains parsing state across lines
- Handles multi-line transactions
- Preserves bank-specific parsing logic

## User Experience

### Immediate Benefits
- **Faster Processing**: Stream processing eliminates memory bottlenecks
- **Large File Support**: Can handle files of any size without memory issues
- **Better Reliability**: Reduced risk of timeouts or memory-related failures
- **Improved Concurrency**: Multiple files can be processed simultaneously

### Clean Stream-Only Implementation
- **Deprecate Old Methods**: Remove `parse_content(&str)` and `score_content(&str)` methods
- **Stream-Only API**: All parsing goes through `parse_stream()` and `score_stream()` methods
- **Simplified Architecture**: No need to maintain dual code paths

## Implementation Plan

### Phase 1: Core Stream Infrastructure
1. **Create Stream Processing Traits**: Define `TextStreamProcessor` and enhanced `IndividualBankParser`
2. **Implement Stream Utilities**: Create line-by-line processing utilities
3. **Add Parser Context**: Implement `ParserContext` for state management

### Phase 2: Parser Migration
1. **Migrate Banco Inter Parser**: Convert to stream-oriented processing
2. **Migrate Banco Brasil Parser**: Implement stream-based parsing
3. **Migrate Itau Parser**: Add stream processing capabilities
4. **Update Other Parsers**: Migrate remaining parsers

### Phase 3: Integration and Testing
1. **Update Bank Detection**: Implement two-pass detection with streams
2. **Remove Deprecated Methods**: Clean up old `parse_content()` and `score_content()` methods
3. **Comprehensive Testing**: Ensure all parsers work with stream processing
4. **Performance Testing**: Validate performance improvements

## Format-Specific Stream Processing

### CSV Files - Perfectly Stream-Friendly ✅
**Current State**: Already processes line-by-line
**Stream Implementation**: Direct `AsyncBufRead` → `lines()` → process each line
**Benefits**: Zero memory overhead, perfect scalability

### PDF Files - Stream-Friendly with Text Extraction ⚠️
**Current State**: Full PDF text loaded into memory via `pdf_extract::extract_text_from_mem()`
**Stream Implementation**: 
- Extract PDF text to temporary stream
- Process extracted text line-by-line
- **Alternative**: Investigate streaming PDF libraries
**Benefits**: 90% memory reduction after text extraction

### OFX Files - Already Stream-Based ✅
**Current State**: Uses `quick_xml::Reader` which is already stream-based
**Stream Implementation**: Pass `AsyncRead` directly to XML reader
**Benefits**: Maintains existing XML streaming, eliminates content loading

### Internal Dependencies
- **TextExtractionService**: May need updates for stream-based extraction
- **ParseError**: Error handling must support stream processing
- **Models**: `NormalizedTransaction` and related models remain unchanged

### External Dependencies
- **tokio**: For async stream processing (`AsyncRead`, `AsyncBufRead`)
- **futures**: For stream utilities and combinators
- **regex**: For line-level pattern matching

## Risks and Mitigation

### Risk 1: Complex Multi-Line Transactions
**Mitigation**: Use `ParserContext` to maintain state and handle multi-line patterns

### Risk 2: Performance Regression for Small Files
**Mitigation**: Implement hybrid approach that uses streams for large files, memory for small files

### Risk 3: Bank Detection Accuracy
**Mitigation**: Maintain two-pass approach with comprehensive testing of detection accuracy

### Risk 4: API Changes
**Mitigation**: Clean migration with clear deprecation notices and updated documentation

## Success Criteria

### Performance Metrics
- **Memory Usage**: 90% reduction in peak memory usage for large files
- **Processing Speed**: Maintain or improve processing speed for all file sizes
- **Concurrency**: Support 5x more concurrent file processing

### Quality Metrics
- **Accuracy**: 100% parsing accuracy maintained across all banks
- **Reliability**: Zero memory-related failures or timeouts
- **API Cleanliness**: Clean stream-only API without deprecated methods

### User Experience Metrics
- **Large File Support**: Successfully process files >100MB without issues
- **Response Time**: Maintain sub-second response for files <1MB
- **Error Rate**: Reduce parsing errors by 50% through better error handling

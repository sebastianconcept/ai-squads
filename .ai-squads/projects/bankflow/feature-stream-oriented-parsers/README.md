---
description: Stream-Oriented Parsers Feature - Comprehensive Planning Complete
type: feature-readme
status: planning-complete
---

# Stream-Oriented Parsers Feature

## Overview

This feature transforms the parser architecture from memory-intensive full-content processing to efficient stream-oriented processing that handles files of any size while maintaining backward compatibility and performance.

## Problem Statement

The current parser architecture loads entire file contents into memory and processes them as complete strings, creating critical scalability issues:

- **Memory Scalability**: Large bank statement files consume excessive memory
- **Performance Bottlenecks**: Processing entire files in memory limits concurrent processing
- **Inefficient Text Processing**: Methods like `clean_pdf_text()` process entire content when only line-by-line processing is needed
- **Non-Scalable Architecture**: Current `parse_content(&str)` approach doesn't scale for large files

## Solution Approach

### Stream Processing Architecture
- **Line-by-Line Processing**: Replace full-content processing with stream-based line processing
- **Memory Efficiency**: Only hold necessary content in memory at any given time
- **Two-Pass Strategy**: Efficient bank detection followed by stream-based parsing
- **Context Management**: Maintain parsing state across lines for multi-line transactions

### Key Technical Changes
1. **New Stream Methods**: `score_stream()` and `parse_stream()` methods
2. **Line-Level Operations**: `clean_text_line()` instead of `clean_pdf_text()`
3. **Parser Context**: State management for multi-line transactions
4. **Clean API**: Remove deprecated `parse_content()` and `score_content()` methods

## Customer Jobs Analysis

### Primary Job: Process Bank Statements Efficiently
Users want to extract transaction data quickly and accurately without waiting or experiencing errors.

### Secondary Job: Handle Large Files Without Issues
Users with extensive transaction histories need reliable processing for any file size.

### Tertiary Job: Process Multiple Files Concurrently
Users managing multiple accounts need efficient batch processing capabilities.

## Success Criteria

### Primary Goals
- **Memory Efficiency**: 90% reduction in peak memory usage
- **Large File Support**: Successfully process files >100MB
- **Processing Performance**: Maintain or improve speed for all file sizes
- **Clean API**: Stream-only interface without deprecated methods

### Secondary Goals
- **Concurrency Support**: Support 10+ concurrent operations
- **Parsing Accuracy**: Maintain 100% accuracy across all banks
- **Error Handling**: Reduce parsing errors by 50%

## Implementation Plan

### Phase 1: Core Stream Infrastructure (2 weeks)
- Create stream processing traits and interfaces
- Implement stream processing utilities
- Create parser context management
- Establish comprehensive testing framework

### Phase 2: Parser Migration (3 weeks)
- Migrate Banco Inter parser to stream processing
- Migrate Banco Brasil parser to stream processing
- Migrate Itau parser to stream processing
- Migrate remaining parsers (CSV, OFX)
- Implement two-pass bank detection

### Phase 3: Integration and Optimization (1 week)
- Update main bank parser integration
- Remove deprecated methods and clean up API
- Performance optimization
- Comprehensive testing and validation
- Documentation and deployment preparation

## Format-Specific Stream Processing

### ✅ CSV Files - Perfectly Stream-Friendly
- **Current**: Already processes line-by-line
- **Implementation**: Direct `AsyncBufRead` → `lines()` → process each line
- **Benefits**: Zero memory overhead, perfect scalability

### ⚠️ PDF Files - Stream-Friendly with Text Extraction
- **Current**: Full PDF text loaded into memory
- **Implementation**: Extract PDF text to temporary stream, then process line-by-line
- **Benefits**: 90% memory reduction after text extraction

### ✅ OFX Files - Already Stream-Based
- **Current**: Uses `quick_xml::Reader` (already stream-based)
- **Implementation**: Pass `AsyncRead` directly to XML reader
- **Benefits**: Maintains existing XML streaming, eliminates content loading

### New Stream-Only Interface
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

### Parser Context Management
```rust
pub struct ParserContext {
    pub current_date: Option<NaiveDate>,
    pub account_info: Option<AccountInfo>,
    pub balance_info: Option<BalanceInfo>,
    pub transaction_buffer: Vec<NormalizedTransaction>,
    pub line_number: usize,
}
```

## Risk Mitigation

### Performance Regression Risk
**Mitigation**: Implement hybrid approach - use streams for large files, memory for small files

### Parsing Accuracy Risk
**Mitigation**: Comprehensive testing and validation against existing implementation

### Breaking Changes Risk
**Mitigation**: Clean migration with clear deprecation notices and updated documentation

### Development Complexity Risk
**Mitigation**: Incremental implementation with gradual migration approach

## Quality Gates

### Pre-Commit Quality Gates (MANDATORY)
- `cargo fmt --all -- --check` - Code formatting
- `cargo clippy --all-targets --all-features -- -D warnings` - Full Clippy
- `cargo test` - All tests passing
- `cargo check` - Code compilation
- Code review completed and approved
- Documentation updated
- JTBD analysis completed and validated

### Task Completion Quality Gates
- All acceptance criteria met
- Tests written and passing
- Documentation updated
- Performance requirements met
- Backward compatibility maintained
- Code review completed

## Dependencies

### Internal Dependencies
- **TextExtractionService**: May need updates for stream-based extraction
- **ParseError**: Error handling must support stream processing
- **Models**: Existing models remain unchanged

### External Dependencies
- **tokio**: For async stream processing (`AsyncRead`, `AsyncBufRead`)
- **futures**: For stream utilities and combinators
- **regex**: For line-level pattern matching

## Success Metrics

### Performance Metrics
- **Memory Usage**: Peak memory <50MB for any file size
- **Processing Time**: <2s for <1MB files, <10s for <10MB files
- **Concurrency**: Support 10+ concurrent operations
- **Throughput**: Process 100+ files per hour

### Quality Metrics
- **Success Rate**: >99.5% successful processing rate
- **Error Rate**: <0.5% parsing errors
- **Accuracy**: 100% parsing accuracy maintained
- **API Cleanliness**: No deprecated methods

### User Experience Metrics
- **Satisfaction**: Improved user satisfaction scores
- **Support Tickets**: 50% reduction in memory-related support tickets
- **User Retention**: Improved retention for users with large files
- **Performance Perception**: Users report faster processing

## Current Status

**Status**: Planning Complete
**Phase**: Awaiting User Review and Approval
**Next Milestone**: Implementation Start

### Completed
- ✅ Problem Analysis
- ✅ Solution Design
- ✅ JTBD Analysis
- ✅ Goal Definition
- ✅ Task Planning
- ✅ Risk Assessment

### Pending User Approval
- ⏳ Implementation Start
- ⏳ Testing Framework
- ⏳ Production Deployment

## Next Steps

1. **User Review**: Review comprehensive feature plan
2. **Approval**: Approve implementation approach and timeline
3. **Implementation**: Begin Phase 1 development
4. **Testing**: Implement comprehensive testing framework
5. **Deployment**: Deploy to production with monitoring

## Documentation

- **Problem Definition**: [problem.md](./problem.md)
- **Solution Design**: [solution.md](./solution.md)
- **JTBD Analysis**: [jtbd-analysis.md](./jtbd-analysis.md)
- **Goal Definition**: [goal.md](./goal.md)
- **Task Planning**: [tasks.md](./tasks.md)
- **Status Tracking**: [status.md](./status.md)

## Contact

For questions or concerns about this feature plan, please review the comprehensive documentation and provide feedback for any modifications needed before implementation begins.

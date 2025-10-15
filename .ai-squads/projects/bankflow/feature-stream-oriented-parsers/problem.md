---
description: Feature Problem - Stream-Oriented Parsers
type: feature-problem
priority: high
---

# Problem: Stream-Oriented Parsers

## Problem Statement

The current parser architecture loads entire file contents into memory and processes them as complete strings, which creates several critical issues:

1. **Memory Scalability**: Large bank statement files (especially PDFs with many transactions) consume excessive memory by loading the entire content at once
2. **Performance Bottlenecks**: Processing entire files in memory creates performance issues for large files and limits concurrent processing capabilities
3. **Inefficient Text Processing**: Methods like `clean_pdf_text()` process entire file content when only line-by-line processing is needed
4. **Non-Scalable Architecture**: The current `parse_content(&str)` approach doesn't scale well for very large files or high-volume processing

## User Impact

- **Slow Processing**: Users experience longer wait times when uploading large bank statements
- **Memory Issues**: Server memory consumption spikes with large files, potentially causing timeouts or crashes
- **Limited Concurrency**: Cannot efficiently process multiple large files simultaneously
- **Poor User Experience**: Users may experience timeouts or failures with large files

## Business Impact

- **Scalability Constraints**: Cannot handle enterprise customers with large transaction volumes
- **Performance Issues**: Slower processing affects user satisfaction and retention
- **Resource Inefficiency**: High memory usage increases infrastructure costs
- **Competitive Disadvantage**: Other solutions may handle large files more efficiently

## Current State

The current parser architecture works as follows:

1. **File Loading**: Entire file content is loaded into memory as `&[u8]`
2. **Text Extraction**: Complete text is extracted using `extract_text()` method
3. **Full Content Processing**: Parsers receive complete text via `parse_content(&str)`
4. **Text Cleaning**: Methods like `clean_pdf_text()` process entire content at once
5. **Line Processing**: Individual parsers iterate through lines but still hold full content in memory

**Current Flow:**
```
File → Full Content Load → Text Extraction → Full Text Processing → Line Iteration
```

## Desired State

A stream-oriented architecture that:

1. **Stream Processing**: Opens file streams and processes content line-by-line without loading entire files
2. **Memory Efficiency**: Only holds necessary content in memory at any given time
3. **Scalable Processing**: Can handle files of any size without memory constraints
4. **Concurrent Processing**: Supports efficient concurrent processing of multiple files
5. **Line-Level Operations**: Text cleaning and processing operations work on individual lines

**Desired Flow:**
```
File → Stream Open → Line-by-Line Processing → Stream Cleanup
```

## Constraints

- **Parser Complexity**: Different banks have different parsing patterns that must be preserved
- **Error Handling**: Stream processing must maintain robust error handling
- **Testing Requirements**: All existing tests must continue to pass
- **Performance Requirements**: Must not degrade performance for small files
- **API Cleanliness**: Can remove deprecated methods for cleaner implementation

## Technical Constraints

- **Rust Ownership**: Must work within Rust's ownership and borrowing rules
- **Async Compatibility**: Must maintain async/await compatibility
- **Trait Interface**: Must preserve the `IndividualBankParser` trait interface (stream methods)
- **Bank Detection**: Two-pass approach needed for bank detection and parsing

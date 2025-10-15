---
description: Feature Tasks - Universal Bank Detection System Implementation
type: feature-tasks
status: planned
---

# Tasks: Universal Bank Detection System Implementation

## Overview

This document outlines the implementation tasks for the Universal Bank Detection System, which eliminates the need for bank-specific unified parsers and provides a single, clean interface for all banks.

### Key Changes from Original Plan
- **Removed**: `BancoInterUnifiedParser` (unnecessary with universal detection)
- **Renamed**: `UniversalBankDetector` → `BankDetector`
- **Added**: Mock parsers for Banco Brasil CSV/OFX (no samples available)
- **Simplified**: Direct integration of individual parsers into main system

## Implementation Timeline

### Phase 1: Universal Bank Detection System (2-3 days)
### Phase 2: Testing & Validation (1 day)
### Phase 3: Enhanced Architecture Implementation (4 weeks)

---

## Phase 1: Universal Bank Detection System (2-3 days)

### 1.1 Create BankDetector System
**Agent**: @agent:rusty  
**Estimated Time**: 4-6 hours  
**Priority**: High  

#### Tasks:
- [ ] **Create `bank_detector.rs` module**
  - Create file: `crates/api/src/parsers/bank_detector.rs`
  - Implement `BankDetector` struct
  - Add `BankDetectionResult` and `BankDetectionCandidate` enums
  - Add comprehensive documentation and examples

- [ ] **Implement universal detection logic**
  - Add confidence scoring across all parsers
  - Add threshold-based detection (high confidence = auto-detect)
  - Add ambiguous file handling (low confidence = manual selection)
  - Add performance optimization for large parser lists

- [ ] **Implement detection result handling**
  - Handle `Detected` case (high confidence)
  - Handle `Ambiguous` case (low confidence)
  - Add sorting by confidence score
  - Add error handling for detection failures

#### Success Criteria:
- [ ] BankDetector module created and compiles
- [ ] Detection accuracy >80% for supported banks
- [ ] Proper handling of ambiguous files
- [ ] All Rust quality gates pass (`cargo fmt`, `cargo clippy`, `cargo test`)

#### Dependencies:
- All existing individual parsers
- `IndividualBankParser` trait
- `ParseError` and `NormalizedTransaction` types

### 1.2 Individual Parser Integration
**Agent**: @agent:rusty  
**Estimated Time**: 2-3 hours  
**Priority**: High  

#### Tasks:
- [ ] **Update `BankParser` enum with individual parsers**
  - Update `crates/api/src/parsers/bank_parser.rs`
  - Add individual variants: `BancoInterCsv`, `BancoInterPdf`, `BancoInterOfx`
  - Add individual variants: `BancoBrasilCsv`, `BancoBrasilOfx` (MOCK)
  - Add individual variants: `ItauCsv`, `ItauPdf`
  - Update `all_parsers()` method to return all individual parsers

- [ ] **Create mock parsers for Banco Brasil**
  - Create `BancoBrasilCsvParser` (mock - no samples)
  - Create `BancoBrasilOfxParser` (mock - no samples)
  - Implement `IndividualBankParser` trait with low confidence
  - Add proper error messages for unimplemented parsers

- [ ] **Test integration**
  - Test parser creation and instantiation
  - Test bank detection with all formats
  - Verify mock parsers return appropriate errors

#### Success Criteria:
- [ ] All individual parsers integrated into `BankParser` enum
- [ ] Mock parsers properly implemented
- [ ] Bank detection works correctly for all formats
- [ ] All integration tests pass

#### Dependencies:
- Completed BankDetector implementation
- Existing individual parsers
- Mock parser implementations

### 1.3 Main System Integration
**Agent**: @agent:rusty  
**Estimated Time**: 2-3 hours  
**Priority**: High  

#### Tasks:
- [ ] **Update module exports**
  - Export `BankDetector` from parsers module
  - Update `crates/api/src/parsers/mod.rs`
  - Add BankDetector to module exports

- [ ] **Update BankParser enum**
  - Remove any unified parser variants
  - Ensure all individual parsers are properly integrated
  - Update parser creation methods

- [ ] **Test end-to-end integration**
  - Test file upload → detection → parsing flow
  - Test confidence scoring accuracy
  - Test ambiguous file handling

#### Success Criteria:
- [ ] BankDetector integrated into main system
- [ ] All individual parsers working correctly
- [ ] End-to-end flow functional
- [ ] All integration tests pass

#### Dependencies:
- Completed BankDetector implementation
- Completed individual parser integration
- Existing main system architecture

---

## Phase 2: Testing & Validation (1 day)

### 2.1 Comprehensive Unit Tests
**Agent**: @agent:rusty  
**Estimated Time**: 4-6 hours  
**Priority**: High  

#### Tasks:
- [ ] **Create unit tests for `BankDetector`**
  - Test detection with high confidence files
  - Test detection with ambiguous files
  - Test confidence scoring accuracy
  - Test error handling

- [ ] **Enhance existing tests for individual parsers**
  - Ensure `BancoInterCsvParser`, `BancoInterPdfParser`, `BancoInterOfxParser` have robust tests
  - Cover edge cases, malformed data, and various transaction types
  - Test mock parsers for Banco Brasil

- [ ] **Test confidence scoring accuracy**
  - Verify confidence scores >0.8 for Banco Inter files
  - Test confidence scoring with non-Banco Inter files
  - Test mock parser confidence scores

#### Success Criteria:
- [ ] Comprehensive unit tests for BankDetector
- [ ] High test coverage for all individual parsers
- [ ] Confidence scoring accuracy validated
- [ ] All unit tests pass

#### Dependencies:
- Completed BankDetector implementation
- Existing individual parsers
- Sample files for all formats

### 2.2 Integration and End-to-End Testing
**Agent**: @agent:rusty  
**Estimated Time**: 3-4 hours  
**Priority**: High  

#### Tasks:
- [ ] **Create integration tests for `BankParser`**
  - Test `BankParser::all_parsers()` with various files
  - Verify end-to-end parsing flow
  - Ensure correct `BankInfo` is returned

- [ ] **Test with real-world sample files**
  - Use a diverse set of Banco Inter CSV, PDF, and OFX files
  - Validate transaction extraction accuracy against expected results
  - Test with Itau and Banco Brasil files

- [ ] **Performance and memory usage validation**
  - Benchmark parsing times for large files
  - Monitor memory consumption during parsing
  - Test detection performance with many parsers

#### Success Criteria:
- [ ] Robust integration tests for `BankParser`
- [ ] Successful parsing of all real-world sample files
- [ ] Performance and memory usage within acceptable limits
- [ ] All integration tests pass

#### Dependencies:
- Completed BankDetector and individual parser tests
- Real-world sample files
- Benchmarking tools

---

## Phase 3: Enhanced Architecture Implementation (4 weeks)

### 3.1 Parser Versioning Infrastructure
**Agent**: @agent:rusty  
**Status**: 📋 **PLANNED**  
**Priority**: High (affects all banks, not just Banco Inter)

#### Database Schema Implementation
- [ ] **Create parser_versions table**
  - Implement UUID primary key
  - Add bank_code, format, version fields
  - Add confidence_threshold and sample_file_hashes
  - Add parser_config JSONB field

- [ ] **Create parser_performance table**
  - Track parser performance metrics
  - Monitor confidence scores over time
  - Track parse times and success rates

- [ ] **Create parser_requests table**
  - Lead capture for unsupported banks
  - Track user requests for new parsers
  - Integration with email system

#### Parser Versioning System
- [ ] **Implement ParserVersion struct**
  - Add version management utilities
  - Implement version comparison logic
  - Add changelog tracking

- [ ] **Implement VersionedParser wrapper**
  - Add backward compatibility system
  - Implement fallback to previous versions
  - Add version-specific configuration

### 3.2 UI Integration Components
**Agent**: @agent:uidev  
**Status**: 📋 **PLANNED**  
**Priority**: Medium

#### Confidence Display Component
- [ ] **Show confidence score to users**
  - Visual indicators for detection quality
  - Color-coded confidence levels
  - Parser version information display

#### Manual Bank Selection
- [ ] **Dropdown with supported banks**
  - Search functionality for bank names
  - Bank code display for technical users
  - Confidence score display for each option

#### Lead Capture System
- [ ] **"Implement this for me" button**
  - Email capture form
  - File upload for sample files
  - Integration with parser_requests table
  - User notification system

### 3.3 Implementation Timeline
- **Week 1**: Parser versioning infrastructure
- **Week 2**: Database schema and migrations
- **Week 3**: UI integration components
- **Week 4**: Lead capture and analytics

---

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

---

## Risk Mitigation

### Technical Risks
- **Parser Performance**: Monitor detection times with many parsers
- **Confidence Accuracy**: Validate scoring algorithms with diverse files
- **Mock Parser Behavior**: Ensure mock parsers don't interfere with detection

### User Experience Risks
- **Ambiguous File Handling**: Ensure clear UI for manual selection
- **Error Messages**: Provide helpful error messages for failed detections
- **Performance**: Ensure detection doesn't slow down file upload

### Business Risks
- **Lead Quality**: Ensure captured leads are actionable
- **Parser Maintenance**: Plan for ongoing parser updates
- **Scalability**: Ensure system can handle new banks and formats

---

## Dependencies

### External Dependencies
- All existing individual parsers must be functional
- Sample files for testing and validation
- Database migration tools for versioning system

### Internal Dependencies
- `IndividualBankParser` trait implementation
- `ParseError` and `NormalizedTransaction` types
- Main system architecture compatibility

### Team Dependencies
- **@agent:rusty**: Core implementation and testing
- **@agent:uidev**: UI integration components
- **@agent:scribas**: Git workflow and deployment
---
description: Feature Problem - Prelaunch Redesign
type: feature-problem
priority: high
---

# Problem: Prelaunch Redesign

## Problem Statement

The current ConciliaExtrato prelaunch page uses an email capture approach that creates friction in the user journey and doesn't demonstrate the core product value. Users must provide their email and wait for launch instead of experiencing immediate value through file processing demonstration.

## User Impact

### Current User Experience Issues
- **Friction in Conversion**: Users must provide email before seeing product value
- **Delayed Value Realization**: No immediate demonstration of core capabilities
- **Unclear Value Proposition**: Users don't understand what the product actually does
- **Low Engagement**: Email capture doesn't engage users with the actual product
- **Trust Issues**: Users are skeptical without seeing product functionality

### User Journey Problems
1. **Landing Page**: Generic messaging about bank statement conversion
2. **Email Capture**: Forced email submission without value demonstration
3. **Wait Period**: Users must wait for launch to see product functionality
4. **Abandonment**: High bounce rate due to lack of immediate value

## Business Impact

### Conversion Problems
- **Low Conversion Rate**: Email capture approach has poor conversion rates
- **Lead Quality Issues**: Captured leads haven't experienced product value
- **Customer Acquisition Cost**: High CAC due to low conversion efficiency
- **Market Validation Delay**: Can't validate product-market fit without user interaction

### Growth Limitations
- **Limited User Feedback**: No real user interaction data for product improvement
- **Weak Product Positioning**: Can't demonstrate competitive advantages
- **Reduced Market Penetration**: Friction prevents broader market reach
- **Customer Education Gap**: Users don't understand product capabilities

## Current State

### Existing Implementation
- **Email Capture Form**: HTMX-powered form with email validation
- **Generic Landing Page**: Basic messaging about bank statement conversion
- **Separate Demo Page**: File processing functionality exists but is hidden
- **No Value Demonstration**: Users can't see product capabilities upfront

### Technical Infrastructure
- **File Upload Service**: `FileUploadService` in `crates/api/src/services/file_upload.rs`
- **Session Management**: Anonymous session creation capability
- **Processing Pipeline**: Bank statement parsing and analysis working
- **Download Endpoints**: CSV/JSON file generation functional

## Desired State

### Immediate Value Demonstration
- **Direct File Processing**: Users can upload and process files immediately
- **Real-time Feedback**: Live processing status and bank detection
- **Instant Results**: Immediate download of processed files
- **Value Realization**: Users understand product capabilities through experience

### Improved User Journey
1. **Landing Page**: Clear value proposition with immediate action
2. **File Upload**: Prominent drag & drop interface
3. **Processing**: Real-time status updates and bank detection
4. **Results**: Financial summary and download options
5. **Conversion**: Natural progression to user registration

### Business Benefits
- **Higher Conversion**: Direct value demonstration increases conversion rates
- **Better Lead Quality**: Users who process files are more qualified leads
- **Market Validation**: Real user interaction validates product-market fit
- **Competitive Advantage**: Immediate value demonstration differentiates from competitors

## Constraints

### Technical Constraints
- **File Size Limits**: 50MB maximum file size for processing
- **Format Support**: Limited to CSV and PDF files initially
- **Processing Time**: Must maintain sub-30-second processing target
- **Session Management**: Anonymous sessions for non-registered users

### Business Constraints
- **Resource Limitations**: Limited development resources for implementation
- **Timeline Pressure**: Need to launch quickly to capture market opportunity
- **Quality Standards**: Must maintain high processing accuracy (>99%)
- **Security Requirements**: Must handle sensitive financial data securely

### User Constraints
- **Technical Literacy**: Users may have varying technical capabilities
- **File Format Knowledge**: Users may not know which formats are supported
- **Trust Requirements**: Users need to trust the system with financial data
- **Mobile Usage**: Must work effectively on mobile devices

## Success Criteria

### User Experience Success
- **Immediate Value**: Users can process files within 30 seconds of landing
- **Clear Understanding**: Users understand product capabilities through experience
- **Smooth Journey**: Seamless flow from landing to file processing to results
- **High Engagement**: Users spend time interacting with the product

### Business Success
- **Higher Conversion**: Significant increase in conversion rates
- **Better Lead Quality**: Leads who have experienced product value
- **Market Validation**: Real user data validates product-market fit
- **Competitive Positioning**: Clear differentiation through value demonstration

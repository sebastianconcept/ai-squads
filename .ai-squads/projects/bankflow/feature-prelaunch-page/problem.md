---
description: Feature Problem - Pre-launch Page
type: feature-problem
priority: high
---

# Problem: Pre-launch Page

## Problem Statement
We need to create a pre-launch page that serves as the main landing page for BankFlow, replacing the current product-focused landing page. This pre-launch page should capture leads (email addresses) for marketing communications while moving the current product pages to appropriate sub-routes.

## User Impact
- **New visitors** will see a pre-launch page that builds anticipation and captures their interest
- **Existing users** will still have access to the product demo and features through the new `/product` route
- **Marketing team** will have a dedicated lead capture mechanism for pre-launch campaigns

## Business Impact
- **Lead Generation**: Capture email addresses for marketing communications and launch notifications
- **Brand Positioning**: Create anticipation and excitement for the upcoming launch
- **User Journey**: Maintain access to product features while building a pre-launch audience
- **Marketing ROI**: Enable targeted marketing campaigns to captured leads

## Current State
- Main route (`/`) serves a product-focused landing page with features and demo access
- Demo page is accessible at `/demo` with full functionality
- No lead capture mechanism exists
- All content is product-ready rather than pre-launch focused

## Desired State
- Main route (`/`) serves a pre-launch page with lead capture
- Current landing page content moved to `/product`
- Demo page moved to `/product/demo`
- Pre-launch page includes email capture form for marketing leads
- Maintains professional design and user experience

## Constraints
- Must maintain existing demo functionality
- Should preserve current design aesthetic and branding
- Email capture must be secure and compliant
- Routing changes must not break existing functionality
- Should be easily reversible if needed

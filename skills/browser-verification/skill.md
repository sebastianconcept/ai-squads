---
name: browser-verification
description: "Verify frontend changes work correctly in a browser. Use when implementing frontend user stories or when acceptance criteria require browser verification. Triggers on: verify in browser, check browser, test in browser, visual verification, frontend verification."
---

# Browser Verification Skill

Verify that frontend changes work correctly in a browser by navigating to pages, interacting with UI elements, and confirming expected behavior.

---

## The Job

When a user story has `type: "frontend"` or includes "Verify in browser" in acceptance criteria, you must visually verify the changes work as expected.

---

## When to Use

- **Frontend user stories**: Any story with `type: "frontend"` in prd.json
- **Acceptance criteria**: When acceptance criteria explicitly requires "Verify in browser"
- **UI changes**: After implementing any UI component, page, or interaction
- **Before committing**: Frontend stories are NOT complete until browser verification passes

---

## Verification Steps

### 1. Start Development Server (if needed)

If the project requires a dev server:
```bash
# Check if server is running
# Start if needed (e.g., npm run dev, cargo run, etc.)
```

### 2. Navigate to Relevant Page

- Open the page/route where changes were made
- Use the correct URL (localhost, dev server, etc.)
- Ensure the page loads without errors

### 3. Verify UI Changes

Check that the implemented changes work as expected:
- **Visual elements**: Buttons, forms, components render correctly
- **Interactions**: Clicking, typing, submitting work as expected
- **Styling**: CSS/Tailwind styles apply correctly
- **Responsiveness**: Layout works on different screen sizes (if applicable)
- **Accessibility**: Keyboard navigation, screen reader compatibility (if applicable)

### 4. Test User Flows

If the story involves user interactions:
- Complete the full user flow described in the story
- Verify all steps work end-to-end
- Check error states and edge cases (if applicable)

### 5. Take Screenshot (Optional but Recommended)

Capture a screenshot of the verified UI:
- Save to project notes or feature directory
- Include in progress log for reference
- Helps document what was verified

### 6. Document Verification

Append to project notes (`~/docs/{project-name}/NOTES.md`):
```
## [Date/Time] - [Story ID] - Browser Verification
- Verified: [What was checked]
- Page: [URL or route]
- Status: ✅ Pass / ❌ Fail
- Notes: [Any issues found or observations]
- Screenshot: [Path if taken]
```

---

## Verification Checklist

Before marking a frontend story as complete, verify:

- [ ] Page/component loads without errors
- [ ] Visual elements render correctly
- [ ] User interactions work as expected
- [ ] Styling matches design/requirements
- [ ] No console errors (check browser console)
- [ ] Responsive design works (if applicable)
- [ ] Accessibility features work (if applicable)
- [ ] Screenshot captured (recommended)
- [ ] Verification documented in project notes

---

## Common Issues to Check

### Visual Issues
- Elements not rendering
- Incorrect styling or layout
- Missing images or assets
- Broken CSS/Tailwind classes

### Functional Issues
- Buttons/links not working
- Forms not submitting
- JavaScript errors in console
- API calls failing
- State not updating correctly

### Browser Compatibility
- Works in target browser(s)
- No browser-specific errors
- Responsive breakpoints work

---

## Manual Verification (Fallback)

If automated browser tools are not available:

1. **Start dev server** (if needed)
2. **Open browser** manually
3. **Navigate** to the page
4. **Interact** with the UI
5. **Verify** expected behavior
6. **Document** results in project notes
7. **Take screenshot** if possible

---

## Integration with Execution Loop

When executing frontend stories:

1. Agent implements the story
2. Quality checks pass (typecheck, lint, format, test)
3. **Browser verification runs** (this skill)
4. If verification passes: commit and mark story as complete
5. If verification fails: log issue, continue to next iteration

**Important**: A frontend story is NOT complete until browser verification passes.

---

## Example Usage

**Story**: "Add login form UI"
**Type**: frontend
**Acceptance Criteria**: 
- Login form with email and password fields
- Form validation works
- Verify in browser using browser-verification skill

**Verification Process**:
1. Navigate to `/login` page
2. Verify form fields render correctly
3. Test form validation (empty fields, invalid email)
4. Test form submission
5. Take screenshot of login form
6. Document verification in project notes
7. Mark story as complete if all checks pass

---

## Notes

- Browser verification is **required** for frontend stories
- Always check browser console for errors
- Document any issues found during verification
- Screenshots help with debugging and documentation
- If verification fails, update story notes with specific issues

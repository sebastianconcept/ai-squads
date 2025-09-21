---
description: AJAX Lead Capture Implementation Guide
type: technical-implementation
status: planned
---

# AJAX Lead Capture Implementation Guide

## Overview
This document provides detailed implementation guidance for the AJAX-enhanced lead capture system with loading states, animations, and compelling copy.

## Technical Requirements

### Frontend Implementation

#### 1. HTML Structure
```html
<div class="lead-capture-section">
    <div class="lead-capture-form" id="leadForm">
        <h2>Use confortavelmente os extratos de todas suas contas bancárias desde um só lugar</h2>
        <p class="mystery-text">Estamos trabalhando para tornar isso realidade para todos os bancos.</p>
        <p class="cta-text">Seja o primeiro a saber quando estiver disponível.</p>
        
        <form id="emailForm" class="email-form">
            <div class="form-group">
                <input type="email" id="emailInput" placeholder="Seu email" required>
                <input type="text" id="nameInput" placeholder="Seu nome (opcional)">
            </div>
            <button type="submit" id="submitBtn" class="submit-btn">
                <span class="btn-text">Quero ser o primeiro</span>
                <span class="spinner" style="display: none;">⏳</span>
            </button>
        </form>
        
        <div class="success-message" id="successMessage" style="display: none;">
            <div class="success-icon">✅</div>
            <h3>Obrigado!</h3>
            <p>Você será o primeiro a saber quando estiver disponível.</p>
        </div>
        
        <div class="error-message" id="errorMessage" style="display: none;">
            <div class="error-icon">❌</div>
            <h3>Ops! Algo deu errado</h3>
            <p id="errorText">Tente novamente em alguns instantes.</p>
            <button class="retry-btn" onclick="retrySubmission()">Tentar novamente</button>
        </div>
    </div>
</div>
```

#### 2. CSS Styling
```css
.lead-capture-section {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    padding: 80px 20px;
    text-align: center;
    color: white;
}

.lead-capture-form h2 {
    font-size: 2.5rem;
    margin-bottom: 20px;
    font-weight: 700;
    text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
}

.mystery-text {
    font-size: 1.2rem;
    margin-bottom: 10px;
    opacity: 0.9;
}

.cta-text {
    font-size: 1.1rem;
    margin-bottom: 40px;
    font-weight: 600;
}

.email-form {
    max-width: 400px;
    margin: 0 auto;
}

.form-group {
    display: flex;
    gap: 15px;
    margin-bottom: 20px;
}

.form-group input {
    flex: 1;
    padding: 15px;
    border: none;
    border-radius: 8px;
    font-size: 1rem;
    background: rgba(255, 255, 255, 0.9);
}

.submit-btn {
    width: 100%;
    padding: 15px 30px;
    background: #10b981;
    color: white;
    border: none;
    border-radius: 8px;
    font-size: 1.1rem;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.3s ease;
    position: relative;
}

.submit-btn:hover {
    background: #059669;
    transform: translateY(-2px);
}

.submit-btn:disabled {
    background: #6b7280;
    cursor: not-allowed;
    transform: none;
}

.spinner {
    animation: spin 1s linear infinite;
}

@keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
}

.success-message {
    background: rgba(16, 185, 129, 0.1);
    border: 2px solid #10b981;
    border-radius: 12px;
    padding: 30px;
    margin-top: 20px;
    animation: slideIn 0.5s ease-out;
}

.success-icon {
    font-size: 3rem;
    margin-bottom: 15px;
    animation: celebrate 0.6s ease-out;
}

@keyframes celebrate {
    0% { transform: scale(0); opacity: 0; }
    50% { transform: scale(1.2); opacity: 1; }
    100% { transform: scale(1); opacity: 1; }
}

@keyframes slideIn {
    from { opacity: 0; transform: translateY(20px); }
    to { opacity: 1; transform: translateY(0); }
}

.error-message {
    background: rgba(239, 68, 68, 0.1);
    border: 2px solid #ef4444;
    border-radius: 12px;
    padding: 30px;
    margin-top: 20px;
    animation: slideIn 0.5s ease-out;
}

.retry-btn {
    background: #ef4444;
    color: white;
    border: none;
    padding: 10px 20px;
    border-radius: 6px;
    cursor: pointer;
    margin-top: 15px;
    transition: background 0.3s ease;
}

.retry-btn:hover {
    background: #dc2626;
}

@media (max-width: 768px) {
    .lead-capture-form h2 {
        font-size: 2rem;
    }
    
    .form-group {
        flex-direction: column;
    }
}
```

#### 3. JavaScript Implementation
```javascript
document.addEventListener('DOMContentLoaded', function() {
    const form = document.getElementById('emailForm');
    const emailInput = document.getElementById('emailInput');
    const nameInput = document.getElementById('nameInput');
    const submitBtn = document.getElementById('submitBtn');
    const btnText = document.querySelector('.btn-text');
    const spinner = document.querySelector('.spinner');
    const successMessage = document.getElementById('successMessage');
    const errorMessage = document.getElementById('errorMessage');
    const errorText = document.getElementById('errorText');

    form.addEventListener('submit', async function(e) {
        e.preventDefault();
        
        const email = emailInput.value.trim();
        const name = nameInput.value.trim();
        
        // Validate email
        if (!isValidEmail(email)) {
            showError('Por favor, insira um email válido.');
            return;
        }
        
        // Show loading state
        setLoadingState(true);
        
        try {
            const response = await fetch('/api/leads', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    email: email,
                    name: name || null
                })
            });
            
            if (response.ok) {
                const result = await response.json();
                showSuccess();
            } else {
                const error = await response.json();
                showError(error.message || 'Erro no servidor. Tente novamente.');
            }
        } catch (error) {
            console.error('Submission error:', error);
            showError('Erro de conexão. Verifique sua internet e tente novamente.');
        } finally {
            setLoadingState(false);
        }
    });
    
    function setLoadingState(loading) {
        if (loading) {
            submitBtn.disabled = true;
            btnText.style.display = 'none';
            spinner.style.display = 'inline-block';
            errorMessage.style.display = 'none';
        } else {
            submitBtn.disabled = false;
            btnText.style.display = 'inline-block';
            spinner.style.display = 'none';
        }
    }
    
    function showSuccess() {
        form.style.display = 'none';
        successMessage.style.display = 'block';
        
        // Track successful signup
        if (typeof gtag !== 'undefined') {
            gtag('event', 'lead_capture_success', {
                'event_category': 'engagement',
                'event_label': 'prelaunch_signup'
            });
        }
    }
    
    function showError(message) {
        errorText.textContent = message;
        errorMessage.style.display = 'block';
        
        // Track error
        if (typeof gtag !== 'undefined') {
            gtag('event', 'lead_capture_error', {
                'event_category': 'error',
                'event_label': 'prelaunch_signup_error'
            });
        }
    }
    
    function isValidEmail(email) {
        const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
        return emailRegex.test(email);
    }
    
    // Retry function
    window.retrySubmission = function() {
        errorMessage.style.display = 'none';
        form.style.display = 'block';
        emailInput.focus();
    };
});
```

### Backend Implementation

#### 1. Lead Data Structure
```rust
#[derive(Debug, Deserialize)]
pub struct LeadCaptureForm {
    pub email: String,
    pub name: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Lead {
    pub email: String,
    pub name: Option<String>,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub source: String, // "prelaunch_page"
    pub country: Option<String>, // Detect from IP or user agent
}
```

#### 2. API Endpoint Handler
```rust
pub async fn handle_lead_capture(
    State(_state): State<std::sync::Arc<AppState>>,
    Json(form): Json<LeadCaptureForm>,
) -> Result<Json<serde_json::Value>, StatusCode> {
    debug!("📧 Lead capture request: {:?}", form);
    
    // Validate email format
    if !is_valid_email(&form.email) {
        return Err(StatusCode::BAD_REQUEST);
    }
    
    // Check for duplicates
    if is_duplicate_email(&form.email).await {
        return Ok(Json(json!({
            "success": true,
            "message": "Email já cadastrado. Você será notificado quando estiver disponível."
        })));
    }
    
    // Create lead record
    let lead = Lead {
        email: form.email.clone(),
        name: form.name,
        created_at: chrono::Utc::now(),
        source: "prelaunch_page".to_string(),
        country: None, // TODO: Detect from IP
    };
    
    // Store lead
    match store_lead(lead).await {
        Ok(_) => {
            debug!("✅ Lead stored successfully: {}", form.email);
            Ok(Json(json!({
                "success": true,
                "message": "Obrigado! Você será o primeiro a saber quando estiver disponível."
            })))
        }
        Err(e) => {
            error!("❌ Failed to store lead: {}", e);
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

fn is_valid_email(email: &str) -> bool {
    let email_regex = regex::Regex::new(r"^[^\s@]+@[^\s@]+\.[^\s@]+$").unwrap();
    email_regex.is_match(email)
}

async fn is_duplicate_email(email: &str) -> bool {
    // TODO: Implement duplicate check
    false
}

async fn store_lead(lead: Lead) -> Result<(), Box<dyn std::error::Error>> {
    // TODO: Implement lead storage
    debug!("📝 Storing lead: {:?}", lead);
    Ok(())
}
```

## User Experience Flow

### 1. Initial State
- Clean, minimal form with compelling copy
- Professional banking/finance illustration
- Clear value proposition and mystery element

### 2. Loading State
- Elegant spinner animation
- "Enviando..." text feedback
- Disabled form to prevent double submission

### 3. Success State
- Celebratory checkmark animation
- "Obrigado!" confirmation message
- Clear next steps communication

### 4. Error State
- Clear error message
- Retry button for easy recovery
- Helpful error descriptions

## Analytics and Tracking

### Google Analytics Events
```javascript
// Track form interactions
gtag('event', 'lead_capture_start', {
    'event_category': 'engagement',
    'event_label': 'prelaunch_signup'
});

// Track successful signups
gtag('event', 'lead_capture_success', {
    'event_category': 'conversion',
    'event_label': 'prelaunch_signup'
});

// Track errors
gtag('event', 'lead_capture_error', {
    'event_category': 'error',
    'event_label': 'prelaunch_signup_error'
});
```

### Lead Quality Metrics
- **Conversion Rate**: % of visitors who sign up
- **Email Quality**: % of valid email addresses
- **Geographic Distribution**: Brazil vs Portugal signups
- **Error Rate**: % of failed submissions
- **Retry Rate**: % of users who retry after error

## Testing Requirements

### Functional Testing
- [ ] Form submission works with valid email
- [ ] Form validation rejects invalid emails
- [ ] Loading states display correctly
- [ ] Success animation plays on successful submission
- [ ] Error handling works for various error scenarios
- [ ] Retry functionality works correctly

### User Experience Testing
- [ ] Form is intuitive and easy to use
- [ ] Loading states provide clear feedback
- [ ] Success message is celebratory and clear
- [ ] Error messages are helpful and actionable
- [ ] Mobile experience is smooth and responsive

### Performance Testing
- [ ] Form submission is fast (< 2 seconds)
- [ ] Animations are smooth and don't lag
- [ ] Page load time remains under 2 seconds
- [ ] AJAX requests don't block UI

## Security Considerations

### Input Validation
- **Client-side**: Basic email format validation
- **Server-side**: Comprehensive email validation and sanitization
- **Rate limiting**: Prevent spam submissions
- **CSRF protection**: Secure form submissions

### Data Protection
- **Privacy compliance**: LGPD/GDPR considerations
- **Data storage**: Secure lead storage mechanism
- **Data access**: Controlled access to lead data
- **Data retention**: Clear data retention policies

## Future Enhancements

### Phase 2 Features
- **Email verification**: Double opt-in for lead validation
- **Segmentation**: Different messaging for Brazil vs Portugal
- **A/B testing**: Test different copy and designs
- **Integration**: Connect with email marketing tools

### Phase 3 Features
- **Personalization**: Customized experience based on location
- **Social sharing**: Referral incentives for sharing
- **Progress updates**: Regular updates on development progress
- **Early access**: Exclusive features for early signups

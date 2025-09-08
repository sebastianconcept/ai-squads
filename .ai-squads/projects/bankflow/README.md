---
description: BankFlow Project Overview - Complete project documentation and getting started guide
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# BankFlow Project

> Last Updated: December 2024
> Version: 1.0

## 🎯 Project Overview

BankFlow is a microSaaS that transforms Brazilian bank statement normalization from a manual, time-consuming process into an automatic, instant solution. We liberate accountants to focus on strategic consulting by automating the tedious work of converting bank statements from different formats into standardized data.

## 🚀 Quick Start

### For Developers
```bash
# Clone the repository
git clone https://github.com/your-org/bankflow.git
cd bankflow

# Set up development environment
docker-compose up -d

# Run migrations
cargo run --bin migrate

# Start development server
cargo run --dev
```

### For Product Team
- **Mission**: See [mission.md](mission.md) for project vision and strategy
- **Roadmap**: See [roadmap.md](roadmap.md) for development phases
- **Tech Stack**: See [tech-stack.md](tech-stack.md) for technical architecture
- **Decisions**: See [decisions.md](decisions.md) for architectural decisions

## 📋 Project Status

**Current Phase**: Phase 1 - MVP & Validation (Weeks 1-4)
**Status**: 🚀 ACTIVE - Week 1 of 4
**Next Milestone**: Complete demo flow validation with 5 major Brazilian banks

### Recent Achievements
- ✅ Project documentation structure created
- ✅ Technical architecture defined
- ✅ Development roadmap established
- ✅ Team workflows and templates prepared
- ✅ Phase 1 strategic roadmap created
- ✅ Team coordination and feature planning prepared

### Current Focus
- 🔄 Development environment setup
- 🔄 Database schema design
- 🔄 Authentication system implementation
- 🔄 File upload service development

### Demo Flow Validation
**Core Demo Flow**: Landing Page → File Upload → Real-time Status → Background Processing → Result Download
**Success Criteria**: <30 second processing, >99% accuracy, >80% demo completion rate

## 👥 Team & Squad

**Active Squad**: Sulaco Squad - Micro SaaS Development
**Squad Focus**: Customer validation, product-market fit, growth marketing, customer success

### Squad Members
- **@agent:steve** - Project coordination and strategic oversight
- **@agent:moesta** - JTBD and customer jobs analysis, solution validation
- **@agent:godin** - Content marketing, brand storytelling, growth content
- **@agent:scribas** - Version control, workflow management, deployment
- **@agent:rusty** - Full-stack development, DevOps, SaaS infrastructure
- **@agent:team** - Team coordination, customer success, quality assurance
- **@agent:uxe** - User experience research, design, conversion optimization
- **@agent:uidev** - Frontend implementation, growth optimization, A/B testing
- **@agent:guy** - Strategic product planning, business model design, SaaS metrics

## 🏗️ Architecture Overview

### Tech Stack
- **Frontend**: HTMX + Alpine.js + Tailwind CSS
- **Backend**: Rust + Axum + Tokio
- **Database**: PostgreSQL + Redis
- **Storage**: Local file storage with automatic cleanup
- **Infrastructure**: Digital Ocean Droplet + Docker Compose

### Key Components
- **Parser Engine**: Bank-specific parsing with automatic detection
- **Processing Queue**: Asynchronous job processing with Redis
- **File Storage**: Local temporary storage with automatic cleanup
- **Real-time Updates**: Server-sent events for status updates
- **Authentication**: JWT-based auth with Argon2 password hashing

## 📊 Success Metrics

### Phase 1 Targets (MVP)
- **Trial Users**: 100 users
- **Conversion Rate**: 40% trial-to-paid
- **NPS Score**: >50
- **Processing Time**: <30 seconds per file
- **Accuracy**: >99% parsing accuracy

### Business Metrics
- **MRR Target**: R$ 5k by Month 3
- **Churn Rate**: <5% monthly
- **CAC Payback**: <6 months
- **LTV/CAC**: >3:1

## 🎯 Key Features

### MVP Features (Phase 1)
- [ ] **5 Major Banks**: Itaú, Bradesco, BB, Santander, Caixa
- [ ] **File Upload**: Drag & drop interface with progress tracking
- [ ] **Real-time Processing**: Live status updates via SSE
- [ ] **Standardized Output**: CSV/JSON format conversion
- [ ] **User Dashboard**: Job management and history

### Future Features (Phase 2+)
- [ ] **Extended Bank Support**: 15+ Brazilian banks
- [ ] **API Integration**: RESTful API for ERP integration
- [ ] **Smart Categorization**: ML-powered transaction categorization
- [ ] **Team Collaboration**: Multi-user accounts and role management
- [ ] **Advanced Analytics**: Business intelligence and insights

## 🔒 Security & Compliance

### Data Protection
- **Encryption**: AES-256-GCM for file storage
- **Authentication**: JWT with Argon2 password hashing
- **Transport**: TLS 1.3 for all communications
- **Compliance**: LGPD compliance for Brazilian market

### Security Features
- **Audit Logging**: Comprehensive activity tracking
- **Access Control**: Role-based permissions
- **Data Retention**: Automated cleanup policies
- **Backup Strategy**: Multi-region disaster recovery

## 📈 Growth Strategy

### Customer Acquisition
- **Content Marketing**: Educational content about automation benefits
- **Word-of-Mouth**: Referral program leveraging satisfied customers
- **Partnership Channel**: Integration with popular accounting ERPs
- **SEO Strategy**: Target keywords like "converter extrato bancário"

### Market Positioning
- **Primary Value Prop**: 80% time savings with 99% accuracy
- **Target Market**: Brazilian accounting offices (70k+ offices)
- **Competitive Advantage**: Brazilian bank specialization + instant processing
- **Pricing Strategy**: Freemium model with usage-based tiers

## 🛠️ Development Workflow

### Getting Started
1. **Read Documentation**: Start with [mission.md](mission.md) and [roadmap.md](roadmap.md)
2. **Set Up Environment**: Follow [tech-stack.md](tech-stack.md) setup guide
3. **Review Decisions**: Understand [decisions.md](decisions.md) architectural choices
4. **Join Squad**: Connect with Sulaco squad members for collaboration

### Development Process
- **Git Flow**: Feature branches with quality gates
- **Testing**: Comprehensive unit and integration tests
- **CI/CD**: Automated testing and deployment
- **Code Review**: Peer review for all changes
- **Documentation**: Keep docs updated with changes

### Quality Standards
- **Performance**: <30 second processing, <200ms API response
- **Reliability**: 99.9% uptime target
- **Security**: Industry-standard security practices
- **Accessibility**: WCAG 2.1 AA compliance
- **Testing**: >80% code coverage

## 📚 Documentation Structure

### Core Documentation
- **[mission.md](mission.md)** - Project vision, strategy, and user personas
- **[roadmap.md](roadmap.md)** - Development phases and feature roadmap
- **[tech-stack.md](tech-stack.md)** - Technical architecture and technology choices
- **[decisions.md](decisions.md)** - Architectural decisions and rationale

### Additional Resources
- **`docs/briefing/`** - Original project briefings and specifications
- **`docs/development/`** - Development setup and guidelines
- **`docs/api/`** - API documentation and specifications
- **`docs/deployment/`** - Deployment guides and procedures

## 🤝 Contributing

### For Squad Members
1. **Review Mission**: Understand project goals and user needs
2. **Check Roadmap**: See current phase priorities and tasks
3. **Follow Workflows**: Use Sulaco squad workflows and templates
4. **Coordinate**: Work with other squad members for handoffs

### For External Contributors
1. **Read Documentation**: Start with this README and mission
2. **Set Up Environment**: Follow development setup guide
3. **Review Decisions**: Understand architectural choices
4. **Submit PR**: Follow contribution guidelines

## 📞 Support & Contact

### Squad Coordination
- **Director**: @agent:steve for project coordination
- **Technical**: @agent:rusty for development questions
- **UX/UI**: @agent:uxe and @agent:uidev for design
- **Product**: @agent:guy for business strategy
- **Growth**: @agent:godin for content and marketing

### Project Resources
- **Repository**: [GitHub Repository](https://github.com/your-org/bankflow)
- **Documentation**: [Project Docs](docs/)
- **Issues**: [GitHub Issues](https://github.com/your-org/bankflow/issues)
- **Discussions**: [GitHub Discussions](https://github.com/your-org/bankflow/discussions)

## 🎯 Next Steps

### Immediate Actions (This Week)
1. **Set Up Development Environment**: Docker, database, and local setup
2. **Create Core Models**: User, Job, and Transaction data models
3. **Implement Basic Auth**: JWT authentication with Argon2
4. **Build File Upload**: Drag & drop interface with validation
5. **Start Parser Development**: Begin with Itaú bank parser

### This Month
1. **Complete MVP**: All Phase 1 features implemented
2. **User Testing**: Beta testing with 10 accounting offices
3. **Performance Optimization**: Sub-30-second processing
4. **Security Review**: Comprehensive security audit
5. **Deployment**: Production deployment and monitoring

---

**Remember**: BankFlow is not just a tool - it's the first step in transforming how Brazilian accountants work with financial data. Every decision should trace back to customer success and time savings.

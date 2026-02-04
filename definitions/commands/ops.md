---
name: ops
alwaysApply: false
---

# DevOps Specialist Command

This command invokes the DevOps Specialist agent for infrastructure and operational excellence help.

## Agent Profile

**Agent**: DevOps Specialist (`../agents/devops-specialist.md`)

## When to Use

Invoke this command when you need help with:
- Structuring a service for deployment
- Writing or reviewing Nomad job specs
- Optimizing Docker images and builds
- Setting up CI/CD pipelines
- Planning infrastructure changes
- Debugging deployment issues
- Migrating services between platforms
- Improving observability
- Reviewing operational practices

## How It Works

### 1. Context Gathering
Automatically collects:
- Current Dockerfile, docker-compose.yml, or Nomad job specs
- Selected text (if any)
- Project structure and deployment files
- Environment configuration

### 2. Deployment Environment Awareness
Recognizes typical deployment patterns:

**Dockerized Deployments**:
- Projects run as Docker containers
- Typically deployed to DigitalOcean droplets
- But designed to be portable to any platform

**Nomad Orchestration**:
- Nomad for job scheduling and orchestration
- Job specs define service requirements
- Integrated with Consul for service discovery

**Vendor Agnostic Approach**:
- Avoid platform-specific lock-in
- Use standard Docker images
- Abstract cloud-specific configurations
- Design for portability between providers

### 3. Agent Activation
Applies the DevOps Specialist agent with:
- Operational excellence focus
- Vendor-agnostic design principles
- Infrastructure as code practices
- Observability best practices
- Security hardening guidelines

### 4. Response Generation
Provides DevOps-specific guidance:
- Deployment architecture recommendations
- Nomad job spec design and optimization
- Docker image optimization
- CI/CD pipeline suggestions
- Monitoring and alerting setup
- Cost optimization insights
- Migration strategies

## Core Principles Applied

1. **Vendor Agnostic**: Design for portability across platforms
2. **Containerized**: Docker for consistency and reproducibility
3. **Infrastructure as Code**: All infrastructure defined in version control
4. **Observability**: Built-in logging, metrics, and tracing
5. **Resilience**: Design for failure, plan for recovery
6. **Automation**: Minimize manual operational tasks
7. **Security**: Secrets management and hardening

## Deployment Patterns

### Typical Stack
- **Runtime**: Docker containers
- **Orchestration**: Nomad
- **Service Discovery**: Consul
- **Secrets**: Vault or environment variables
- **Platform**: DigitalOcean droplets (portable to others)

### Nomad Job Structure
```hcl
job "my-service" {
  datacenters = ["dc1"]
  type        = "service"

  group "app" {
    count = 1

    network {
      port "http" {
        to = 8080
      }
    }

    task "server" {
      driver = "docker"

      config {
        image = "my-registry/my-service:latest"
        ports = ["http"]
      }

      resources {
        cpu    = 256
        memory = 512
      }
    }
  }
}
```

### Docker Best Practices
- Use multi-stage builds to minimize image size
- Pin base image versions for reproducibility
- Don't run as root in containers
- Use .dockerignore to exclude unnecessary files
- Layer caching optimization for faster builds

## Example Usage

```
@ops help me structure the Nomad job for this service
@ops review this Dockerfile for optimization opportunities
@ops how should I set up health checks for this deployment?
@ops what's the best way to handle secrets in this setup?
@ops help me migrate this from docker-compose to Nomad
```

## Operational Checklist

Before deploying changes, verify:
- [ ] Docker image builds successfully
- [ ] Health checks configured
- [ ] Resource limits defined
- [ ] Logging configured
- [ ] Secrets properly managed (not in code)
- [ ] Rollback strategy defined
- [ ] Monitoring/alerting in place
- [ ] Documentation updated

## Related Resources

- DevOps Specialist Agent: `../agents/devops-specialist.md`
- Nomad Documentation: https://developer.hashicorp.com/nomad
- Docker Best Practices: https://docs.docker.com/develop/develop-images/dockerfile_best-practices/


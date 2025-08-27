---
description: API Documentation - API reference and specifications
globs:
alwaysApply: false
version: 1.0
encoding: UTF-8
---

# API Documentation

> Last Updated: [DATE]
> Version: [VERSION]

## API Overview

This document provides comprehensive API documentation for the [PROJECT_NAME] project.

## Base URL

- **Development**: `[DEV_BASE_URL]`
- **Staging**: `[STAGING_BASE_URL]`
- **Production**: `[PRODUCTION_BASE_URL]`

## Authentication

### Authentication Method
[Description of authentication method - OAuth, API Key, JWT, etc.]

### Getting Credentials
1. [Step 1 for obtaining credentials]
2. [Step 2 for obtaining credentials]
3. [Step 3 for obtaining credentials]

### Example Request
```bash
curl -H "Authorization: Bearer [TOKEN]" \
     [ENDPOINT_URL]
```

## API Endpoints

### [Resource 1]

#### GET /[resource]
Retrieves a list of [resource] items.

**Parameters:**
- `page` (optional): Page number for pagination
- `limit` (optional): Number of items per page
- `sort` (optional): Sort field
- `order` (optional): Sort order (asc/desc)

**Response:**
```json
{
  "data": [
    {
      "id": "[ID]",
      "name": "[NAME]",
      "description": "[DESCRIPTION]",
      "created_at": "[TIMESTAMP]"
    }
  ],
  "pagination": {
    "page": 1,
    "limit": 10,
    "total": 100,
    "pages": 10
  }
}
```

#### POST /[resource]
Creates a new [resource] item.

**Request Body:**
```json
{
  "name": "[NAME]",
  "description": "[DESCRIPTION]",
  "metadata": "[METADATA]"
}
```

**Response:**
```json
{
  "id": "[ID]",
  "name": "[NAME]",
  "description": "[DESCRIPTION]",
  "created_at": "[TIMESTAMP]"
}
```

### [Resource 2]

#### GET /[resource]/[id]
Retrieves a specific [resource] item by ID.

**Parameters:**
- `id`: The unique identifier of the [resource]

**Response:**
```json
{
  "id": "[ID]",
  "name": "[NAME]",
  "description": "[DESCRIPTION]",
  "created_at": "[TIMESTAMP]",
  "updated_at": "[TIMESTAMP]"
}
```

## Error Handling

### Error Response Format
```json
{
  "error": {
    "code": "[ERROR_CODE]",
    "message": "[ERROR_MESSAGE]",
    "details": "[ADDITIONAL_DETAILS]"
  }
}
```

### Common Error Codes
- `400` - Bad Request: Invalid parameters or request body
- `401` - Unauthorized: Invalid or missing authentication
- `403` - Forbidden: Insufficient permissions
- `404` - Not Found: Resource not found
- `429` - Too Many Requests: Rate limit exceeded
- `500` - Internal Server Error: Server-side error

## Rate Limiting

- **Rate Limit**: [X] requests per [time period]
- **Headers**: Rate limit information is included in response headers
- **Exceeded**: When rate limit is exceeded, requests return 429 status

## Pagination

All list endpoints support pagination with the following parameters:
- `page`: Page number (default: 1)
- `limit`: Items per page (default: 10, max: 100)

## Filtering

Supported filter operators:
- `eq`: Equal to
- `ne`: Not equal to
- `gt`: Greater than
- `gte`: Greater than or equal to
- `lt`: Less than
- `lte`: Less than or equal to
- `contains`: Contains substring
- `in`: Value in array

## Sorting

Supported sort fields:
- `created_at`: Creation timestamp
- `updated_at`: Last update timestamp
- `name`: Resource name
- `id`: Resource identifier

## SDKs and Libraries

### Official SDKs
- **[Language 1]**: [Repository URL] - [Description]
- **[Language 2]**: [Repository URL] - [Description]

### Community Libraries
- **[Library 1]**: [Repository URL] - [Description]
- **[Library 2]**: [Repository URL] - [Description]

## Examples

### [Use Case 1]
```bash
# Example command or code snippet
curl -X POST [ENDPOINT] \
  -H "Authorization: Bearer [TOKEN]" \
  -H "Content-Type: application/json" \
  -d '{
    "key": "value"
  }'
```

### [Use Case 2]
```bash
# Example command or code snippet
curl -X GET "[ENDPOINT]?param=value" \
  -H "Authorization: Bearer [TOKEN]"
```

## Testing

### Test Environment
- **Base URL**: [TEST_BASE_URL]
- **Credentials**: [Test credentials information]
- **Data**: [Test data availability]

### Postman Collection
[Link to Postman collection or instructions for importing]

## Support

- **Documentation**: [Documentation URL]
- **Issues**: [Issue tracker URL]
- **Discussions**: [Discussion forum URL]
- **Email**: [Support email]

## Related Documentation

- [Main Docs](../README.md)
- [Architecture Overview](../architecture/overview.md)
- [Development Setup](../development/setup.md)
- [Deployment Guide](../deployment/)

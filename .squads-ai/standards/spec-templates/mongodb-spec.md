# MongoDB Specification Template

This template provides comprehensive coverage for MongoDB-based specifications, covering document modeling, collections, indexes, aggregation pipelines, and MongoDB-specific best practices.

## Document Model Design

### Collection Structure
```javascript
// Example collection schema
{
  "_id": ObjectId,           // MongoDB auto-generated unique identifier
  "metadata": {
    "created_at": Date,       // Document creation timestamp
    "updated_at": Date,       // Last modification timestamp
    "version": Number,        // Document version for optimistic locking
    "status": String          // Document status (active, archived, deleted)
  },
  "business_data": {
    // Main business logic fields
  },
  "relationships": {
    // References to other documents
  }
}
```

### Data Types and Constraints
- **String**: Max length, validation patterns, required fields
- **Number**: Min/max values, precision requirements
- **Date**: Timezone handling, format requirements
- **Array**: Max elements, unique constraints, nested validation
- **Object**: Nested schema validation, required sub-fields
- **Boolean**: Default values, business logic implications

### Validation Rules
```javascript
// Example validation schema
{
  validator: {
    $jsonSchema: {
      required: ["field1", "field2"],
      properties: {
        field1: { type: "string", minLength: 1, maxLength: 100 },
        field2: { type: "number", minimum: 0, maximum: 1000 }
      }
    }
  }
}
```

## Collection Design

### Collection Naming
- **Plural nouns**: users, products, orders
- **Descriptive**: user_sessions, product_categories
- **Consistent casing**: snake_case or camelCase (project standard)
- **Avoid prefixes**: No "tbl_" or "col_" prefixes

### Collection Relationships
- **Embedded Documents**: For one-to-one or one-to-few relationships
- **References**: For one-to-many or many-to-many relationships
- **Hybrid Approach**: Embed frequently accessed data, reference less common

### Sharding Strategy
- **Shard Key Selection**: High cardinality, even distribution
- **Chunk Size**: Default 64MB, adjust based on data patterns
- **Jumbo Chunks**: Monitor and split oversized chunks

## Index Strategy

### Primary Indexes
```javascript
// Single field indexes
db.collection.createIndex({ "field": 1 })

// Compound indexes
db.collection.createIndex({ "field1": 1, "field2": -1 })

// Unique indexes
db.collection.createIndex({ "email": 1 }, { unique: true })

// Sparse indexes
db.collection.createIndex({ "optional_field": 1 }, { sparse: true })
```

### Index Types
- **Single Field**: Basic queries on individual fields
- **Compound**: Multi-field queries, order matters
- **Multikey**: Array fields, automatic for arrays
- **Text**: Full-text search capabilities
- **Geospatial**: Location-based queries
- **Hashed**: Sharding on non-numeric fields

### Index Optimization
- **Covered Queries**: Index includes all required fields
- **Index Size**: Monitor index storage and memory usage
- **Index Maintenance**: Regular index analysis and cleanup

## Aggregation Pipeline Design

### Pipeline Stages
```javascript
// Example aggregation pipeline
db.collection.aggregate([
  { $match: { status: "active" } },           // Filter documents
  { $lookup: { ... } },                       // Join with other collections
  { $group: { _id: "$category", count: { $sum: 1 } } }, // Group and aggregate
  { $sort: { count: -1 } },                   // Sort results
  { $limit: 10 }                              // Limit output
])
```

### Common Stages
- **$match**: Filter documents early in pipeline
- **$lookup**: Join with other collections
- **$group**: Aggregate data by specified criteria
- **$project**: Reshape documents
- **$sort**: Order results
- **$limit/$skip**: Pagination

### Performance Considerations
- **Early Filtering**: Use $match early to reduce document count
- **Index Usage**: Ensure pipeline stages can use indexes
- **Memory Limits**: Monitor pipeline memory usage
- **Pipeline Optimization**: Use $hint for index selection

## Data Migration and Versioning

### Schema Evolution
```javascript
// Version-based schema handling
{
  "schema_version": 1,
  "data": { ... },
  "migration_history": [
    {
      "from_version": 0,
      "to_version": 1,
      "migration_date": Date,
      "migration_script": "migration_001.js"
    }
  ]
}
```

### Migration Strategies
- **Forward-Only**: Add new fields, deprecate old ones
- **Backward Compatible**: Maintain old field support
- **Data Transformation**: Convert old format to new format
- **Rollback Plan**: Ability to revert schema changes

### Migration Scripts
```javascript
// Example migration script
db.collection.updateMany(
  { "schema_version": 0 },
  {
    $set: {
      "new_field": "default_value",
      "schema_version": 1
    },
    $rename: {
      "old_field": "new_field_name"
    }
  }
)
```

## Performance and Scalability

### Read Optimization
- **Index Coverage**: Ensure queries use indexes
- **Projection**: Only return required fields
- **Cursor Management**: Use appropriate batch sizes
- **Read Preferences**: Primary vs secondary reads

### Write Optimization
- **Bulk Operations**: Use insertMany, updateMany
- **Write Concerns**: Balance durability vs performance
- **Ordered vs Unordered**: Choose based on requirements
- **Write Acknowledgment**: Configure acknowledgment levels

### Connection Management
- **Connection Pooling**: Configure appropriate pool size
- **Connection Timeouts**: Set reasonable timeout values
- **Replica Set Awareness**: Handle failover gracefully

## Security and Access Control

### Authentication
- **User Roles**: Define specific permissions
- **Database Access**: Control access to specific databases
- **Collection Access**: Fine-grained collection permissions

### Data Protection
- **Field-Level Encryption**: Sensitive data encryption
- **Audit Logging**: Track data access and modifications
- **Data Masking**: Hide sensitive fields in queries

## Monitoring and Maintenance

### Health Checks
- **Connection Status**: Monitor database connectivity
- **Performance Metrics**: Query execution times
- **Storage Usage**: Monitor collection and index sizes
- **Replica Set Status**: Check replication health

### Maintenance Tasks
- **Index Rebuilding**: Periodic index optimization
- **Data Archiving**: Move old data to archive collections
- **Statistics Updates**: Keep collection statistics current
- **Compaction**: Optimize storage for WiredTiger

## Testing and Validation

### Test Data Setup
```javascript
// Example test data
db.collection.insertMany([
  {
    "field1": "test_value_1",
    "field2": 100,
    "metadata": {
      "created_at": new Date(),
      "status": "active"
    }
  }
])
```

### Test Scenarios
- **CRUD Operations**: Create, read, update, delete
- **Index Usage**: Verify query plan uses indexes
- **Aggregation**: Test complex aggregation pipelines
- **Edge Cases**: Handle null values, empty arrays
- **Performance**: Query execution time validation

## Integration Patterns

### Application Integration
- **Connection String**: Environment-specific configuration
- **Error Handling**: Graceful degradation on failures
- **Retry Logic**: Exponential backoff for transient errors
- **Circuit Breaker**: Prevent cascade failures

### Data Consistency
- **Eventual Consistency**: Handle replica set lag
- **Write Concerns**: Configure durability requirements
- **Read Preferences**: Choose appropriate read sources
- **Conflict Resolution**: Handle concurrent modifications

## Best Practices Summary

### Do's
- ✅ Design schemas for query patterns, not normalization
- ✅ Use appropriate indexes for common queries
- ✅ Embed frequently accessed data
- ✅ Use aggregation pipelines for complex queries
- ✅ Monitor and optimize performance regularly

### Don'ts
- ❌ Don't create too many indexes (write performance impact)
- ❌ Don't embed large arrays (16MB document limit)
- ❌ Don't use _id for business logic
- ❌ Don't ignore connection pooling configuration
- ❌ Don't skip monitoring and maintenance

This template ensures comprehensive MongoDB specification coverage for modern applications using document-based data storage.

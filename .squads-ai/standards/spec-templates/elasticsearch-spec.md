# Elasticsearch Specification Template

This template provides comprehensive coverage for Elasticsearch-based specifications, covering index design, mapping, search queries, aggregation, and Elasticsearch-specific best practices.

## Index Design

### Index Naming Convention
```
[environment].[domain].[data-type].[version]
Examples:
- prod.user.profiles.v1
- staging.product.catalog.v2
- dev.order.transactions.v1
```

### Index Configuration
```json
{
  "settings": {
    "number_of_shards": 3,
    "number_of_replicas": 1,
    "refresh_interval": "1s",
    "max_result_window": 10000,
    "analysis": {
      "analyzer": {
        "custom_analyzer": {
          "type": "custom",
          "tokenizer": "standard",
          "filter": ["lowercase", "stop", "snowball"]
        }
      }
    }
  }
}
```

### Index Lifecycle Management
```json
{
  "policy": {
    "phases": {
      "hot": {
        "min_age": "0ms",
        "actions": {
          "rollover": {
            "max_size": "50GB",
            "max_age": "1d"
          }
        }
      },
      "warm": {
        "min_age": "1d",
        "actions": {
          "forcemerge": {
            "max_num_segments": 1
          },
          "shrink": {
            "number_of_shards": 1
          }
        }
      },
      "cold": {
        "min_age": "7d",
        "actions": {
          "freeze": {}
        }
      },
      "delete": {
        "min_age": "30d",
        "actions": {
          "delete": {}
        }
      }
    }
  }
}
```

## Mapping Design

### Field Types
```json
{
  "mappings": {
    "properties": {
      "user_id": {
        "type": "keyword",
        "ignore_above": 256
      },
      "email": {
        "type": "keyword",
        "normalizer": "lowercase"
      },
      "name": {
        "type": "text",
        "analyzer": "standard",
        "fields": {
          "keyword": {
            "type": "keyword",
            "ignore_above": 256
          }
        }
      },
      "age": {
        "type": "integer",
        "index": true
      },
      "created_at": {
        "type": "date",
        "format": "strict_date_optional_time||epoch_millis"
      },
      "location": {
        "type": "geo_point"
      },
      "tags": {
        "type": "keyword",
        "ignore_above": 256
      },
      "profile_data": {
        "type": "object",
        "dynamic": "strict",
        "properties": {
          "bio": {
            "type": "text",
            "analyzer": "standard"
          },
          "preferences": {
            "type": "object",
            "dynamic": true
          }
        }
      }
    }
  }
}
```

### Dynamic Mapping
```json
{
  "mappings": {
    "dynamic": "strict",
    "properties": {
      "user_id": {
        "type": "keyword"
      }
    }
  }
}
```

### Nested Objects
```json
{
  "mappings": {
    "properties": {
      "orders": {
        "type": "nested",
        "properties": {
          "order_id": {
            "type": "keyword"
          },
          "amount": {
            "type": "float"
          },
          "items": {
            "type": "nested",
            "properties": {
              "product_id": {
                "type": "keyword"
              },
              "quantity": {
                "type": "integer"
              }
            }
          }
        }
      }
    }
  }
}
```

## Search Query Design

### Basic Search
```json
{
  "query": {
    "bool": {
      "must": [
        {
          "match": {
            "name": {
              "query": "john doe",
              "operator": "and"
            }
          }
        }
      ],
      "filter": [
        {
          "range": {
            "age": {
              "gte": 18,
              "lte": 65
            }
          }
        },
        {
          "term": {
            "status": "active"
          }
        }
      ]
    }
  }
}
```

### Full-Text Search
```json
{
  "query": {
    "multi_match": {
      "query": "elasticsearch tutorial",
      "fields": ["title^2", "content", "tags"],
      "type": "best_fields",
      "fuzziness": "AUTO"
    }
  },
  "highlight": {
    "fields": {
      "title": {},
      "content": {
        "fragment_size": 150,
        "number_of_fragments": 3
      }
    }
  }
}
```

### Geo-Spatial Search
```json
{
  "query": {
    "bool": {
      "must": {
        "match_all": {}
      },
      "filter": {
        "geo_distance": {
          "distance": "10km",
          "location": {
            "lat": 40.7128,
            "lon": -74.0060
          }
        }
      }
    }
  },
  "sort": [
    {
      "_geo_distance": {
        "location": {
          "lat": 40.7128,
          "lon": -74.0060
        },
        "order": "asc",
        "unit": "km"
      }
    }
  ]
}
```

### Aggregation Queries
```json
{
  "size": 0,
  "aggs": {
    "age_distribution": {
      "histogram": {
        "field": "age",
        "interval": 10
      }
    },
    "status_count": {
      "terms": {
        "field": "status",
        "size": 10
      }
    },
    "avg_age": {
      "avg": {
        "field": "age"
      }
    },
    "location_clusters": {
      "geohash_grid": {
        "field": "location",
        "precision": 4
      }
    }
  }
}
```

## Data Indexing

### Bulk Indexing
```json
// Bulk indexing format
{"index": {"_index": "users", "_id": "123"}}
{"user_id": "123", "name": "John Doe", "email": "john@example.com", "age": 30}
{"index": {"_index": "users", "_id": "124"}}
{"user_id": "124", "name": "Jane Smith", "email": "jane@example.com", "age": 25}
```

### Index Aliases
```json
{
  "actions": [
    {
      "add": {
        "index": "users_v2",
        "alias": "users"
      }
    },
    {
      "remove": {
        "index": "users_v1",
        "alias": "users"
      }
    }
  ]
}
```

### Reindexing
```json
{
  "source": {
    "index": "users_v1"
  },
  "dest": {
    "index": "users_v2"
  },
  "script": {
    "source": "ctx._source.remove('deprecated_field')"
  }
}
```

## Performance Optimization

### Sharding Strategy
```json
{
  "settings": {
    "number_of_shards": 3,
    "routing": {
      "allocation": {
        "include": {
          "node_type": "hot"
        }
      }
    }
  }
}
```

### Caching Configuration
```json
{
  "settings": {
    "indices": {
      "queries": {
        "cache": {
          "enabled": true,
          "size": "10%"
        }
      },
      "fielddata": {
        "cache": {
          "size": "20%"
        }
      }
    }
  }
}
```

### Query Optimization
```json
{
  "query": {
    "bool": {
      "must": [
        {
          "term": {
            "status": "active"
          }
        }
      ],
      "should": [
        {
          "match": {
            "name": "john"
          }
        }
      ],
      "minimum_should_match": 1
    }
  },
  "size": 20,
  "from": 0,
  "sort": [
    {
      "created_at": {
        "order": "desc"
      }
    }
  ]
}
```

## Security Configuration

### Role-Based Access Control
```json
{
  "role": {
    "cluster": ["monitor"],
    "indices": [
      {
        "names": ["users*"],
        "privileges": ["read"],
        "field_security": {
          "grant": ["user_id", "name", "email"]
        }
      }
    ]
  }
}
```

### Document-Level Security
```json
{
  "query": {
    "bool": {
      "must": [
        {
          "match": {
            "name": "john"
          }
        }
      ],
      "filter": [
        {
          "term": {
            "tenant_id": "tenant_123"
          }
        }
      ]
    }
  }
}
```

## Monitoring and Observability

### Cluster Health
```json
{
  "cluster": {
    "health": {
      "status": "green",
      "number_of_nodes": 3,
      "active_shards": 6,
      "relocating_shards": 0,
      "initializing_shards": 0,
      "unassigned_shards": 0
    }
  }
}
```

### Index Statistics
```json
{
  "indices": {
    "users": {
      "total": {
        "docs": {
          "count": 10000,
          "deleted": 100
        },
        "store": {
          "size_in_bytes": 1048576
        },
        "indexing": {
          "index_total": 10000,
          "index_time_in_millis": 5000
        },
        "search": {
          "query_total": 5000,
          "query_time_in_millis": 2000
        }
      }
    }
  }
}
```

### Performance Metrics
```yaml
# Key metrics to monitor
cluster_metrics:
  - cluster_health_status
  - number_of_nodes
  - active_shards
  - unassigned_shards

index_metrics:
  - indexing_rate
  - search_rate
  - refresh_rate
  - merge_rate

node_metrics:
  - cpu_usage
  - memory_usage
  - disk_usage
  - network_io
```

## Testing and Validation

### Test Data Setup
```json
{
  "index": "test_users",
  "body": [
    {
      "user_id": "test_001",
      "name": "Test User 1",
      "email": "test1@example.com",
      "age": 25,
      "status": "active"
    },
    {
      "user_id": "test_002",
      "name": "Test User 2",
      "email": "test2@example.com",
      "age": 30,
      "status": "inactive"
    }
  ]
}
```

### Test Scenarios
```javascript
// Example integration test
describe('Elasticsearch Integration Tests', () => {
  beforeEach(async () => {
    await createTestIndex();
    await indexTestData();
  });
  
  afterEach(async () => {
    await deleteTestIndex();
  });
  
  it('should search users by name', async () => {
    const response = await client.search({
      index: 'test_users',
      body: {
        query: {
          match: {
            name: 'Test User'
          }
        }
      }
    });
    
    expect(response.hits.total.value).toBe(2);
    expect(response.hits.hits).toHaveLength(2);
  });
  
  it('should filter users by status', async () => {
    const response = await client.search({
      index: 'test_users',
      body: {
        query: {
          bool: {
            filter: [
              {
                term: {
                  status: 'active'
                }
              }
            ]
          }
        }
      }
    });
    
    expect(response.hits.total.value).toBe(1);
    expect(response.hits.hits[0]._source.status).toBe('active');
  });
});
```

## Deployment and Operations

### Cluster Configuration
```yaml
# Production cluster sizing
production_cluster:
  master_nodes: 3
  data_nodes: 6
  ingest_nodes: 2
  coordinating_nodes: 2
  
  resources_per_node:
    cpu: 16 cores
    memory: 64GB
    storage: 2TB SSD
    
  network:
    bandwidth: 10Gbps
    latency: <1ms
```

### Backup Strategy
```json
{
  "snapshot": {
    "repository": "s3_repository",
    "snapshot": "users_backup_20250127",
    "indices": ["users*"],
    "ignore_unavailable": true,
    "include_global_state": false
  }
}
```

### Disaster Recovery
```yaml
# DR configuration
disaster_recovery:
  primary_cluster: us-east-1
  secondary_cluster: us-west-2
  replication:
    type: cross_cluster
    indices: ["users*", "orders*"]
  failover:
    automatic: false
    manual_timeout: 5 minutes
```

## Best Practices Summary

### Do's
- ✅ Design indices for query patterns, not data normalization
- ✅ Use appropriate field types and analyzers
- ✅ Implement proper index lifecycle management
- ✅ Monitor cluster health and performance metrics
- ✅ Use bulk operations for data indexing
- ✅ Implement proper security and access control
- ✅ Test thoroughly with realistic data volumes

### Don'ts
- ❌ Don't create too many shards per index
- ❌ Don't ignore mapping design and field types
- ❌ Don't skip monitoring and alerting setup
- ❌ Don't forget about security configuration
- ❌ Don't use wildcard queries without limits
- ❌ Don't ignore index optimization and maintenance

This template ensures comprehensive Elasticsearch specification coverage for modern search, analytics, and data processing applications.

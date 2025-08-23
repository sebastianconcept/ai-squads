# Redis Specification Template

This template provides comprehensive coverage for Redis-based specifications, covering data structures, caching strategies, pub/sub messaging, clustering, and Redis-specific best practices.

## Data Structure Design

### String Operations
```redis
# Basic string operations
SET user:123:profile "{\"name\":\"John\",\"email\":\"john@example.com\"}"
GET user:123:profile
SETEX user:123:session 3600 "session_token_abc123"
TTL user:123:session

# Atomic operations
INCR user:123:login_count
INCRBY user:123:points 100
DECR user:123:remaining_attempts
```

### Hash Operations
```redis
# User profile as hash
HSET user:123 name "John Doe"
HSET user:123 email "john@example.com"
HSET user:123 age 30
HGETALL user:123
HGET user:123 name
HMSET user:123 name "John Doe" email "john@example.com" age 30
```

### List Operations
```redis
# Activity feed as list
LPUSH user:123:activity "User logged in"
LPUSH user:123:activity "User updated profile"
LRANGE user:123:activity 0 9
LTRIM user:123:activity 0 99  # Keep only last 100 items
LLEN user:123:activity
```

### Set Operations
```redis
# User roles and permissions
SADD user:123:roles "admin" "moderator"
SADD user:123:permissions "read" "write" "delete"
SMEMBERS user:123:roles
SISMEMBER user:123:roles "admin"
SINTER user:123:roles user:456:roles  # Common roles
```

### Sorted Set Operations
```redis
# Leaderboard and rankings
ZADD leaderboard 1000 "player1"
ZADD leaderboard 950 "player2"
ZADD leaderboard 1100 "player3"
ZREVRANGE leaderboard 0 9 WITHSCORES
ZRANK leaderboard "player1"
ZSCORE leaderboard "player1"
```

## Caching Strategy

### Cache Patterns
```redis
# Cache-aside pattern
GET user:123:profile
# If miss, fetch from database and cache
SETEX user:123:profile 3600 "{\"name\":\"John\",\"email\":\"john@example.com\"}"

# Write-through pattern
SET user:123:profile "{\"name\":\"John\",\"email\":\"john@example.com\"}"
# Also update database immediately

# Write-behind pattern
SET user:123:profile "{\"name\":\"John\",\"email\":\"john@example.com\"}"
# Queue for batch database update
```

### Cache Invalidation
```redis
# Time-based expiration
SETEX user:123:profile 3600 "profile_data"  # Expire in 1 hour
EXPIRE user:123:profile 3600

# Event-based invalidation
DEL user:123:profile
DEL user:123:preferences
DEL user:123:settings

# Pattern-based invalidation
DEL user:123:*  # Remove all user:123 keys
```

### Cache Warming
```redis
# Pre-populate frequently accessed data
SETEX popular_products 86400 "product_list_json"
SETEX user_recommendations:123 3600 "recommendations_json"

# Background refresh
SETEX cache_refresh_lock 300 "locked"  # Prevent multiple refreshes
# If lock doesn't exist, refresh cache
```

## Pub/Sub Messaging

### Channel Design
```redis
# Channel naming convention
[environment].[service].[event-type].[version]
Examples:
- prod.user.profile.updated.v1
- staging.order.payment.processed.v2
- dev.notification.email.sent.v1
```

### Publisher Configuration
```javascript
// Example publisher configuration
const redis = require('redis');
const publisher = redis.createClient({
  host: 'localhost',
  port: 6379,
  retry_strategy: (options) => {
    if (options.total_retry_time > 1000 * 60 * 60) {
      return new Error('Retry time exhausted');
    }
    if (options.attempt > 10) {
      return undefined;
    }
    return Math.min(options.attempt * 100, 3000);
  }
});

// Publish message
publisher.publish('user.profile.updated', JSON.stringify({
  user_id: '123',
  event_type: 'profile.updated',
  timestamp: new Date().toISOString(),
  data: { name: 'John Doe', email: 'john@example.com' }
}));
```

### Subscriber Configuration
```javascript
// Example subscriber configuration
const subscriber = redis.createClient({
  host: 'localhost',
  port: 6379,
  retry_strategy: (options) => {
    if (options.total_retry_time > 1000 * 60 * 60) {
      return new Error('Retry time exhausted');
    }
    if (options.attempt > 10) {
      return undefined;
    }
    return Math.min(options.attempt * 100, 3000);
  }
});

// Subscribe to channels
subscriber.subscribe('user.profile.updated', 'user.login');
subscriber.on('message', (channel, message) => {
  console.log(`Received message on ${channel}:`, JSON.parse(message));
});
```

### Message Patterns
```redis
# Pattern subscriptions
PSUBSCRIBE user.*.updated
PSUBSCRIBE *.profile.*
PSUBSCRIBE user.profile.*

# Wildcard matching
user.profile.updated
user.profile.created
user.profile.deleted
```

## Clustering and High Availability

### Redis Cluster Configuration
```yaml
# Redis cluster configuration
cluster:
  nodes: 6
  replicas: 1
  hash_slots: 16384
  node_timeout: 5000ms
  cluster_require_full_coverage: yes
  
nodes:
  - host: redis-node-1
    port: 7000
    role: master
    slots: 0-5461
  
  - host: redis-node-2
    port: 7001
    role: master
    slots: 5462-10922
  
  - host: redis-node-3
    port: 7002
    role: master
    slots: 10923-16383
```

### Sentinel Configuration
```yaml
# Redis Sentinel configuration
sentinel:
  quorum: 2
  down_after_milliseconds: 30000
  parallel_syncs: 1
  failover_timeout: 180000
  
instances:
  - host: sentinel-1
    port: 26379
    monitor: mymaster redis-master 6379 2
  
  - host: sentinel-2
    port: 26380
    monitor: mymaster redis-master 6379 2
  
  - host: sentinel-3
    port: 26381
    monitor: mymaster redis-master 6379 2
```

### Connection Management
```javascript
// Example cluster connection
const Redis = require('ioredis');
const cluster = new Redis.Cluster([
  { host: 'redis-node-1', port: 7000 },
  { host: 'redis-node-2', port: 7001 },
  { host: 'redis-node-3', port: 7002 }
], {
  redisOptions: {
    password: 'cluster_password',
    retryDelayOnFailover: 100,
    maxRetriesPerRequest: 3
  },
  clusterRetryStrategy: (times) => {
    const delay = Math.min(times * 50, 2000);
    return delay;
  }
});
```

## Performance Optimization

### Memory Management
```redis
# Memory configuration
maxmemory 2gb
maxmemory-policy allkeys-lru
maxmemory-samples 5

# Memory optimization
SET user:123:profile "profile_data" EX 3600
SET user:123:session "session_data" PX 3600000
```

### Pipeline Operations
```javascript
// Example pipeline usage
const pipeline = redis.pipeline();
pipeline.set('user:123:name', 'John Doe');
pipeline.set('user:123:email', 'john@example.com');
pipeline.expire('user:123:name', 3600);
pipeline.expire('user:123:email', 3600);
pipeline.exec((err, results) => {
  console.log('Pipeline results:', results);
});
```

### Connection Pooling
```javascript
// Example connection pool configuration
const redis = require('redis');
const client = redis.createClient({
  host: 'localhost',
  port: 6379,
  maxRetriesPerRequest: 3,
  retryDelayOnFailover: 100,
  enableReadyCheck: true,
  maxRetriesPerRequest: 3,
  lazyConnect: true
});
```

## Security Configuration

### Authentication
```redis
# Redis authentication
AUTH your_password

# Configuration file
requirepass your_strong_password
```

### Network Security
```redis
# Bind to specific interfaces
bind 127.0.0.1 192.168.1.100

# Protected mode
protected-mode yes

# Port configuration
port 6379
```

### SSL/TLS Configuration
```redis
# SSL configuration
tls-port 6380
tls-cert-file /path/to/redis.crt
tls-key-file /path/to/redis.key
tls-ca-cert-file /path/to/ca.crt
```

## Monitoring and Observability

### Key Metrics
```yaml
# Performance metrics
performance:
  - ops_per_sec
  - hit_rate
  - memory_usage
  - connected_clients
  - blocked_clients

# Memory metrics
memory:
  - used_memory
  - used_memory_peak
  - used_memory_rss
  - mem_fragmentation_ratio

# Network metrics
network:
  - total_connections_received
  - total_commands_processed
  - instantaneous_ops_per_sec
  - total_net_input_bytes
  - total_net_output_bytes
```

### Health Checks
```javascript
// Example health check
async function checkRedisHealth() {
  try {
    const start = Date.now();
    await redis.ping();
    const latency = Date.now() - start;
    
    const info = await redis.info();
    const memory = await redis.info('memory');
    
    return {
      status: 'healthy',
      latency: `${latency}ms`,
      memory: parseMemoryInfo(memory),
      uptime: parseInfo(info, 'uptime_in_seconds')
    };
  } catch (error) {
    return {
      status: 'unhealthy',
      error: error.message
    };
  }
}
```

### Logging Configuration
```redis
# Redis logging configuration
loglevel notice
logfile /var/log/redis/redis.log
slowlog-log-slower-than 10000
slowlog-max-len 128
```

## Data Persistence

### RDB Configuration
```redis
# RDB persistence
save 900 1
save 300 10
save 60 10000
stop-writes-on-bgsave-error yes
rdbcompression yes
rdbchecksum yes
dbfilename dump.rdb
dir /var/lib/redis
```

### AOF Configuration
```redis
# AOF persistence
appendonly yes
appendfilename "appendonly.aof"
appendfsync everysec
no-appendfsync-on-rewrite no
auto-aof-rewrite-percentage 100
auto-aof-rewrite-min-size 64mb
```

### Hybrid Persistence
```redis
# Hybrid approach
save 900 1
save 300 10
save 60 10000
appendonly yes
appendfsync everysec
```

## Testing and Validation

### Test Environment Setup
```yaml
# Test Redis configuration
test_redis:
  host: localhost
  port: 6379
  database: 15  # Use separate database for testing
  password: null
  max_connections: 10
```

### Test Scenarios
```javascript
// Example integration test
describe('Redis Integration Tests', () => {
  beforeEach(async () => {
    await redis.flushdb(); // Clear test database
  });
  
  afterEach(async () => {
    await redis.flushdb(); // Clean up after each test
  });
  
  it('should cache user profile', async () => {
    const userProfile = { name: 'John Doe', email: 'john@example.com' };
    
    await redis.setex('user:123:profile', 3600, JSON.stringify(userProfile));
    
    const cached = await redis.get('user:123:profile');
    expect(JSON.parse(cached)).toEqual(userProfile);
    
    const ttl = await redis.ttl('user:123:profile');
    expect(ttl).toBeGreaterThan(0);
  });
});
```

### Performance Testing
```javascript
// Example performance test
async function benchmarkRedisOperations() {
  const iterations = 10000;
  const start = Date.now();
  
  for (let i = 0; i < iterations; i++) {
    await redis.set(`key:${i}`, `value:${i}`);
  }
  
  const end = Date.now();
  const duration = end - start;
  const opsPerSecond = Math.round(iterations / (duration / 1000));
  
  console.log(`Performance: ${opsPerSecond} ops/sec`);
  return opsPerSecond;
}
```

## Deployment and Operations

### Container Configuration
```yaml
# Docker Compose configuration
version: '3.8'
services:
  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    volumes:
      - redis_data:/data
      - ./redis.conf:/usr/local/etc/redis/redis.conf
    command: redis-server /usr/local/etc/redis/redis.conf
    environment:
      - REDIS_PASSWORD=your_password
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 30s
      timeout: 10s
      retries: 3

volumes:
  redis_data:
```

### Backup Strategy
```bash
# Backup script example
#!/bin/bash
BACKUP_DIR="/backups/redis"
DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="redis_backup_$DATE.rdb"

# Create backup directory
mkdir -p $BACKUP_DIR

# Trigger RDB save
redis-cli BGSAVE

# Wait for save to complete
while [ "$(redis-cli info persistence | grep rdb_bgsave_in_progress | cut -d: -f2)" != "0" ]; do
  sleep 1
done

# Copy RDB file
cp /var/lib/redis/dump.rdb $BACKUP_DIR/$BACKUP_FILE

# Compress backup
gzip $BACKUP_DIR/$BACKUP_FILE

echo "Backup completed: $BACKUP_DIR/$BACKUP_FILE.gz"
```

### Monitoring and Alerting
```yaml
# Prometheus configuration
scrape_configs:
  - job_name: 'redis'
    static_configs:
      - targets: ['localhost:6379']
    metrics_path: /metrics
    scrape_interval: 15s

# Alert rules
groups:
  - name: redis_alerts
    rules:
      - alert: RedisDown
        expr: redis_up == 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "Redis instance is down"
      
      - alert: RedisMemoryHigh
        expr: redis_memory_used_bytes / redis_memory_max_bytes > 0.8
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Redis memory usage is high"
```

## Best Practices Summary

### Do's
- ✅ Use appropriate data structures for your use case
- ✅ Implement proper cache invalidation strategies
- ✅ Use pipelining for multiple operations
- ✅ Monitor memory usage and set appropriate limits
- ✅ Implement proper error handling and retry logic
- ✅ Use Redis Cluster for high availability
- ✅ Implement proper backup and recovery procedures

### Don'ts
- ❌ Don't store large objects in Redis (use database instead)
- ❌ Don't ignore memory limits and eviction policies
- ❌ Don't use Redis as a primary database
- ❌ Don't skip monitoring and alerting setup
- ❌ Don't forget about security configuration
- ❌ Don't ignore connection pooling and timeout settings

This template ensures comprehensive Redis specification coverage for modern caching, messaging, and data storage applications.

# Kafka Specification Template

This template provides comprehensive coverage for Apache Kafka-based specifications, covering topics, partitions, producers, consumers, streams processing, and Kafka-specific best practices.

## Topic Design

### Topic Naming Convention
```
[environment].[domain].[event-type].[version]
Examples:
- prod.user.account.created.v1
- staging.order.payment.processed.v2
- dev.notification.email.sent.v1
```

### Topic Configuration
```yaml
# Example topic configuration
topic: user-events
partitions: 6
replication-factor: 3
retention: 7 days
cleanup-policy: delete
compression: lz4
max-message-size: 1MB
```

### Topic Categories
- **Event Topics**: Domain events, business transactions
- **Command Topics**: Instructions to services
- **Query Topics**: Data requests and responses
- **Dead Letter Topics**: Failed message processing
- **Audit Topics**: System activity logging

## Partition Strategy

### Partition Count
- **Rule of Thumb**: Number of partitions = Number of consumers × Parallelism factor
- **Maximum**: 4000 partitions per broker (Kafka 2.8+)
- **Scaling**: Plan for 2-3x growth in consumer count

### Partition Key Selection
```java
// Example partition key strategies
// 1. User ID for user-specific events
String partitionKey = "user_" + userId;

// 2. Order ID for order processing
String partitionKey = "order_" + orderId;

// 3. Geographic region for location-based events
String partitionKey = "region_" + countryCode;

// 4. Time-based for temporal events
String partitionKey = "hour_" + (timestamp / 3600000);
```

### Partition Distribution
- **Even Distribution**: Ensure balanced partition assignment
- **Hot Partitions**: Monitor for uneven load distribution
- **Partition Rebalancing**: Handle consumer group changes gracefully

## Producer Design

### Producer Configuration
```java
// Example producer configuration
Properties props = new Properties();
props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, JsonSerializer.class);
props.put(ProducerConfig.ACKS_CONFIG, "all"); // Wait for all replicas
props.put(ProducerConfig.RETRIES_CONFIG, 3);
props.put(ProducerConfig.BATCH_SIZE_CONFIG, 16384);
props.put(ProducerConfig.LINGER_MS_CONFIG, 5);
props.put(ProducerConfig.BUFFER_MEMORY_CONFIG, 33554432);
```

### Message Structure
```json
{
  "metadata": {
    "message_id": "uuid-v4",
    "timestamp": "2025-01-27T10:00:00Z",
    "version": "1.0",
    "source": "user-service",
    "correlation_id": "request-uuid"
  },
  "payload": {
    "event_type": "user.created",
    "user_id": "12345",
    "data": {
      "email": "user@example.com",
      "name": "John Doe"
    }
  }
}
```

### Producer Patterns
- **Synchronous**: Wait for acknowledgment (high durability)
- **Asynchronous**: Fire-and-forget (high throughput)
- **Batched**: Group messages for efficiency
- **Transactional**: Ensure message ordering and exactly-once semantics

## Consumer Design

### Consumer Configuration
```java
// Example consumer configuration
Properties props = new Properties();
props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
props.put(ConsumerConfig.GROUP_ID_CONFIG, "user-events-processor");
props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);
props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, JsonDeserializer.class);
props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, false);
props.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, 500);
props.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, 30000);
```

### Consumer Groups
```yaml
# Example consumer group configuration
group_id: order-processing-service
members: 3
partitions_per_member: 2
auto_offset_reset: earliest
enable_auto_commit: false
session_timeout_ms: 30000
heartbeat_interval_ms: 10000
```

### Consumer Patterns
- **Single Consumer**: Simple message processing
- **Consumer Group**: Parallel processing with load balancing
- **Broadcast Consumer**: All consumers receive all messages
- **Pattern Consumer**: Subscribe to multiple topics with wildcards

## Stream Processing

### Kafka Streams Configuration
```java
// Example Kafka Streams configuration
Properties props = new Properties();
props.put(StreamsConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
props.put(StreamsConfig.APPLICATION_ID_CONFIG, "user-analytics-streams");
props.put(StreamsConfig.DEFAULT_KEY_SERDE_CLASS_CONFIG, Serdes.String().getClass());
props.put(StreamsConfig.DEFAULT_VALUE_SERDE_CLASS_CONFIG, Serdes.String().getClass());
props.put(StreamsConfig.PROCESSING_GUARANTEE_CONFIG, StreamsConfig.EXACTLY_ONCE_V2);
props.put(StreamsConfig.NUM_STREAM_THREADS_CONFIG, 3);
```

### Stream Topology
```java
// Example stream topology
StreamsBuilder builder = new StreamsBuilder();

// Read from input topic
KStream<String, UserEvent> userEvents = builder.stream("user-events");

// Process and transform
KStream<String, UserAnalytics> analytics = userEvents
    .filter((key, value) -> value.getEventType().equals("user.created"))
    .mapValues(value -> new UserAnalytics(value.getUserId(), value.getTimestamp()))
    .groupByKey()
    .windowedBy(TimeWindows.of(Duration.ofHours(1)))
    .aggregate(
        () -> new UserAnalyticsAggregator(),
        (key, value, aggregate) -> aggregate.add(value),
        Materialized.as("user-analytics-store")
    )
    .toStream()
    .map((key, value) -> KeyValue.pair(key.key(), value));

// Write to output topic
analytics.to("user-analytics");
```

### Stream Processing Patterns
- **Filtering**: Remove unwanted messages
- **Transformation**: Convert message format
- **Aggregation**: Combine multiple messages
- **Joining**: Merge data from multiple topics
- **Windowing**: Process messages within time boundaries

## Data Schema Management

### Schema Registry
```yaml
# Example schema registry configuration
schema_registry_url: http://localhost:8081
compatibility: backward
version: 1.0
subjects:
  - user-events-value
  - order-events-value
  - payment-events-value
```

### Schema Evolution
```json
// Example schema evolution
// Version 1.0
{
  "type": "object",
  "properties": {
    "user_id": { "type": "string" },
    "email": { "type": "string" }
  },
  "required": ["user_id", "email"]
}

// Version 1.1 (backward compatible)
{
  "type": "object",
  "properties": {
    "user_id": { "type": "string" },
    "email": { "type": "string" },
    "phone": { "type": "string" }
  },
  "required": ["user_id", "email"]
}
```

### Schema Compatibility
- **Backward**: New schema can read old data
- **Forward**: Old schema can read new data
- **Full**: Both backward and forward compatible
- **None**: No compatibility guarantees

## Error Handling and Resilience

### Dead Letter Queue
```yaml
# Dead letter topic configuration
dead_letter_topic: dlq.user-events
retry_topic: retry.user-events
max_retries: 3
retry_delay: 5000ms
```

### Retry Strategies
```java
// Example retry logic
public void processMessage(ConsumerRecord<String, String> record) {
    try {
        // Process message
        processUserEvent(record.value());
        // Commit offset
        commitOffset(record);
    } catch (ProcessingException e) {
        if (getRetryCount(record) < MAX_RETRIES) {
            // Send to retry topic
            sendToRetryTopic(record, e);
        } else {
            // Send to dead letter queue
            sendToDeadLetterQueue(record, e);
        }
    }
}
```

### Circuit Breaker
```java
// Example circuit breaker implementation
CircuitBreaker circuitBreaker = CircuitBreaker.builder()
    .failureRateThreshold(50)
    .waitDurationInOpenState(Duration.ofSeconds(60))
    .ringBufferSizeInHalfOpenState(2)
    .ringBufferSizeInClosedState(100)
    .build();
```

## Performance and Scalability

### Throughput Optimization
- **Batch Processing**: Group messages for efficiency
- **Compression**: Use LZ4 or Snappy compression
- **Partitioning**: Distribute load across partitions
- **Parallelism**: Scale consumers horizontally

### Latency Optimization
- **Producer Batching**: Configure appropriate batch size and linger
- **Consumer Polling**: Optimize poll intervals and batch sizes
- **Network Tuning**: Configure socket buffers and timeouts
- **JVM Tuning**: Optimize garbage collection and memory settings

### Resource Management
```yaml
# Resource allocation
memory:
  heap: 4GB
  off_heap: 2GB
cpu:
  cores: 4
  threads_per_core: 2
network:
  buffer_size: 64KB
  max_connections: 1000
```

## Monitoring and Observability

### Key Metrics
```yaml
# Producer metrics
producer_metrics:
  - record_send_rate
  - record_error_rate
  - request_latency_avg
  - batch_size_avg

# Consumer metrics
consumer_metrics:
  - records_consumed_rate
  - records_lag_max
  - fetch_latency_avg
  - commit_latency_avg

# Topic metrics
topic_metrics:
  - bytes_in_per_sec
  - bytes_out_per_sec
  - messages_in_per_sec
  - partition_count
```

### Health Checks
```java
// Example health check
@Component
public class KafkaHealthIndicator implements HealthIndicator {
    
    @Override
    public Health health() {
        try {
            // Check producer connectivity
            Producer<String, String> producer = createProducer();
            producer.send(new ProducerRecord<>("health-check", "ping"));
            
            // Check consumer connectivity
            Consumer<String, String> consumer = createConsumer();
            consumer.subscribe(Arrays.asList("health-check"));
            
            return Health.up()
                .withDetail("producer", "connected")
                .withDetail("consumer", "connected")
                .build();
        } catch (Exception e) {
            return Health.down()
                .withDetail("error", e.getMessage())
                .build();
        }
    }
}
```

## Security Configuration

### Authentication
```yaml
# SASL/PLAIN authentication
security_protocol: SASL_PLAINTEXT
sasl_mechanism: PLAIN
sasl_jaas_config: |
  org.apache.kafka.common.security.plain.PlainLoginModule required
  username="admin"
  password="admin-secret";
```

### Authorization
```yaml
# ACL configuration
acls:
  - resource_type: topic
    resource_name: user-events
    operation: read
    principal: "User:consumer-service"
    host: "*"
  
  - resource_type: topic
    resource_name: user-events
    operation: write
    principal: "User:producer-service"
    host: "*"
```

### Encryption
```yaml
# SSL/TLS configuration
security_protocol: SSL
ssl_truststore_location: /path/to/kafka.client.truststore.jks
ssl_truststore_password: truststore-password
ssl_keystore_location: /path/to/kafka.client.keystore.jks
ssl_keystore_password: keystore-password
ssl_key_password: key-password
```

## Testing and Validation

### Test Environment Setup
```yaml
# Test Kafka cluster configuration
test_cluster:
  brokers: 3
  zookeeper: 1
  schema_registry: 1
  kafka_ui: 1
  ports:
    kafka: 9092-9094
    zookeeper: 2181
    schema_registry: 8081
    kafka_ui: 8080
```

### Test Scenarios
```java
// Example integration test
@Test
public void testUserEventProcessing() {
    // Send test message
    ProducerRecord<String, String> record = 
        new ProducerRecord<>("user-events", "user123", 
            "{\"event_type\":\"user.created\",\"user_id\":\"user123\"}");
    
    producer.send(record).get();
    
    // Verify message consumption
    ConsumerRecord<String, String> consumed = 
        consumer.poll(Duration.ofSeconds(10)).iterator().next();
    
    assertEquals("user123", consumed.key());
    assertTrue(consumed.value().contains("user.created"));
}
```

## Deployment and Operations

### Cluster Sizing
```yaml
# Production cluster sizing
production_cluster:
  brokers: 6
  partitions_per_topic: 24
  replication_factor: 3
  storage_per_broker: 2TB
  memory_per_broker: 32GB
  cpu_per_broker: 16 cores
```

### Backup and Recovery
```yaml
# Backup strategy
backup:
  frequency: daily
  retention: 30 days
  storage: S3
  topics:
    - user-events
    - order-events
    - payment-events
```

### Disaster Recovery
```yaml
# DR configuration
disaster_recovery:
  primary_cluster: us-east-1
  secondary_cluster: us-west-2
  replication_factor: 3
  failover_time: 5 minutes
  data_sync: async
```

## Best Practices Summary

### Do's
- ✅ Use descriptive topic names with versioning
- ✅ Plan partition count for future scaling
- ✅ Implement proper error handling and dead letter queues
- ✅ Use schema registry for data validation
- ✅ Monitor key metrics and set up alerts
- ✅ Test thoroughly in staging environment

### Don'ts
- ❌ Don't create too many partitions per broker
- ❌ Don't ignore consumer lag monitoring
- ❌ Don't skip schema evolution planning
- ❌ Don't forget about security configuration
- ❌ Don't underestimate testing complexity

This template ensures comprehensive Kafka specification coverage for modern event-driven applications and stream processing systems.

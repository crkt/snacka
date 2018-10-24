CREATE KEYSPACE IF NOT EXISTS snacka 
    WITH replication = {'class': 'SimpleStrategy',
                        'replication_factor': 1};
        
USE snacka;

CREATE TABLE IF NOT EXISTS message (
    room_id UUID,
    timestamp TIMESTAMP,
    id UUID,
    user_id UUID,
    data text,
    PRIMARY KEY (room_id, timestamp, id)
) WITH CLUSTERING ORDER BY (timestamp ASC)
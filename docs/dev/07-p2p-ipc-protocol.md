---
title: P2P IPC Sidecar Protocol
last_updated: 2026-03-17
tags:
  - p2p
  - ipc
  - networking
---

# P2P IPC Sidecar Protocol

## Overview

The Haskell consensus client communicates with a libp2p sidecar process via
JSON-RPC 2.0 over a Unix domain socket. This architecture separates networking
concerns (gossipsub, QUIC transport, discv5 discovery) from consensus logic.

## Transport

- **Socket**: Unix domain socket at a configurable path (default: `$DATA_DIR/p2p.sock`)
- **Framing**: newline-delimited JSON (one JSON object per line)
- **Encoding**: UTF-8

## JSON-RPC 2.0 Messages

### Haskell → Sidecar (Requests)

#### `publish`

Publish a message to a gossipsub topic.

```json
{
  "jsonrpc": "2.0",
  "method": "publish",
  "params": [{"topic": "beacon_block", "data": "cafebabe..."}],
  "id": 1
}
```

Response: `{"jsonrpc": "2.0", "result": null, "id": 1}`

#### `subscribe`

Subscribe to a gossipsub topic. Incoming messages arrive as `message` notifications.

```json
{
  "jsonrpc": "2.0",
  "method": "subscribe",
  "params": [{"topic": "attestation_0"}],
  "id": 2
}
```

#### `unsubscribe`

Unsubscribe from a gossipsub topic.

```json
{
  "jsonrpc": "2.0",
  "method": "unsubscribe",
  "params": [{"topic": "attestation_0"}],
  "id": 3
}
```

#### `request_by_range`

Request blocks by slot range (for initial sync).

```json
{
  "jsonrpc": "2.0",
  "method": "request_by_range",
  "params": [{"start": 0, "count": 10}],
  "id": 4
}
```

Response: `{"jsonrpc": "2.0", "result": ["cafebabe...", "deadbeef..."], "id": 4}`

#### `request_by_root`

Request blocks by root hashes.

```json
{
  "jsonrpc": "2.0",
  "method": "request_by_root",
  "params": [{"roots": ["aabb...", "ccdd..."]}],
  "id": 5
}
```

#### `peer_count`

Get the current number of connected peers.

```json
{
  "jsonrpc": "2.0",
  "method": "peer_count",
  "params": [],
  "id": 6
}
```

Response: `{"jsonrpc": "2.0", "result": 42, "id": 6}`

#### `stop`

Gracefully shut down the sidecar.

```json
{
  "jsonrpc": "2.0",
  "method": "stop",
  "params": [],
  "id": 7
}
```

### Sidecar → Haskell (Notifications)

#### `message`

A gossipsub message received on a subscribed topic.

```json
{
  "jsonrpc": "2.0",
  "method": "message",
  "params": {"topic": "beacon_block", "data": "cafebabe..."}
}
```

Note: notifications have no `id` field.

## Topics

| Topic | Description |
|-------|-------------|
| `attestation_0` .. `attestation_3` | Per-subnet attestation gossip |
| `aggregation` | Aggregated attestation proofs |
| `beacon_block` | New beacon blocks |

## Data Encoding

All `data` fields use hex-encoded SSZ+zlib-compressed bytes. The Haskell client
encodes via `Network.P2P.Wire.encodeWire` and decodes via `decodeWire`.

## Sidecar Implementation

The sidecar is a standalone Rust binary using `rust-libp2p` with:

- QUIC transport
- Gossipsub protocol
- discv5 peer discovery
- Unix domain socket JSON-RPC server

See `p2p-sidecar/` for the implementation (when available).

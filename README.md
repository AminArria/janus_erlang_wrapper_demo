# Demo: Erlang wrapper for Janus

This is a demo Erlang app to demonstrate the usage of Janus. For further information regarding "why this demo?" I encourage you to read my blog post [Scalable Broadcasting].

The demo will expose the following endpoints:
- http://localhost:8080/janus-client Client connecting directly to Janus for video broadcast
- http://localhost:8080/api-client Client connecting to our Erlang app
- http://localhost:8080/websocket Endpoint to establish a websocket connetion with our Erlang app.

**NOTE:** This demo was tested using Firefox 65 and Chrome 72.

## Requirements
- Erlang/OTP (tested with Erlang/OTP 21)
- Docker Compose

## Running Demo
First turn on Janus

```
make ops
```

Start Erlang app

```
make run
```

To stop Janus

```
make ops_stop
```

## Demo

Both clients, `janus-client` and `api-client`, present the same interface and way to use so everthing explained below applies to both.

### As broadcaster

1. Start session
1. Start handle
1. Make room (This will fill the `Room ID` field)
1. Join as publisher (This will fill the `Feed ID` field)
1. Start broadcast

### As viewer

To join a broadcast you need to fill the `Room ID` and `Feed ID` fields with the values set for the broadcaster.

1. Start session
1. Start handle
1. Join broadcast

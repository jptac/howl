#Howl
Howl is a riak_core and web-socket based message delivery system. The architecture is as follows:

1. The front end is a cowboy web server that accepts web socket connections.
2. The backend is a simple TPC/BERT based protocol.
3. Each riak_core node offers both a front end and a backend, those front and back ends are equal, as in sending messages to any backend will reach any front end.
4. Messages are send to channels, and clients can subscribe to channels.

## Web socket protocol
The following JSON encoded messages can be send to the web socket fronted. Those messages are checked against an authentication mechanism. Received messages are JSON encoded strings.

```
{auth: {'user': user.value, 'pass': pass.value}}
{join: input.value}
```
### Auth
Will authenticate the web socket for permission checking.

### Join
Will join a channel for the websocket.



## TPC protocol
The following TPC messages are known by the protocl. An example implementation can be found in [libhowl](https://github.com/project-fifo/libhowl).

```
{msg, Channel, Msg}
```

### msg
Sends **Msg** to the given **Channel**.
* **Msg** should be in a format that it can be encoded [jsx](https://github.com/talentdeficit/jsx).
* **Channel** should be an binary.

## Authentication
The only authentication mechanism supported right now is via the [Snarl AAA server](https://github.com/project-fifo/snarl).
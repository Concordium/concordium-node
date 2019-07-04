@0xf3deb67e71f473d0;
# using NetworkPacket = import "packet.capnp";
# using import "packet.capnp".NetworkPacket;

struct IpAddrV4 {
    a @0 :UInt8;
    b @1 :UInt8;
    c @2 :UInt8;
    d @3 :UInt8;
}

struct IpAddrV6 {
    a @0 :UInt16;
    b @1 :UInt16;
    c @2 :UInt16;
    d @3 :UInt16;
    e @4 :UInt16;
    f @5 :UInt16;
    g @6 :UInt16;
    h @7 :UInt16;
}


struct IpAddr {
    union {
        v4 @0 :IpAddrV4;
        v6 @1 :IpAddrV6;
    }
}

enum PeerType {
    node @0;
    bootstrapper @1;
}

struct P2PNodeId {
    id @0 :UInt64;
}

struct P2PPeer {
    ip @0 :IpAddr;
    port @1 :UInt16;
    id @2 :P2PNodeId;
    peerType @3 :PeerType;
}

struct NetworkPacketDirect{
    peer @0 :P2PPeer;
    msgId @1 :Data;
    receiver @2 :P2PNodeId;
    networkId @3 :UInt16;
    msg @4 :Data;
}

struct NetworkPacket {
    union {
        direct @0 :NetworkPacketDirect;
        broadcast @1 :Void;
    }
    timestamp @2 :UInt64;
    other @3 :UInt64;
}

struct RequestPing {
    peer @0 :P2PPeer;
}

struct RequestFindNode {
    peer @0 :P2PPeer;
    node @1 :P2PNodeId;
}

struct NetworkRequest {
    union {
        ping @0 :RequestPing;
        findNode @1 :RequestFindNode;
    }
    timestamp @2 :UInt64;
    other @3 :UInt64;
}

struct ResponsePong {
    peer @0 :P2PPeer;
}

struct ResponseFindNode {
    peer @0 :P2PPeer;
    nodes @1 :List(P2PPeer);
}

struct NetworkResponse {
    union {
        pong @0 :ResponsePong;
        findNode @1 :ResponseFindNode;
    }
    timestamp @2 :UInt64;
    other @3 :UInt64;
}

struct NetworkMessage {
    union {
        packet @0 :NetworkPacket;
        request @1 :NetworkRequest;
        response @2 :NetworkResponse;
        unknown @3 :Void;
        invalid @4 :Void;
    }
}

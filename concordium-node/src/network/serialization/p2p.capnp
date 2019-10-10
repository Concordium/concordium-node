@0xf3deb67e71f473d0;

struct P2PNodeId {
    id @0 :UInt64;
}

struct NetworkMessage {
    union {
        packet @0 :NetworkPacket;
        request @1 :NetworkRequest;
        response @2 :NetworkResponse;
        invalid @3 :Void;
    }
}

struct NetworkPacket {
    packetType @0 :PacketType;
    networkId @1 :UInt16;
    message @2 :Data;
    timestamp @3 :UInt64;
    other @4 :UInt64;
}

struct PacketType {
    union {
        direct @0 :P2PNodeId;
        broadcast @1 :List(P2PNodeId);
    }
}

struct RequestFindNode {
    node @0 :P2PNodeId;
}

struct NetworkRequest {
    union {
        ping @0 :Void;
        findNode @1 :RequestFindNode;
    }
    timestamp @2 :UInt64;
    other @3 :UInt64;
}

struct ResponseFindNode {
    nodes @0 :List(P2PNodeId);
}

struct NetworkResponse {
    union {
        pong @0 :Void;
        findNode @1 :ResponseFindNode;
    }
    timestamp @2 :UInt64;
    other @3 :UInt64;
}

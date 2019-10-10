@0xf3deb67e71f473d0;

struct P2PNodeId {
    id @0 :UInt64;
}

struct NetworkMessage {
    timestamp @0 :UInt64;
    union {
        packet @1 :NetworkPacket;
        request @2 :NetworkRequest;
        response @3 :NetworkResponse;
    }
}

struct NetworkPacket {
    packetType @0 :PacketType;
    networkId @1 :UInt16;
    message @2 :Data;
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
}

struct ResponseFindNode {
    nodes @0 :List(P2PNodeId);
}

struct NetworkResponse {
    union {
        pong @0 :Void;
        findNode @1 :ResponseFindNode;
    }
}

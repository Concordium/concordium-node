# Network Layer
This document describes the network layer of the Concordium blockchain.

## Overview 
The network layer consists of a peer-to-peer (P2P) network. Each node in the network is connected to a *set of peers*. The node uses the established connections for various tasks including: *retrieving more peers*, _consensus_, _catching up_ on the chain etc. There exist two *types* of peers in the network, namely the *Node* and the *Bootstrapper* types. The *Node* carries out the tasks mentioned above while the purpose of the latter's task is to solely serve peers.

The peers are orchestrated in a random graph and communicates using a [*gossip-like*](https://en.wikipedia.org/wiki/Gossip_protocol) protocol.

Peers can take part in more than one network at the same time. A *network* groups peers together, thus the node can request to *join* a network, leave a *network* and also request more peers of a certain *network*. Currently only a single *network* is being used.

## Joining a Network
When a node wishes to join a network it starts out by contacting a *bootstrapper*. The *bootstrapper* is a special variant of the *concordium-node* which relays peers (only the ones matching the requested `NetworkId`) but disregards all other types of messages.

The *bootstrapper* sends out a *randomized* list of peers to the requesting node. The node who wishes to join a network will in turn try to connect to these *candidates* received from the *bootstrapper*. 
The *bootstrapper* will only keep peers on its list if the *bootstrapper* and the peer have successfully established a connection (see below).

The node will also send a *GetPeers* message to its connected peers if it still *lacks* peers. In return a peer will send a list of *candidates* to the node, to which the node can try establishing connections. 

It is also possible for a node to be *pinned* to certain other peers. This is a task that is carried out by the node operator when configuring the node. The node will always try to keep a connection to each of the *pinned* peers.

The node operator can via the RPC interface make the node *join* or *leave* a given *network* (specified by the `NetworkId`).

## Connection Establishment
When a node and a peer try to establish a connection, the node and the other peer will perform a 2-part handshake.

The purpose of the *first part* of the handshake is to create a *secure channel* between the two peers. In practice a [*Noise XX*](https://noiseexplorer.com/patterns/XX/) handshake is being carried out by the two peers. This ensures *authenticity* and *confidentiality* for the rest of the communication. Note that currently none of the data sent between the peers is in fact secret and thus it is not a requirement from a security point-of-view that the *secure channel* is in fact being encrypted.

After completing the first part of the handshake, then the two peers will carry out the second part of the handshake. By checking various metadata, the two peers confirm that they are in fact compatible. Specifically the peers check that:

1. The client versions are compatible
2. The wire versions are compatible
3. The peers have *compatible* genesis blocks. *Compatible* in this context means that one peer's list of *genesis blocks* must be a prefix of the other's.

When the *handshake* has been successfully completed then the peers will *promote* each other into their corresponding lists of peers, i.e. the peers are now connected and can communicate with each other, hence the peers can now relay messages between them, carry out *consensus* and *catchup* etc.

## Control Flow
When the node is running, it continously checks for incoming connections and polls for events related to the already established connections. Furthermore the node performs several housekeeping tasks wrt. networking related matters. More specifically the node checks that its current connections are *healthy* i.e., verifying reasonable latency, removing connections of which a handshake is missing etc. If any connections are deemed *unhealthy* then the node will close the connection and remove the peer from its set of peers. Note that depending on the level of *unhealthy* the node will perhaps *soft ban* a peer (more on this below).


If the node has exceeded its maximum amount of peers it will remove a random subset of its peers. Note that the node will never remove any of the *pinned peers*. 
The node then proceeds to check if it should unban any of the *soft banned* peers and does so if that is the case. Note that *soft bans* have a duration of 5 minutes.

Afterwards the node may try to *re-bootstrap* again (if the node did not recently *bootstrap*). Finally if the node still lacks peers then it will send out *GetPeers* messages to its peers.


The node listens on its established connections and processes the received messages accordingly. If the message was *re-broadcastable*, then the node will relay the message to its own peers such that the message will propagate throughout the network. Before processing the message the node checks that it has not yet seen the message before or otherwise the node will not process the *duplicated* message. 

The control flow of the *network layer* can be summarized as follows:

1. Listen for incoming connections
2. Process network messages
3. (Optionally) Carry out housekeeping tasks if a suffcient time period has passed since last time a housekeeping was done.
4. (Optionally) Establish new outgoing connections if required
5. Repeat

## Processing Network Message
When the node receives a network message on one of its connections it will either dispatch the network message to the *consensus layer* or immediately handle the network message if it was only *network related*. An example of the latter could be that the node was asked for peers by the *GetPeers* message, responding to a *Ping*, *Join*-/*Leave* network messages etc. 
However, when the message is for the *consensus layer*, it is first checked that the message was not a *duplicate message* i.e., a message that the node has received before (more on this below). If the message was a *duplicate*, then node will not process the *duplicated* message. 
The node dispatches *new* messages to a *low-priority queue* or a *high-priority queue* depending on the content of the network message. Messages containing *blocks*, *finalization records*, *finalization messages*, *catch-up status'* are treated as high priority, while *transactions* are treated as low priority.
If the message was *re-broadcastable*, then the node will relay the message to its own peers such that the message will propagate throughout the network. 

The node holds *de-duplication* buffers for each of the *rebroadcastable* network packet types; finalization-, transaction-, block-, and finalization record packets. These *de-duplication* buffers contain hashes of previously received messages. Before the node *handles* the actual message (i.e. passing the message through to the *consensus* layer) it is checked that the message is not a duplicate (by computing the hash of the message and checking whether it was contained within the corresponding de-duplication buffer). If the message was a duplicate then the node will simply disregard the duplicated message. 

## Banning
A node holds information if another peer is *permanently banned* or *soft banned* (e.g. a temporary ban could have a duration of 24 hours). 
Permanent banning is carried out by a manual process, thus the operator of the node can put other peers on the permanent ban list by calling the RPC API. Banned peers are identified by their IP address and so the node will not establish connections to such an address.

*Soft banning* is on the other hand an automatic procedure carried out by the node. The node *soft bans* a peer based on the outcome from the *consensus* layer. Reasons include: the peer sends malformed messages; the peer sends messages with *bad* data e.g. an inconsistent *catch-up* message; etc. *Soft bans* are also given by the IP address of the peer and as such the node will immediately disconnect from the peer and avoid establishing new connections to the peer within the *soft ban duration*.

## Connecting to the *Mainnet*
In order to connect to the Concordium *Mainnet* the node should be configurged with the Concordium *bootstrappers*. Currently there are two *bootstrappers* for the Concordium *Mainnet* and they can be reached at `bootstrap.mainnet.concordium.software`. Likewise there are two *bootstrapppers* for the Concordium *Testnet* and they are located at `bootstrap.testnet.concordium.com`.

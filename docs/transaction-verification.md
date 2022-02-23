# Transaction Verification
This document describes the _transaction policy_ for a Concordium Node.
That is, how the node decides to reject or accept transactions.
 
## Overview
Transactions are submitted to a node in the Concordium Network through the GRPC API.
From here the transaction is propagated throughout the network via the peers of the node. 
Transactions can be propagated throughout the network in the following two ways. 
 
1. The node relays transactions as part of a block.
2. The node relays the transactions it individually receives.
 
Before transactions are relayed to the network they undergo a verification process within the node. 
The purpose of the verification process of transactions are the following:
 
1. Prevent congestion of network packets on the network i.e., lowering the network bandwidth.
This is in combination with the `deduplication` functionality of the node described in the [./network-layer.md](network layer document).
2. Preventing the consensus layer from performing unnecessary work.
3. To strengthen the security of the overall network.
 
## Types of transactions
 
The `transactions` of the Concordium Blockchain can be grouped into three categories.
 
1. `CredentialDeployments` When a user creates a new account. This transaction type does not cost the sender any CCD, however there is a limit on how many 
`CredentialDeployments` the chain will accept per block.
2. `ChainUpdates` Special kinds of transactions which can update properties on the running chain. 
The `ChainUpdate` transactions can only be issued by the governance committee.
3. `Transactions` The Concordium Blockchain offers a variety of transactions, common for these are there exists a `sender`.
Transfers where a user wants to send CCD from one account to another is an example hereof. Common for all the `Transaction` types is that the sender pays for 
the inclusion of the transaction.
 
These types of transactions are all subject to a verification process. Naturally the verification process is different 
for each of the types and thus we will now describe what is checked for each type of transaction.
However there is some common verification that is being carried out before the transaction is deserialized into one of the above mentioned types.
 
## Verification
The verification of transactions takes place at two times in the lifetime of a transaction being sent to the node.
The first occurence of verification is when it is initially received by the node.
The second time a transaction is being verified is just before the execution of it in the `scheduler`.
To prevent duplicate work between the `initial` verification and the `pre execution` then the verification result from the 
`initial` verification is stored alongside the transaction in the `transaction table`. The `scheduler` leverages its verification process 
by using the existing verification result when possible. This is especially to avoid duplicating the expensive checks such as signature verification.
Ephemeral properties of transactions, such as `expiry`, balances of accounts etc. are always checked by the `scheduler` before executing the transaction.
 
### Initial verification 
 
The verification process is dependent on the `context` of the transaction. 
As stated above, the node can receive transactions individually or via a block. 
Transactions received as part of a block will be verified in the context of the (alive) parent block if available, 
otherwise verification will be carried out in the context of the `last finalized block`.
Individually received transactions are always verified in the context of the last finalized block.
The reason for this discrepancy is that blocks may arrive out of order - this happens especially when the node is catching up on the chain, 
and as such this heuristic aims to be the 'best guess' for which the transaction should be valid.
 
If the transaction was received individually then it must meet all the criteria stated underneath in order to be taken into consideration for `consensus` 
otherwise it will be rejected.
However if the transaction was received as part of a block then the criterias are 'loosened up' a bit. 
The reason is, when the node is not up to date with the chain then there is a probability that it just might not know about the the actual state of the chain and 
as such the transaction could've become valid further on the chain.
In general transactions depend on other the execution of other transactions and as such it is impossible to say if a transaction is valid or not in some cases without
actually executing the block.
 
Every transaction has an `expiry` associated which is a timestamp indicating when a transaction should no longer be taken into account.
 
Further when a transaction passes the `initial verification` then certain data about the state of the transaction is saved at the time of verification, thus the stored `verification result` is then used to leverage `pre execution` verification.
 
#### Credential Deployment
Credential deployments are checked for the following:
 
1. `expiry` Credential deployments have an expiry associated - not to be confused with the `expiry` of the transaction itself.
Expired credential deployments are always rejected and not being relayed to `consensus`.
2. `duplicate` For each credential deployment there is a `credential id`. Duplicates are always rejected.
3. `existence of the identity provider` Each credential is issued by an identity provider. In order for a credential deployment 
to become a valid account on the chain the issuing identity provider must be registered on the chain. 
Credential deployments issued by unknown identity providers will be rejected if the transaction was received individually however if 
the transaction was received as part of a block then it may be valid for a 'future block' as it might just be the case 
that the node does not know about the block of which the identity provider was added to the chain. 
The transaction will be added to the transaction table and re-verified in `pre execution` verification.
4. `existence of the anonymity revokers` As with the existence of valid identity providers there must also be valid anonymity revokers present in 
the credential deployment when the transaction was received individually, otherwise it will be rejected. But if the transaction was received as part of a 
block then it might be valid as the anonymity revoker(s) could've been added in a future block.
The transaction will be added to the transaction table and re-verified in `pre execution` verification.
5. `cryptographic verification` Finally the signatures along with the `credential proof` on the credential deployment will be verified and if they are invalid then 
it will be rejected and not relayed to `consensus`. This is always the case as identity providers and anonymity revokers have no means 
to update or modify the on-chain key pair.
 
Successfully verified credential deployments will be put into the `Transaction Table` and later be executed by the `Scheduler`.
The `Scheduler` asserts that the credential deployment transaction is valid - by asserting the above requirements - before executing it.
 
#### Chain Update
Chain updates are checked for: 
 
1. `valid effective time` A chain update has an `effective time` and a `timeout` associated. While the latter indicates when the current 
chain update should no longer be taken into consideration and the former is a timestamp of when the chain update should be 'active' from.
It is verified that the timeout is not later than the effective time or is immediate. If this is the case then the transaction will always be rejected. 
2. `disallowing old sequence numbers` Chain updates each have an associated `sequence number`. This number serves as a `nonce`, hence each chain update must have a unique 
`sequence number`. Thus Chain updates with an associated `sequence number` that has already been used are always rejected and not forwarded to `consensus`.
3. `signature verification` A chain update must be correctly signed by the authorized signer(s). If this is not the case i.e., the signature is invalid 
then the transaction is rejected and never passed to `consensus` if the transaction was received individually. 
However if the transaction was part of a block the transaction will not immediately be rejected as the set of authorized keys can be changed over time. 
 
Successfully verified chain updates are put in the transaction table and later executed by the `Scheduler`.
The `Scheduler` is making sure that the above requirements are satisfied.
 
#### Transaction
Transactions are checked for:
 
1. `energy supply` If the sender did not supply enough energy for the cost of the transaction then it will always be rejected.
Note. Only the header cost is taken into account here thus the transaction might still end up in a block with an `OutOfEnergy` failure.
2. `exceeding the block energy limit` If the supplied NRG was beyond the maximum block energy then the transaction will always be rejected.
3. `sender exists` If the sender of the transaction does not exist then the transaction will be rejected if it was received individually. 
However if the transaction was received as part of a block then it's possible that it is valid because the sender account could've been added
to the chain in a later block currently not known to the node.
4. `duplicate nonce` If the nonce present in the transaction has already been used by the sender account then the transaction will always be rejected.
5. `sequential nonce` If the transaction was received individually then it is also checked that the nonce is the next in line. This verification step is not carried out 
immediately when receiving the transaction when it was part of a block since there can be multiple transactions from the same account in a block and as such 
it cannot be rejected since we're verifying against either the parent block or the last finalized one. 
However when verifying a transaction received individually then the `nonce` is verified against the `non finalized transactions` in the transaction table which is an in 
memory transaction store for all transactions not yet finalized. Transactions failing this verification step - that is the ones received individually will be rejected.
6. `sender has enough funds` It is verified that the sender account has enough funds to cover the transfer. If this is not the case then the transaction 
will be rejected if it was received individually otherwise if it was received as part of a block it may be valid in a future block.
7. `correctly signed` The transaction must be correctly signed by the sender i.e., when the transaction was received individually then a valid signature on the transaction
will lead to rejection of the transaction. If the transaction was part of a block it may not rejected right away as users can update the keys associated with an account.
 
### Pre Execution
Transactions which have passed the verification according to the policies described above are being put 
into the transaction table and ultimately ends up at the `Scheduler` which is responsible for updating the block state.
Before executing transactions the `Scheduler` asserts that a transaction is valid.
Some of these verification steps are leveraged by the `initial verification` and as such the initial verification process also relieves the `Scheduler` from having to do some work - in particular it can possibly result in the scheduler from performing avoiding signature verifications which is relatively a rather performance extensive operation compared to the other verification steps.
 
#### Credential Deployments
Before executing a credential deployment the following is always checked:
 
1. `expiry` If the transaction is expired at this point in time then it will not be executed and will be deemed invalid.
2. `uniqueness of registration id` The credential deployment must be unique, in particular the registration id is being checked being unique.
If the registration id is already present on the chain then we deem the transaction invalid.
3. `uniqueness of address` The account address tied to the credential must not clash with existing ones on the chain, thus a clash would deem the transaction invalid.
 
The `Scheduler` then looks if the transaction _successfully_ passed the `initial verification`. If so, the transaction is deemed valid and a new account will be created on the chain.
On the other hand, if the transaction passed the `initial verification`, but at the time it was received it had an unknown identity provider or set of anonymity revokers then the credential deployment will be verified again.
If this results in a successful verification then the account is created on the chain otherwise the transaction is deemed invalid. Nb. Only credential deployments received as part of a block where the identity provider or the anonymity revokers were not deemed valid will be re-verified by the `Scheduler`. Credential deployments received individually have always been deemed valid in the `initial verification` in order to ever reach the point of execution.
 
#### Chain Update
Before executing a chain update the following is always checked:
 
1. `expiry` The transaction must never be expired and if it is (expired) then it will be deemed invalid.
2. `sequential sequence number` It is verified that the sequence number of the transaction is the next one.
If the `sequence number` is not the next one then the transaction is invalid and will not be executed.
 
The `Scheduler` will now use the cached verification result computed in the `initial verification`. 
If the transaction passed the `initial verification` but the signatures could not be validated then it is checked now and based on the result it will be deemed valid or invalid.
 
On the other hand, if the transaction did pass the `initial verification` the cached verification result contains a hash of the `UpdateKeysCollection` used for verifying the signature. It is asserted that this hash corresponds to the current set of authorized keys i.e. `UpdateKeysCollection` registered on chain. If the authorized keys are the same then the transaction is deemed valid. Otherwise we re-verify the signature on the transaction as the authorized keys might have changed since the transaction was first processed, if the 
signature can be verified the transaction is accepted otherwise it is rejected.
 
#### Transaction
Before executing a `normal transaction` the following is always checked:
 
1. `remaining block energy` There must be enough energy left in the current block, if this is not the case then the transaction will be kept for later.
2. `expiry` The transaction may not be expired.
3. `sender exists` The sender of the transaction must exist on the chain. If the sender does not exist then the transaction is deemed invalid.
4. `senders balance` The sender must have enough funds to cover the transfer. If this is not the case then the transaction is deemed invalid.
5. `sequential nonce` The _next_ account nonce of the sender account must match the nonce in the transaction. If not, then
the transaction invalid.
 
If the transaction was received as part of a block and did not pass the `initial verification` then the transaction is re-verified now and based on the results deemed valid or invalid. 
If the transaction was _successfully_ passing the `initial verification` then it is checked that the `AccountInformation` at that point in time matches the current ones registered on chain. This is checked by using a cached hash of the `AccountInformation` used for verifying in the `initial verification` with the current `AccountInformation` of the account. If it turns out the `AccountInformation` match then the transaction is valid. Otherwise we re-verify the signature with the current registered keys. Nb. key(s) might've been added for the account in the meantime, but the old keys are still present and as such it makes sense to re-verify the signature given the current account keys and thus the transaction is either deemed valid or invalid. The `AccountInformation` type holds information of the public keys and the `threshold` for the account.
 

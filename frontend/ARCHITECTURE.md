# App Architecture

The main objective is to guide the voter through all the steps needed to submit their vote onchain.
To that aim, the app is split in stages:

1. Preparing a vote Tx
2. Signing a vote Tx
3. Gather signatures and submit Tx

The signing stage is sufficiently independant that it should be possible to perform it by someone else than the preparation stage, granted the correct signatures are provided of course.
This way, we can easily envision a scenario where the vote Tx is prepared as a group.
But then each individual party can sign the prepared vote with their own setup.

In the following stage details, steps with a working POC are checked already.

## Preparing a vote Tx

This stage requires the following steps:

- [ ] Identify the voter type: CC | DRep | SPO
- [ ] Identify the vote credentials: Single stake key | Script
- [ ] Identify the fee provider: Connected wallet | ?
- [x] Load governance actions and select one
- [ ] Retrieve gov action metadata
- [ ] Help fill the vote rationale, following the relevant CIP standard JSON
- [x] Generate a pretty PDF version of the JSON rationale
- [x] Pin the rationale in permanent storage (IPFS)
- [x] Build the Tx
- [ ] Provide ways to export the Tx and rationale to share with signers

## Signing a vote Tx

Once a Tx is built, signing is the easy part.
There are many ways to do so.
Some people have a custom CLI setup, others can use this app to sign with their browser wallet, like Eternl.
The important part is being able to verify the Tx, meaning:

- [ ] Display the content of the Tx
- [ ] Check that the shared rationale hash corresponds to the one in the Tx
- [x] Sign the Tx with CIP-30
- [ ] Export the Tx signature witness

## Gather signatures and submit Tx

The final stage simply is gathering all signature witnesses from the participating voters, and submit the transaction.

- [ ] Load the vote Tx
- [ ] Load the signature witnesses
- [x] Submit the Tx (CIP-30)

## Additional remarks

If in the preparation step, the voter is simply using their connected wallet and signing with their default stake key, there is no need to split the process in multiple stages.
The voter can directly submit their vote as soon as the Tx is built.

# Cardano Governance Voting Tool

The goal of this project is to make Cardano Governance voting easy, both for SPOs, DReps and CC members.

It should support both VKey and script voters, using any wallet (light, hard, cold, ...).
It should help generating a vote rationale, in the correct JSON metadata format.
It should help storing that metadata on permanent storage (IPFS).
It should help generating pretty PDFs containing the metadata rationales of votes.

## Getting Started (code)

To retrieve the code, clone the repository with `--recursive` (there is a git submodule).
The code of this project is split in two parts, a Python backend, and an Elm frontend.
The backend code lives in the `backend/` folder and similarly for the frontend code in `frontend/`.

Open your terminal inside the `backend/` directory, and start the backend server locally.
Follow the instructions in the backend readme for that.

Then move inside the `frontend/` directory, and compile the Elm frontend app.
Follow the instructions in the frontend readme for that.

Then simply open the app at http://localhost:8000

## Contributions

Contributions are very welcomed, in many shapes, code or not code!
Please follow contribution guidelines for each part (backend/frontend) that you are interested in contributing to.
Usually, the best approach is to open an issue to start the discussion.

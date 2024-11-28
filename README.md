Minimalist server for Cardano governance uses.

First create and modify the `.env` file containing the IPFS node access config.

```
IPFS_RPC_URL=ipfs-rpc.mycompany.org
IPFS_RPC_USER=user
IPFS_RPC_PASSWORD=password
```

Then you can run the python server.
I suggest you use [`uv`](https://docs.astral.sh/uv/) for that, which takes care of all the dependency stuff.

```sh
uv run server.py
```

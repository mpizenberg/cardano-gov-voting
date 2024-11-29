Minimalist server for Cardano governance uses

## Getting Started

First create and modify the `.env` file containing the IPFS node access config.

```
IPFS_RPC_URL=ipfs-rpc.mycompany.org
IPFS_RPC_USER=user
IPFS_RPC_PASSWORD=password
```

Then create a `static/` folder and you can run the python server.
I suggest you use [`uv`](https://docs.astral.sh/uv/) for that, which takes care of all the dependency stuff.

```sh
mkdir static
uv run server.py
```

To trigger the `pretty-gov-pdf` endpoint, you can use a request like this:

```sh
curl -X POST "http://localhost:8000/pretty-gov-pdf" \
      -H "Content-Type: application/json" \
      -d @cf-ikigai-modified.json \
      --output metadata.pdf
```

To trigger the `ipfs-pin` endpoint, you can use a request like this:

```sh
curl -X POST "http://localhost:8000/ipfs-pin" \
     -H "Content-Type: multipart/form-data" \
     -F "file=@some-file.pdf"
```

## Contributions

All sorts of contributions are welcome!
If you are unsure about how to proceed, the best is to start by opening an issue in this GitHub repository.

**Code contributions**

The python project is handled using [`uv`](https://docs.astral.sh/uv/) so please install it first.
To start the server, follow the steps in the "Getting Started" section above.

The code is linted and formatted using [`ruff`](https://docs.astral.sh/ruff/) so please install it too.

```sh
# From inside the backend/ folder:
ruff check           # check the lints
ruff format --check  # check code formatting
```

To avoid having to manually check lints and code format, I suggest you install a ruff extension in your favorite editor.

import json
import logging
import os
import shutil
import base64
import subprocess
import tempfile
from pathlib import Path
from typing import List, Optional, Tuple

import httpx
from pydantic import BaseModel
from dotenv import load_dotenv
from fastapi import Body, FastAPI, HTTPException, UploadFile, Request
from fastapi.responses import HTMLResponse, Response, FileResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from contextlib import asynccontextmanager
from brotli_asgi import BrotliMiddleware

TIMEOUT_SECONDS = 10
MAX_CONCURRENT_REQUESTS = 100

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Load environment variables from .env file
# Add environment variable validation at startup
load_dotenv()
required_vars = ["IPFS_RPC_URL", "IPFS_RPC_USER", "IPFS_RPC_PASSWORD", "NETWORK_ID"]
missing_vars = [var for var in required_vars if not os.getenv(var)]
NETWORK_ID = os.getenv("NETWORK_ID", "0")  # defaults to 0 if not set
if missing_vars:
    logger.warn(f"Missing environment variables: {', '.join(missing_vars)}")
    logger.warn("All IPFS requests will need to provide RPC config or will fail")


# Define an async HTTP client (using httpx) and attach it to the FastAPI app
@asynccontextmanager
async def lifespan(app: FastAPI):
    async with httpx.AsyncClient(
        timeout=httpx.Timeout(TIMEOUT_SECONDS),
        limits=httpx.Limits(max_connections=MAX_CONCURRENT_REQUESTS),
    ) as client:
        app.async_client = client  # pyright: ignore
        yield


# Initialize FastAPI app
app = FastAPI(title="Cardano Gov Voting Server", lifespan=lifespan)

# Add BrotliMiddleware to compress responses
app.add_middleware(BrotliMiddleware)

# Statically served directory (contains the frontend build)
static_dir = Path("../frontend/static")
templates = Jinja2Templates(directory=static_dir)


@app.get("/", response_class=HTMLResponse)
async def read_root(request: Request):
    return templates.TemplateResponse(
        "index.html", {"request": request, "network_id": NETWORK_ID}
    )


@app.get("/page/{full_path:path}", response_class=HTMLResponse)
async def get_page(full_path: str, request: Request):
    return templates.TemplateResponse(
        "index.html", {"request": request, "network_id": NETWORK_ID}
    )


@app.post("/pretty-gov-pdf")
async def create_pretty_pdf(data: dict = Body(...)):
    """
    Generate a pretty PDF from JSON governance data.
    Returns the PDF file.
    """
    try:
        logger.info("Pretty PDF attempt")
        # Create a temporary directory for processing
        with tempfile.TemporaryDirectory() as temp_dir:
            # Write metadata to JSON file
            metadata_file = Path(temp_dir) / "metadata.json"
            with open(metadata_file, "w") as f:
                json.dump(data, f)

            output_path = Path(temp_dir) / "metadata.pdf"

            # Copy template file to temp directory
            template_dest = Path(temp_dir) / "template.typ"
            shutil.copyfile("template.typ", str(template_dest))

            # Change to temp directory and run typst command
            current_dir = os.getcwd()

            try:
                os.chdir(temp_dir)
                # Add more verbose logging
                logger.info(f"Running typst in directory: {temp_dir}")
                logger.info(f"Files in directory: {os.listdir('.')}")

                process = subprocess.run(
                    ["typst", "compile", "template.typ", "metadata.pdf"],
                    capture_output=True,
                    text=True,
                    check=True,
                )

                # Log the process output
                if process.stdout:
                    logger.info(f"Typst output: {process.stdout}")
                if process.stderr:
                    logger.warning(f"Typst stderr: {process.stderr}")

                # Check if the file was actually created
                if not output_path.exists():
                    logger.error("PDF file was not created after typst command")
                    raise HTTPException(
                        status_code=500,
                        detail="PDF generation failed - output file not created",
                    )

                # Log file size to ensure it's not empty
                logger.info(f"Generated PDF size: {output_path.stat().st_size} bytes")

                # Read the file into memory before the temp directory is cleaned up
                pdf_content = output_path.read_bytes()

                return Response(
                    content=pdf_content,
                    media_type="application/pdf",
                    headers={
                        "Content-Disposition": "attachment; filename=metadata.pdf"
                    },
                )

            finally:
                os.chdir(current_dir)

    except subprocess.CalledProcessError as e:
        logger.error(f"PDF generation failed: {e.stderr}")
        raise HTTPException(
            status_code=500, detail=f"PDF generation failed: {e.stderr}"
        )
    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}")
        raise HTTPException(status_code=500, detail="An unexpected error occurred")


class IPFSPinRequest(BaseModel):
    ipfsServer: str | None = None
    headers: List[Tuple[str, str]] | None = None


class IPFSPinJSONRequest(IPFSPinRequest):
    fileName: str
    jsonContent: str


async def pin_to_ipfs_common(
    temp_file_path: Path,
    ipfs_server: Optional[str] = None,
    custom_headers: Optional[List[Tuple[str, str]]] = None,
):
    """Common IPFS pinning logic used by both endpoints."""
    try:
        # Use default IPFS settings if not provided
        server_url = ipfs_server or os.getenv("IPFS_RPC_URL") or ""
        username = os.getenv("IPFS_RPC_USER", "")
        password = os.getenv("IPFS_RPC_PASSWORD", "")
        auth_string = f"{username}:{password}"
        token = base64.b64encode(auth_string.encode("utf-8")).decode("ascii")
        headers = {"Authorization": f"Basic {token}"}

        # Use custom headers if a custom IPFS RPC URL was provided in the request
        if ipfs_server:
            headers = dict(custom_headers or [])

        # Open the file and create the files parameter for the request
        with open(temp_file_path, "rb") as f:
            response = await app.async_client.post(  # type: ignore
                url=f"{server_url}/add",
                headers=headers,
                files={"file": f},
            )
            return Response(
                content=response.content,
                status_code=response.status_code,
                media_type=response.headers.get("content-type"),
            )

    except json.JSONDecodeError as e:
        logger.error(f"Failed to parse IPFS output: {str(e)}")
        raise HTTPException(status_code=500, detail="Failed to parse IPFS response")
    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}")
        raise HTTPException(status_code=500, detail="An unexpected error occurred")


@app.post("/ipfs-pin/file")
async def pin_file_to_ipfs(
    file: UploadFile,
    ipfs_server: Optional[str] = None,
    headers: Optional[List[Tuple[str, str]]] = None,
):
    """Pin a file to IPFS and return the hash."""
    logger.info("IPFS file pin attempt")

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_file_path = Path(temp_dir) / file.filename  # type: ignore
        content = await file.read()

        with open(temp_file_path, "wb") as f:
            f.write(content)

        return await pin_to_ipfs_common(temp_file_path, ipfs_server, headers)


@app.post("/ipfs-pin/json")
async def pin_json_to_ipfs(request: IPFSPinJSONRequest):
    """Pin JSON content to IPFS and return the hash."""
    logger.info("IPFS JSON pin attempt")

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_file_path = Path(temp_dir) / request.fileName

        with open(temp_file_path, "w", encoding="utf-8") as f:
            f.write(request.jsonContent)

        return await pin_to_ipfs_common(
            temp_file_path, request.ipfsServer, request.headers
        )


class ProxyRequest(BaseModel):
    url: str
    method: str = "GET"
    headers: dict = {}
    body: Optional[dict] = None


@app.post("/proxy/json")
async def proxy_request(request: ProxyRequest):
    """
    Proxy an HTTP request to the specified URL and return the response.
    Default "accept" and "content-type" headers for JSON are automatically added.
    """
    try:
        # Default headers for JSON
        default_headers = {
            "accept": "application/json",
            "content-type": "application/json",
        }

        # Make the request
        response = await app.async_client.request(  # pyright: ignore
            method=request.method,
            url=request.url,
            headers={**default_headers, **request.headers},
            json=request.body,
        )

        return Response(
            content=response.content,
            status_code=response.status_code,
            media_type=response.headers.get("content-type"),
        )

    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}")
        raise HTTPException(status_code=500, detail="An unexpected error occurred")


class CompressedStaticFiles(StaticFiles):
    async def get_response(self, path: str, scope):
        """
        Serve pre-compressed `.gz` or `.br` files if available and the client supports it.
        """
        accept_encoding = ""
        for header, value in scope["headers"]:
            if header == b"accept-encoding":
                accept_encoding = value.decode()
                break

        full_path = os.path.join(self.directory, path)  # pyright: ignore

        # Serve Brotli (.br) if supported and available
        if "br" in accept_encoding and os.path.exists(full_path + ".br"):
            return FileResponse(full_path + ".br", headers={"Content-Encoding": "br"})

        # Serve Gzip (.gz) if supported and available
        if "gzip" in accept_encoding and os.path.exists(full_path + ".gz"):
            return FileResponse(full_path + ".gz", headers={"Content-Encoding": "gzip"})

        # Default to the uncompressed version
        return await super().get_response(path, scope)


# Mount static files from static directory
app.mount("/", CompressedStaticFiles(directory=static_dir), name="static")


if __name__ == "__main__":
    import uvicorn

    # Ensure the static directory exists
    if not static_dir.exists():
        logger.warning(
            "static directory does not exist. Static files will not be served."
        )

    # Run the server
    uvicorn.run(
        "server:app",
        host="0.0.0.0",
        port=8000,
        reload=True,
        limit_concurrency=MAX_CONCURRENT_REQUESTS,
    )

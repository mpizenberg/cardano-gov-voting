import json
import logging
import os
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import List, Optional, Tuple

from pydantic import BaseModel
import requests
from dotenv import load_dotenv
from fastapi import Body, FastAPI, HTTPException, UploadFile
from fastapi.responses import HTMLResponse, Response, FileResponse
from fastapi.staticfiles import StaticFiles

# Load environment variables from .env file
# Add environment variable validation at startup
load_dotenv()
required_vars = ["IPFS_RPC_URL", "IPFS_RPC_USER", "IPFS_RPC_PASSWORD"]
missing_vars = [var for var in required_vars if not os.getenv(var)]
if missing_vars:
    raise ValueError(
        f"Missing required environment variables: {', '.join(missing_vars)}"
    )

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize FastAPI app
app = FastAPI(title="Custom Processing Server")

# Statically served directory (contains the frontend build)
static_dir = Path("../frontend/static")


@app.get("/", response_class=HTMLResponse)
async def read_root():
    return FileResponse(static_dir / "index.html")


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
        headers = {
            "Authorization": f"Basic {os.getenv('IPFS_RPC_USER')}:{os.getenv('IPFS_RPC_PASSWORD')}"
        }
        if ipfs_server:
            headers = dict(custom_headers or [])

        # Open the file and create the files parameter for the request
        with open(temp_file_path, "rb") as f:
            response = requests.post(
                url=f"{server_url}/add",
                headers=headers,
                files={"file": f},
            )
            return Response(
                content=response.content,
                status_code=response.status_code,
                media_type=response.headers.get("content-type"),
            )

    except requests.RequestException as e:
        logger.error(f"IPFS request failed: {str(e)}")
        raise HTTPException(status_code=500, detail=f"IPFS request failed: {str(e)}")
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


# Mount static files from static directory
app.mount("/", StaticFiles(directory=static_dir), name="static")


if __name__ == "__main__":
    import uvicorn

    # Ensure the static directory exists
    if not static_dir.exists():
        logger.warning(
            "static directory does not exist. Static files will not be served."
        )

    # Run the server
    uvicorn.run("server:app", host="0.0.0.0", port=8000, reload=True)

import json
import logging
import os
import shutil
import subprocess
import tempfile
from pathlib import Path

import requests
from dotenv import load_dotenv
from fastapi import Body, FastAPI, HTTPException, UploadFile
from fastapi.responses import HTMLResponse, JSONResponse, Response
from fastapi.staticfiles import StaticFiles

# Load environment variables from .env file
# Add environment variable validation at startup
load_dotenv()
required_vars = ['IPFS_RPC_URL', 'IPFS_RPC_USER', 'IPFS_RPC_PASSWORD']
missing_vars = [var for var in required_vars if not os.getenv(var)]
if missing_vars:
    raise ValueError(f"Missing required environment variables: {', '.join(missing_vars)}")

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize FastAPI app
app = FastAPI(title="Custom Processing Server")

# Mount static files from static directory
app.mount("/static", StaticFiles(directory="static"), name="static")


# HTML content remains the same
HTML_CONTENT = """
<!DOCTYPE html>
<html>
<head>
    <title>Processing Server</title>
</head>
<body>
    <h1>Welcome to the Processing Server</h1>
    <div class="container">
        <h2>Available Endpoints:</h2>
        <ul>
            <li><code>/pretty-gov-pdf</code> - Generate pretty PDF from JSON governance data</li>
            <li><code>/ipfs-pin</code> - Pin files to IPFS</li>
        </ul>
    </div>
</body>
</html>
"""

@app.get("/", response_class=HTMLResponse)
async def read_root():
    """Serve the main HTML page."""
    return HTML_CONTENT

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
            with open(metadata_file, 'w') as f:
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
                    check=True
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
                        detail="PDF generation failed - output file not created"
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
                    }
                )

            finally:
                os.chdir(current_dir)

    except subprocess.CalledProcessError as e:
        logger.error(f"PDF generation failed: {e.stderr}")
        raise HTTPException(
            status_code=500,
            detail=f"PDF generation failed: {e.stderr}"
        )
    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail="An unexpected error occurred"
        )

@app.post("/ipfs-pin")
async def pin_to_ipfs(file: UploadFile):
    """
    Pin a file to IPFS and return the hash using HTTP request.
    Accepts any file type and returns the IPFS add command output.
    """
    try:
        logger.info("IPFS pin attempt")
        # Create a temporary directory for processing
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_file_path = Path(temp_dir) / file.filename   # type: ignore

            # Save uploaded file
            content = await file.read()
            with open(temp_file_path, "wb") as f:
                f.write(content)

            # Prepare the HTTP request
            url = f"https://{os.getenv('IPFS_RPC_URL')}/api/v0/add"
            auth = (os.getenv('IPFS_RPC_USER'), os.getenv('IPFS_RPC_PASSWORD'))

            # Open the file and create the files parameter for the request
            with open(temp_file_path, 'rb') as f:
                files = {'file': f}

                # Make the HTTP request
                response = requests.post(
                    url,
                    auth=auth,  # type: ignore
                    files=files
                )

                # Raise an exception for bad status codes
                response.raise_for_status()

                # Parse the JSON response
                ipfs_result = response.json()

            return JSONResponse(content=ipfs_result)


    except subprocess.CalledProcessError as e:
        logger.error(f"IPFS operation failed: {e.stderr}")
        raise HTTPException(
            status_code=500,
            detail=f"IPFS operation failed: {e.stderr}"
        )
    except json.JSONDecodeError as e:
        logger.error(f"Failed to parse IPFS output: {e}")
        raise HTTPException(
            status_code=500,
            detail="Failed to parse IPFS command output"
        )
    except Exception as e:
        logger.error(f"Unexpected error: {str(e)}")
        raise HTTPException(
            status_code=500,
            detail="An unexpected error occurred"
        )

if __name__ == "__main__":
    import uvicorn
    # Ensure the static directory exists
    if not Path("static").exists():
        logger.warning("static/ directory does not exist. Static files will not be served.")

    # Run the server
    uvicorn.run(
        "server:app",
        host="0.0.0.0",
        port=8000,
        reload=True
    )

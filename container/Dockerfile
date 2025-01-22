# Use a Python image with uv pre-installed to build the backend server
FROM ghcr.io/astral-sh/uv:python3.10-alpine AS backend-builder

# Install the server into `/backend`
WORKDIR /backend

# Enable bytecode compilation
ENV UV_COMPILE_BYTECODE=1

# Copy from the cache instead of linking since it's a mounted volume
ENV UV_LINK_MODE=copy

# Install the project's dependencies using the lockfile and settings
RUN --mount=type=cache,target=/root/.cache/uv \
    --mount=type=bind,source=backend/uv.lock,target=uv.lock \
    --mount=type=bind,source=backend/pyproject.toml,target=pyproject.toml \
    uv sync --frozen --no-install-project --no-dev

# Then, add the rest of the project source code and install it
# Installing separately from its dependencies allows optimal layer caching
ADD backend .
RUN --mount=type=cache,target=/root/.cache/uv \
    uv sync --frozen --no-dev

# ---------------------------------------------------------

# Use a Node image with npm pre-installed to build the frontend Elm app
FROM node:lts AS frontend-builder

# Install the dev dependencies, Elm, etc.
WORKDIR /frontend
RUN --mount=type=bind,source=frontend/package.json,target=package.json \
    --mount=type=bind,source=frontend/package-lock.json,target=package-lock.json \
    npm ci

# Then add the code and build the frontend app
ADD frontend .
RUN npm run make

# ---------------------------------------------------------

# Then use the final image for the server without uv
# and with the compiled frontend app into /frontend/static/
FROM python:3.10-alpine
# It is important to use the image that matches the builder,
# as the path to the Python executable must be the same.

# Copy the python server code
COPY --from=backend-builder --chown=backend:backend /backend/.venv /backend/.venv
COPY --from=backend-builder /backend/server.py /backend/server.py

# Copy the compiled frontend code
COPY --from=frontend-builder /frontend/static /frontend/static

# Run the FastAPI server
# Uses `--host 0.0.0.0` to allow access from outside the container
WORKDIR /backend
CMD [".venv/bin/fastapi", "run", "--host", "0.0.0.0", "server.py"]

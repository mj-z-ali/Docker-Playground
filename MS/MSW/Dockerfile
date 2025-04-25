# ---------- builder ----------

FROM ubuntu:latest AS build

# Set default shell to bash instead of /bin/sh [Dash] to prevent writing RUN /bin/bash -c
SHELL ["/bin/bash", "-c"]
# Prevent interactive prompts during package installs
ENV DEBIAN_FRONTEND=noninteractive
# Save cabal and ghcup paths to prevent writing source ~/.ghcup/env every time
ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:${PATH}"

# Update and install dependencies
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    libffi-dev \
    libgmp-dev \
    libtinfo-dev \
    libncurses-dev \
    libtool \
    autoconf \
    xz-utils \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install GHCup (manages GHC, Cabal, etc.)
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash -s -- --non-interactive

# Source ghcup env and verify installations
RUN ghcup install ghc recommended && ghcup set ghc recommended && \
    ghcup install cabal recommended && ghcup set cabal recommended

COPY . .

RUN set -euo pipefail                                       && \
    source ~/.ghcup/env                                     && \
    cabal configure --enable-executable-static --disable-shared && \
    cabal build exe:mergesortwork                         && \
    mkdir -p /out                                           && \
    cp "$(cabal list-bin exe:mergesortwork)" /out/mergesortwork 
# ---------- runtime ----------

# FROM gcr.io/distroless/static
FROM busybox:latest

COPY --from=build /out/mergesortwork   /usr/local/bin/

WORKDIR /src

COPY entrypoint.sh /entrypoint

RUN mkdir /start /done && chmod +x /entrypoint

ENTRYPOINT ["/entrypoint"]




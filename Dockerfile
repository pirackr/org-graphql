# syntax=docker/dockerfile:1

FROM fpco/stack-build:lts-23.16 AS build
WORKDIR /app

COPY stack.yaml stack.yaml.lock org-graphql.cabal /app/
RUN stack --no-nix setup
RUN stack --no-nix build --only-dependencies

COPY . /app
RUN stack --no-nix build --copy-bins --local-bin-path /app/bin

FROM debian:bookworm-slim
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 \
    zlib1g \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /app/bin/org-graphql-exe /usr/local/bin/org-graphql

ENV ORG_BACKEND_ORG_DIR=/data
EXPOSE 8080
VOLUME ["/data"]

USER 65532:65532
ENTRYPOINT ["org-graphql"]

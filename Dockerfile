FROM registry.gitlab.com/mopsa/mopsa-analyzer:1.2 AS build

WORKDIR /home/opam

# Install system deps
USER root
RUN apt-get update && apt-get install -y libev-dev libssl-dev

# Install deps
USER mopsa
ADD aita-server.opam aita-server.opam
RUN opam update && opam install . --deps-only -y

# Build
ADD . .
RUN opam exec -- dune build

ENTRYPOINT ["/home/opam/_build/default/bin/main.exe"]

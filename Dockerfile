# ------------------------------------------------------------------------
FROM fpco/stack-build-small:lts-19.11 as build-dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

RUN apt-get update
  
# Docker build should not use cached layer if any of these is modified
COPY stack.yaml pgenie-client-app.cabal stack.yaml.lock /opt/build/
RUN stack build --dependencies-only

# ------------------------------------------------------------------------
FROM build-dependencies as build-app

# Copy compiled dependencies from previous stage
COPY --from=build-dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build

RUN mv "$(stack path --local-install-root)/bin" /opt/build/bin

# -----------------------------------------------------------------------
FROM ubuntu:18.04 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

RUN apt-get update && apt-get install -y \
  libgmp-dev

COPY --from=build-app /opt/build/bin .

CMD ["/opt/app/pgn"]

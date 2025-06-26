FROM haskell:9.4 AS builder

WORKDIR /build

# Copy the project configuration
COPY *.cabal /build/
COPY cabal.project /build/

# Install dependencies. Hopefully cached by Docker if the .cabal file doesn't change.
RUN cabal update
RUN cabal build --only-dependencies

COPY . /build/

RUN cabal build -O2

FROM debian:stable-slim

COPY --from=builder /build/dist-newstyle/build/x86_64-linux/ghc-9.4.*/convex-schema-parser-*/x/convex-schema-parser/opt/build/convex-schema-parser /usr/local/bin/

ENTRYPOINT ["convex-schema-parser"]

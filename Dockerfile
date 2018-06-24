# Run stack build --copy-bins before building and running this image.
FROM ubuntu:18.04

RUN apt-get update && apt-get install -y libgmp10 netbase ca-certificates

COPY .stack-work/docker/_home/.local/bin/ ./

ENTRYPOINT ["./haskell-classifieds-notifications"]

FROM alpine:edge

RUN apk update && apk add \
    alpine-sdk      \
    git             \
    ca-certificates \
    ghc             \
    gmp-dev         \
    zlib-dev        \
    shadow          \
    xz

RUN mkdir -p /opt/hcn/

WORKDIR /opt/hcn

COPY . .
RUN ls

RUN chmod +x stack-1.7.1-x86_64-unofficial-fully-static-musl
RUN chmod 755 stack-1.7.1-x86_64-unofficial-fully-static-musl

RUN ./stack-1.7.1-x86_64-unofficial-fully-static-musl build --copy-bins --ghc-options="-fPIC -fllvm"

ENTRYPOINT ["ls"]


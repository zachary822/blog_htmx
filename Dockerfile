FROM haskell:9.8-slim

WORKDIR /build

RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/root/.local/cabal \
    cabal update

COPY CHANGELOG.md ./
COPY *.cabal *.project ./

COPY . .

RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/root/.local/cabal \
    cabal build

RUN --mount=type=cache,target=/root/.cabal \
    --mount=type=cache,target=/root/.local/cabal \
    cabal install --install-method=copy --installdir=/root/.local/bin

CMD haskell-blog

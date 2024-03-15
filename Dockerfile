FROM haskell:9.8-slim

WORKDIR /build

RUN cabal update

COPY CHANGELOG.md ./
COPY *.cabal *.project ./

RUN cabal build --dependencies-only

COPY . .

RUN cabal build

CMD cabal run

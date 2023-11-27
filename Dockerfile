FROM haskell:9.6-slim

WORKDIR /build

RUN cabal update

COPY CHANGELOG.md ./
COPY *.cabal *.project ./

RUN cabal build --dependencies-only

COPY . .

RUN cabal build

CMD cabal run

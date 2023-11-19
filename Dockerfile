FROM haskell:9.6-slim

WORKDIR /build

RUN cabal update

COPY *.cabal cabal.project ./
COPY CHANGELOG.md ./

RUN cabal build --dependencies-only

COPY . .

RUN cabal build

CMD cabal run

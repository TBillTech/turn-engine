# turn-engine
Backbone logic for computing GameState, legal moves, and VisibleGameState per player. Also contains trainers for AI players.

## Setup

See [SETUP_GUIDE.md](SETUP_GUIDE.md) for Cabal + `hspec-discover` setup instructions (including Windows and VS Code notes).

Quick commands:

- `cabal build all`
- `cabal test`
- `cabal run turn-engine`
- `cabal run turn-engine -- --version`
- `cabal run turn-engine -- --test-example <request-file> <response-file>`
- `./scripts/package-release.ps1`

Protocol examples for the stdio service boundary are documented in [docs/service-protocol.md](docs/service-protocol.md).

Release packaging is handled by [scripts/package-release.ps1](scripts/package-release.ps1). This is intentionally a repository script instead of a Cabal target: Cabal builds components and source distributions, but it does not provide a built-in target for packaging a Windows executable plus the `docs` directory into a zip file.

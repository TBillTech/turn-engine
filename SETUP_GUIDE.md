# Setup Guide (Cabal + Hspec)

This repo is a standard Cabal project with:

- a library (`src/`)
- an executable (`app/`)
- an Hspec test suite using `hspec-discover` (`test/`)

The project uses `default-language: GHC2024` in its Cabal file, so you need a GHC that supports the GHC2024 language edition (**GHC >= 9.10**).

You also need a recent enough `cabal-install` that understands the `GHC2024` language edition (in practice, **cabal-install >= 3.16** is a safe baseline).

If you attempt to build with an older compiler (for example GHC 9.6.x), Cabal will fail with an error like:

> conflict: requires GHC2024 which is not supported

## Install the Haskell toolchain (GHC + Cabal + HLS)

If you don’t already have a Haskell toolchain installed, the simplest, most “official” path is **GHCup**.

- GHCup (recommended): https://www.haskell.org/ghcup/

GHCup can install and manage:

- **GHC** (the compiler)
- **Cabal** (the build tool used by this repo)
- **HLS** (Haskell Language Server, for editor features)

Minimum versions for this repo:

- GHC: **9.10 or newer** (for `GHC2024`)
- cabal-install: **3.16 or newer** (so `GHC2024` is understood)

### Windows (recommended)

1. Go to https://www.haskell.org/ghcup/
2. Use the Windows installer / instructions there.
3. During install, accept prompts to set up the Windows toolchain environment (GHC on Windows typically relies on an MSYS2-based environment).
4. Go to https://www.haskell.org/ghcup/install/ and apply the Window section enviornment variables (most is done automatically, but the MSYS2 var might be missing).
5. Reboot computer to load new env variables.
6. Open a **new** PowerShell window and confirm the tools are on PATH:

   - `ghcup --version`
   - `ghc --version`
   - `cabal --version`

7. Ensure you have a compatible compiler installed via GHCup (GHC **9.10+**).

   If `ghc --version` shows an older version, install and select a newer GHC (choose a version that GHCup offers that is >= 9.10):

   - `ghcup list -t ghc`
   - `ghcup install ghc 9.10.1` (example)
   - `ghcup set ghc 9.10.1` (example)

   Then re-check:

   - `ghc --version`
8. Install HLS via GHCup (optional but recommended for VS Code):

   - `ghcup install hls`

9. Ensure Cabal itself is new enough (cabal-install **3.16+**). For example:

  - `ghcup install cabal 3.16.1.0` (example)
  - `ghcup set cabal 3.16.1.0` (example)
  - `cabal --version`

If `cabal` (or `ghc`) is “not recognized”, it’s almost always one of:

- You need to open a new terminal after installation.
- The GHCup `bin` directory wasn’t added to `PATH`.

Quick checks:

- `where.exe ghcup`
- `where.exe cabal`
- `where.exe ghc`

### macOS / Linux

Use the platform-specific instructions on https://www.haskell.org/ghcup/ to install GHCup, then verify:

- `ghc --version`
- `cabal --version`

## After installing: verify Cabal can build this repo

Once `cabal` is available, go back to this repo root and run:

- `cabal update`
- `cabal build all`
- `cabal test`

## Quick start (all platforms)

From the repo root:

- Update package index: `cabal update`
- Build everything: `cabal build all`
- Run tests: `cabal test`
- Run the executable: `cabal run turn-engine`

Useful during development:

- Open a REPL for the library: `cabal repl`
- Open a REPL for the test suite: `cabal repl turn-engine-test`
- Clean build artifacts (rarely needed): `cabal clean`

## How tests work (`hspec-discover`)

This project uses `hspec-discover` so you don’t need to manually maintain a central test runner.

Where it’s configured:

- `test/Spec.hs` contains:
  - `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}`
  - This tells GHC to run `hspec-discover` as a preprocessor.
- The Cabal test stanza includes:
  - `build-depends: hspec, hspec-discover, QuickCheck, ...`
  - `build-tool-depends: hspec-discover:hspec-discover`

How to add a new test module:

1. Create a new file under `test/` named `SomethingSpec.hs`.
2. Export `spec :: Spec`.
3. Put your tests in `spec`.

Example skeleton:

```haskell
module SomethingSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "..." $ do
    it "..." $ do
      True `shouldBe` True
```

Then `cabal test` will pick it up automatically.

## Windows notes

After you’ve installed the toolchain via the steps in “Install the Haskell toolchain (GHC + Cabal + HLS)” above, these tips tend to make Cabal projects smoother on Windows:

- Prefer Windows PowerShell or Windows Terminal.
- Avoid paths with unusual characters for your Cabal store and repo path if you encounter toolchain issues.
- If compilation fails with missing C toolchain components, re-run GHCup and ensure the Windows/MSYS2 toolchain option is installed.

If you hit dependency solver issues:

- Double-check your GHC version is new enough for `GHC2024`.
- This repo’s `cabal.project` includes an `allow-newer` section intended to help on newer compilers by relaxing some boot library bounds. If you intentionally use an older/stable compiler, you may be able to remove those relaxations (or put overrides in a local `cabal.project.local`)—but prefer staying on a compatible GHC.

## VS Code recommendations

### Extensions

These are the most useful extensions for a Cabal + Haskell workflow:

- **Haskell** (`haskell.haskell`)
  - Provides syntax highlighting, integrates with HLS, and drives editor features.
- **EditorConfig for VS Code** (`EditorConfig.EditorConfig`) (optional)
  - Useful if the repo later adds an `.editorconfig`.

If you use git heavily inside VS Code, you may also like:

- **GitLens** (`eamodio.gitlens`) (optional)

### Haskell Language Server (HLS)

For the best VS Code experience, install HLS via GHCup:

- `ghcup install hls`
- `ghcup set hls` (if you have multiple versions)

Then open the workspace in VS Code; the Haskell extension should detect HLS.

Note: On Windows, HLS binaries are only available for some GHC versions. If GHCup warns that there is no matching HLS binary for your chosen GHC, either:

- switch to a GHC version that GHCup tags as `hls-powered` (from `ghcup list -t ghc`), or
- let GHCup compile HLS for your compiler (see the message GHCup prints for a suggested `ghcup compile hls ...` command).

### Useful commands

- Format (if configured by your tooling): “Format Document”
- Load a REPL in-terminal: `cabal repl`
- Run a single test target: `cabal test turn-engine-test`

## Troubleshooting

### Cabal says “package list does not exist” / “unknown package: relude”

Symptoms often look like:

- `Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal update'...`
- `unknown package: relude`

Checklist:

1. Confirm you are running the expected tools:

  - `ghc --version`
  - `cabal --version`
  - `cabal path`

2. Run `cabal update`.

3. If it still can’t see packages, check whether you have a custom Cabal directory configured (common on Windows when moving the Cabal store off `%LOCALAPPDATA%`):

  - `echo $env:CABAL_DIR`

  If `CABAL_DIR` is set, Cabal will use that folder for its package index/cache.

  In that case, check the Hackage cache directory shown by `cabal path` (look at `remote-repo-cache`) and verify it contains generated index files like:

  - `01-index.cache`
  - `01-index.tar.idx`

4. The simplest fix is to temporarily unset `CABAL_DIR` and rebuild the index in the default location:

  - `Remove-Item Env:CABAL_DIR` (PowerShell)
  - `cabal update`

  If you want to keep using a custom `CABAL_DIR`, creating a fresh directory for it can also help (then rerun `cabal update`).

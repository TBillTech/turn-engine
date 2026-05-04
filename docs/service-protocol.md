# Service Protocol

## Running the Executable

Assume the built executable is named `turn-engine` and lives in a sibling directory next to `docs`, for example `../turn-engine/turn-engine.exe` when invoked from inside `docs` on Windows.

From the repository root, you can run the executable directly:

```powershell
.\turn-engine\turn-engine.exe --service
```

From inside the `docs` directory, run the same executable via a relative path:

```powershell
..\turn-engine\turn-engine.exe --service
```

`--service` starts the line-delimited JSON protocol over standard input and standard output. Send one JSON request object per line on `stdin`, and read one JSON response object per line from `stdout`.

The executable also exposes non-service helper modes that are useful while developing the protocol:

- `..\turn-engine\turn-engine.exe --version`: print the executable version and exit.
- `..\turn-engine\turn-engine.exe --test-example <request-file> <response-file>`: deserialize the request example, run it through the engine, and fail if the expected response file does not deserialize or does not match the engine output.
- `..\turn-engine\turn-engine.exe --example CursedTreasure`: print the full built-in example transcript.
- `..\turn-engine\turn-engine.exe --example CursedTreasure summary`: print the summary text for the example state.
- `..\turn-engine\turn-engine.exe --example CursedTreasure request 3`: print a specific example request line.
- `..\turn-engine\turn-engine.exe --example CursedTreasure response 3`: print the matching example response line.
- `..\turn-engine\turn-engine.exe --dumbplay 100 CursedTreasure`: run repeated AI-only games and print aggregate stats.

If the executable is not on your `PATH`, always invoke it by relative or absolute path as shown above.

## Generic Service Protocol

The service transport is line-delimited JSON over standard I/O.

- Unity writes exactly one JSON request object per line to `stdin`.
- The Haskell service writes exactly one JSON response object per line to `stdout`.
- Invalid JSON is returned as a `serviceError` response.

The top-level service envelope is explicit and regular:

- Every request has a `request` field.
- Every response has a `response` field.
- `gameState`, `playerViews`, and most `playerMove` payloads are engine-owned values that Unity can deserialize, cache, and send back unchanged.

### Generic Game Loop

1. Client calls `getGameSetupPlayers`.
2. Client chooses which game to play, such as `CursedTreasure`, filters out the other rulesets, restricts the `playerTemplates` list to the colors chosen by the players, and updates the selected Player Descriptions with player names and any `playerAI` annotation the client wishes to track.
3. Client passes the restricted and completed list of Player Descriptions to `createNewGame`.
4. Client receives the authoritative initial `gameState`, plus a list of censored `playerViews` containing only the information safe to show the respective users.
5. If the authoritative `gameState` is in the game-over state, the client displays Game Over, shows any interesting scores or statistics, and ends the workflow.
6. Otherwise, the client requests the list of available moves from `enumerateActivePlayerOptions` using the authoritative `gameState` as the argument.
7. Client displays the current `gameState` and the returned move options to the user.
8. User chooses one move option.
9. Client passes the chosen move and the authoritative `gameState` to `makeMove`, and receives a new authoritative `gameState` plus a fresh list of censored `playerViews` safe to show the respective users.
10. Client returns to step 5.

As an alternative, the client may choose not to show the `gameState` and enumerated move list to a human user. Instead, after step 6, the client may call `heuristicHint`, passing the authoritative `gameState` together with the list of legal `playerMoves`, and receive back a ranked list of candidate moves. The client may then submit the best-ranked move at step 9. This supports rudimentary AI bot players for early development and testing.

### API Guide

#### `getGameSetupPlayers`

Use this entry point to discover which rulesets the service currently exposes and which default player templates belong to each ruleset.

- Request shape: a request object with `"request":"getGameSetupPlayers"`.
- Primary use: bootstrap the game setup screen.
- Success response: `gameSetupPlayers`.
- Returned data: a `rulesets` array. Each ruleset includes a human-readable name and a `playerTemplates` list.
- Client expectation: the client chooses one ruleset, discards the others, and edits only the selected ruleset's player descriptions before creating a new game.

#### `createNewGame`

Use this entry point to start a fresh game for one chosen ruleset.

- Request shape: `"request":"createNewGame"` with a `players` list and a `randomSeed`.
- Primary use: convert edited setup data into the first authoritative engine state.
- Success response: `newGameCreated` with `"status":"ok"`.
- Returned data on success: the authoritative `gameState` and a parallel `playerViews` list of censored states safe for the respective users.
- Error response: `newGameCreated` with `"status":"error"` and a `message`.
- Client expectation: on success, cache the authoritative `gameState` as the source of truth and distribute only the corresponding censored `playerViews` to individual players.

#### `enumerateActivePlayerOptions`

Use this entry point to ask the engine which moves are legal for the current authoritative state.

- Request shape: `"request":"enumerateActivePlayerOptions"` with the authoritative `gameState`.
- Primary use: populate the current turn's action menu.
- Success response: `activePlayerOptions`.
- Returned data: `playerMoves`, a list of legal moves for the active player in the supplied state.
- Client expectation: do not synthesize legal moves on the client. Treat this list as the authoritative set of actions the UI may present or an AI may choose from.

#### `makeMove`

Use this entry point to apply one legal move to the authoritative game state.

- Request shape: `"request":"makeMove"` with the authoritative `gameState` and one selected `playerMove`.
- Primary use: advance the game by one engine-approved action.
- Success response: `moveApplied`.
- Returned data: the next authoritative `gameState` and a fresh `playerViews` list safe for the respective users.
- Client expectation: replace the previous cached state with the returned one. The engine is authoritative; the client should not patch forward state locally.

#### `heuristicHint`

Use this entry point to ask the engine for a ranked recommendation over an already-enumerated move list.

- Request shape: `"request":"heuristicHint"` with a difficulty or search `level`, the authoritative `gameState`, and the list of legal `playerMoves`.
- Primary use: bot play, auto-play assistance, or development-time diagnostics.
- Success response: `hintGenerated`.
- Returned data: `hints`, a ranked list of candidate moves with scores.
- Client expectation: call `enumerateActivePlayerOptions` first, then pass that legal move list into `heuristicHint`. The client may then submit one suggested move through `makeMove`.

#### `summary`

Use this entry point to request a textual summary of a supplied authoritative game state.

- Request shape: `"request":"summary"` with the authoritative `gameState`.
- Primary use: debugging, transcript generation, searchable logs, or lightweight observer output.
- Success response: `summaryGenerated`.
- Returned data: `summary`, a text rendering of the game state from the engine.
- Client expectation: this is auxiliary output, not a substitute for the structured `gameState` contract.

#### `serviceError`

This is the generic failure response when the service cannot parse or accept a request at the protocol layer.

- Typical trigger: invalid JSON or a malformed top-level request envelope.
- Response shape: `"response":"serviceError"` with a `message`.
- Client expectation: treat this as a protocol failure, log the message, and do not assume any state transition occurred.

## CursedTreasure Service Protocol

The files in [docs/jsonexamples/CursedTreasure](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure) are concrete request and response examples generated from the transcript-backed example API.

### Setup and Game Creation

- [docs/jsonexamples/CursedTreasure/get-game-setup-players.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/get-game-setup-players.request.json): Minimal `getGameSetupPlayers` request.
- [docs/jsonexamples/CursedTreasure/get-game-setup-players.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/get-game-setup-players.response.json): Available rulesets and default player templates, including the full CursedTreasure color roster.
- [docs/jsonexamples/CursedTreasure/create-new-game.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/create-new-game.request.json): `createNewGame` request with a restricted player list and explicit random seed.
- [docs/jsonexamples/CursedTreasure/create-new-game.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/create-new-game.response.json): Successful game creation, including the authoritative initial `gameState` and per-player censored `playerViews`.

### Early Turn Enumeration

- [docs/jsonexamples/CursedTreasure/enumerate-first.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-first.request.json): First `enumerateActivePlayerOptions` request after game creation.
- [docs/jsonexamples/CursedTreasure/enumerate-first.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-first.response.json): Initial legal move list for the active player.
- [docs/jsonexamples/CursedTreasure/enumerate-before-jeep-move.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-jeep-move.request.json): Enumeration immediately before a later jeep movement example.
- [docs/jsonexamples/CursedTreasure/enumerate-before-jeep-move.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-jeep-move.response.json): Legal jeep destinations for that turn.
- [docs/jsonexamples/CursedTreasure/make-move-jeep.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-jeep.request.json): `makeMove` request for a concrete `MoveJeep` action.
- [docs/jsonexamples/CursedTreasure/make-move-jeep.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-jeep.response.json): Updated authoritative state and censored views after the jeep move resolves.

### Clue Play

- [docs/jsonexamples/CursedTreasure/enumerate-before-clue-play.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-clue-play.request.json): Enumeration request taken at a point where clue-play options are legal.
- [docs/jsonexamples/CursedTreasure/enumerate-before-clue-play.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-clue-play.response.json): Mixed move list showing pass, exchange, clue-play, and jeep options together.
- [docs/jsonexamples/CursedTreasure/make-move-clue-play.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-clue-play.request.json): `makeMove` request for a concrete `PlayClue` action.
- [docs/jsonexamples/CursedTreasure/make-move-clue-play.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-clue-play.response.json): Post-clue state showing how treasure boards, tokens, and turn state advance.

### Raise Treasure

- [docs/jsonexamples/CursedTreasure/enumerate-before-raise-treasure.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-raise-treasure.request.json): Enumeration request immediately before a `RaiseTreasure` move becomes relevant.
- [docs/jsonexamples/CursedTreasure/enumerate-before-raise-treasure.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-raise-treasure.response.json): Legal move list including a `RaiseTreasure` option.
- [docs/jsonexamples/CursedTreasure/make-move-raise-treasure.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-raise-treasure.request.json): `makeMove` request for the `RaiseTreasure` action.
- [docs/jsonexamples/CursedTreasure/make-move-raise-treasure.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-raise-treasure.response.json): Resulting state showing the raising-treasure workflow and chest state initialization.

### Curse Handling

- [docs/jsonexamples/CursedTreasure/enumerate-before-accept-curse.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-accept-curse.request.json): Enumeration request immediately before the curse-handling decision point.
- [docs/jsonexamples/CursedTreasure/enumerate-before-accept-curse.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-accept-curse.response.json): Legal move list including `RaisingTreasureAcceptCurse`.
- [docs/jsonexamples/CursedTreasure/make-move-accept-curse.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-accept-curse.request.json): `makeMove` request for the actual `RaisingTreasureAcceptCurse` move tag used by the engine.
- [docs/jsonexamples/CursedTreasure/make-move-accept-curse.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-accept-curse.response.json): Resulting state after the curse is accepted.
- [docs/jsonexamples/CursedTreasure/enumerate-before-ward-curse.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-ward-curse.request.json): Alias copy using the requested “ward curse” wording for the same decision point.
- [docs/jsonexamples/CursedTreasure/enumerate-before-ward-curse.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-ward-curse.response.json): Alias copy of the same response example.
- [docs/jsonexamples/CursedTreasure/make-move-ward-curse.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-ward-curse.request.json): Alias copy of the accept-curse request, named with the requested terminology.
- [docs/jsonexamples/CursedTreasure/make-move-ward-curse.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-ward-curse.response.json): Alias copy of the accept-curse response.

### Taking Treasure

- [docs/jsonexamples/CursedTreasure/enumerate-before-take-treasure.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-take-treasure.request.json): Enumeration request immediately before a `RaisingTreasureTake` decision.
- [docs/jsonexamples/CursedTreasure/enumerate-before-take-treasure.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/enumerate-before-take-treasure.response.json): Legal move list showing treasure-taking among the available raise-treasure actions.
- [docs/jsonexamples/CursedTreasure/make-move-take-treasure.request.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-take-treasure.request.json): `makeMove` request for `RaisingTreasureTake`.
- [docs/jsonexamples/CursedTreasure/make-move-take-treasure.response.json](d:/code/haskell/turn-engine/docs/jsonexamples/CursedTreasure/make-move-take-treasure.response.json): Resulting state after a treasure is taken.
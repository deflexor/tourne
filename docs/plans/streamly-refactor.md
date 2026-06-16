# Streamly refactor plan

Status: **completed**. All 6 commits done.

| Commit | SHA | Description |
|---|---|---|
| 1 | `dc49e4e` | Streamly deps + 5 reference tests |
| 2 | `8c088d2` | StreamHandle shim + `openStream'` |
| 3 | `37947bd` | `peStream` field in PlaybackEnv |
| 4 | `f78a5cb` | `decodeLoopStream` function |
| 5 | `10230b1` | Wire into `startPlayback` |
| 6 | `f38fcf7` | Remove all legacy TChan/forkIO/StreamHandle code |

Final: 70/70 tests pass. `Tourne.Audio.Stream` exports only `openStream'` + `Trace`.
`PlaybackEnv` has `peStream` (Stream IO ByteString) instead of `peStreamHandle`.
Decode loop uses `S.foldBreak Fold.one` for pull-based iteration.

## Goals

Replace the `TChan ByteString` + `forkIO` + `IORef Bool`
(cancel) plumbing in `Tourne.Audio.Stream` with a
pull-based `Streamly.Data.Stream.Prelude.Stream IO ByteString`.
Benefits:

1. **Automatic finalisation.** When the consumer stops
   iterating, the HTTP response is closed (either explicitly
   in the EOF step, or via GC of the `Response` value). No
   more `cancelVar`, no more fork-join race conditions.
2. **Pull-based semantics.** The decode loop becomes a
   natural consumer: each `unfoldrM` step is one body read.
   The 32 KB batching we did manually is replaceable by a
   stream combinator (`chunksOf` or similar).
3. **Simpler testing.** `Tourne.Test.Decoder` already builds
   a `Stream IO ByteString` shape manually; the production
   code can be tested by injecting pre-recorded byte streams
   instead of going through HTTP.

## Constraints

- All existing tests must pass at every commit boundary
  (no broken `main` between commits)
- The `StreamHandle`/`drainStreamChunks`/`readStreamChunk`
  API is consumed by `Player.hs`, `State.hs`, and `Decoder.hs`
  tests. Plan for backwards-compat shims during the transition
- Don't reintroduce the `withRunInIO` cross-thread trap:
  the audio cmdThread iterates the stream, no fork needed
- ICY metadata stripping must stay
- The station-switch bug ("speed faster / frames skipped")
  should ideally be fixed as a side effect of the cleanup,
  but is not a hard requirement of this refactor

## Commit-by-commit plan

### Commit 1 — `streamly` deps + reference tests
**Already landed: `dc49e4e`.** Adds `streamly` + `streamly-core`
to library and test deps. Adds `Tourne.Test.Streamly` with
5 tests covering `unfoldrM`, `before` (per-iteration, not
once), and pull semantics. 67/67 tests pass.

### Commit 2 — Helper test infrastructure: a stream-backed
`StreamHandle` shim

Goal: let existing Player/State/Decoder code keep working
against the new Stream-based `Stream.openStream` while we
migrate the consumers. This commit:

- Adds `internal/Tourne/Audio/Stream/Shim.hs` that converts
  a `Stream IO ByteString` into the legacy `StreamHandle`
  shape (TChan + IORef) for the transition period
- Updates `Tourne.Audio.Stream.openStream` to return
  `Stream IO ByteString` and a new `openStreamLegacy` that
  uses the shim (or just have openStream return both shapes
  in a record). Mark the legacy shim as `DEPRECATED`.
- Add a test that exercises the shim end-to-end.

Player.hs keeps using the shim. State.hs keeps the old
`peStreamHandle`. We're in a transitional state.

### Commit 3 — `Player/State.hs`: add the new field, keep the
old

- Add `peStream :: Stream IO ByteString` field to `PlaybackEnv`
- Keep `peStreamHandle` for now (it's filled by the legacy
  shim; the new field will be filled directly once we
  remove the shim)
- Add a test that asserts the legacy shim fills both fields

### Commit 4 — `Player.hs`: the new `decodeLoopStream`

- Add a new function `decodeLoopStream :: Int -> Stream
  IO ByteString -> Eff es ()` that takes the stream
  directly. It iterates the stream, decodes chunks, queues
  PCM to SDL2, and updates state. Same logic as the current
  `decodeLoop`, but pull-based on the stream instead of
  poll-based on the TChan.
- Add a test that runs `decodeLoopStream` against an
  in-memory stream of recorded MP3 bytes and asserts the
  SDL2 queue gets the right PCM.

### Commit 5 — Wire up the new path

- `startPlayback` builds a `pbEnv` with the new `peStream`
  field set to the result of `Stream.openStream`. The
  legacy `peStreamHandle` is set to the shim's handle.
- `commandProcessor` runs the new `decodeLoopStream` first.
- If the iteration ends (EOF or stream error), the loop
  returns. Cleanup runs.
- Add a regression test that simulates a station switch
  with two in-memory streams and asserts no PCM bytes
  leak between them.

### Commit 6 — Remove the legacy shim

- Delete `internal/Tourne/Audio/Stream/Shim.hs`
- Remove `peStreamHandle` from `PlaybackEnv`
- Update `Tourne.Test.Decoder` to use the stream directly
  instead of the TChan-based simulation
- All existing tests should pass against the new shape
- The `forkIO feedStream pipeline keeps accumulating
  over 5s` test is replaced by a stream-iteration test

### Commit 7 — `commandProcessor` cancellation: remove
`cancelVar`

- The decode loop's peek-for-cmd logic was needed because
  the TChan-based pull was non-blocking and the loop
  could read the channel at any time. With the new
  pull-based stream, the cmdThread's only path to interrupt
  itself is by abandoning the stream iteration. The
  cmdProcessor's `CmdPlay` arm now does this by exiting
  the current iteration (via exception or short-circuit).
- The `cancelVar :: TVar Bool` in `AudioEngine` is removed.
  The `cancel` check in the decode loop is also removed.
- Tests confirm the station-switch interrupt still works.

### Commit 8 — `Streamly` ICY stripping as a stream
transformation

- The current `feedStream` accumulates chunks and strips
  ICY metadata inline. With the stream refactor, this
  becomes a `Stream IO ByteString -> Stream IO ByteString`
  transformation, applied to the body reader's output
  before the decode loop sees it.
- This is mostly a code-organization improvement; the
  existing `stripIcyMeta` function is preserved.

### Commit 9 — Final cleanup

- Remove the now-unused `cancelVar`, the peek-for-cmd
  branches, `drainStreamChunks`, `readStreamChunk`,
  `closeStream` from `Tourne.Audio.Stream`
- Update session notes
- Final test run: 70+ tests (the new ones added in
  intermediate commits)

## Estimated time

- Commits 1-3: small (1-2 hours total)
- Commit 4-5: the big one (3-4 hours — the new decode loop)
- Commits 6-9: cleanup (1-2 hours)

Total: ~6-8 hours of focused work, best done across
2-3 sessions with breaks.

## Risks

- **Risk: the new decode loop doesn't match the production
  timing.** The current loop has a 100ms `threadDelay` when
  the queue is healthy. The new pull-based loop might
  spin faster or slower. Mitigation: keep the same
  `threadDelay` and rate-limiting logic; test with
  a real network stream after the refactor.
- **Risk: the GC-based cleanup of `Response` is too
  lazy.** If the consumer abandons the stream, the response
  might not be closed until the next GC cycle. Mitigation:
  call `responseClose` explicitly in the EOF step
  (`Just Nothing` from `unfoldrM`).
- **Risk: the audio bug returns in a new shape.** The
  current station-switch fix (cmd-loop self-interrupt
  + `SDL_ClearQueuedAudio`) was designed around the
  TChan model. With the new stream model, the interrupt
  mechanism changes. The new tests in commits 5-7
  should catch regressions.

## Test infrastructure

The `Tourne.Test.Streamly` module already exists with 5
reference tests. We may want to add:

- `Tourne.Test.Stream` — a helper that builds a
  `Stream IO ByteString` from a `Map Int ByteString` (chunk
  sizes keyed by index), for deterministic test inputs
- `Tourne.Test.Audio.Integration` — a higher-level test
  that exercises the new decode loop against an
  in-memory stream

## Open questions

- **Q: How do we model "EOF" for the producer side?**
  Currently `unfoldrM` returns `Nothing` to signal end of
  stream. If the producer (HTTP body) closes the
  connection, `brRead` returns `BS.empty`, which we map to
  `Nothing`. This works, but the explicit `responseClose`
  in the EOF step is what actually frees the connection.
  Need to verify that Streamly's runtime calls finalizers
  in a timely manner.
- **Q: How do we handle exceptions in the producer?**
  Currently `tryAny` wraps the inner action in
  `withResponse`. With `responseOpen`/`responseClose`
  directly, we lose that bracket. Mitigation: use
  `tryAny` around the unfoldrM step function or wrap
  the whole `unfoldrM` in a Streamly bracket-like
  combinator (if available).
- **Q: Does Streamly have a `bracket`-equivalent for
  streams?** Worth checking. If not, the manual
  tryAny + explicit responseClose is the way.

## Reference materials

- Streamly `unfoldrM` signature:
  `unfoldrM :: (IsStream t, MonadAsync m) => (s -> m (Maybe (a, s))) -> s -> t m a`
  (we only need `Monad m`, not `MonadAsync`)
- Streamly `before :: Monad m => m b -> Stream m a -> Stream m a`
  (per-iteration side effect, NOT once at construction)
- Streamly `take :: (IsStream t, Monad m) => Int -> t m a -> t m a`
  (limit the stream length; useful for tests)
- The five reference tests in `Tourne.Test.Streamly`
  document the actual behaviour we depend on.

## Out of scope for this refactor

- The "speed faster / frames skipped" station-switch bug
  (separate bead, separate session)
- AAC support (P2)
- TUI Action effect (S70)
- HashMap index (T3)
- Updating `Tourne.Persistence` or `Tourne.RadioBrowser` —
  these don't depend on `Stream.hs` and shouldn't need
  changes.

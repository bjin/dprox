# AGENTS.md

- Build: `stack build`.
- Test: `stack test`.
- Format changed Haskell files: `stylish-haskell -i <files>` when available.
- Run focused checks after behavioral changes; do not claim unrun verification.

## Coding style

- Follow `HASKELL-STYLE.md` for all Haskell source changes.
- Keep explicit export lists and top-level type signatures.
- Prefer small named helpers near their use over clever abstractions.
- Convert expected parse/config failures to `Either`/`Maybe`; handle them at IO boundaries.

## Avoid

- Do not use production-unsafe partials: `error`, `undefined`, `read`, `head`, `tail`, `init`, `last`, `!!`, `fromJust`.
- Do not use `unsafePerformIO` or hide effects in pure-looking helpers.
- Do not silently ignore explicit user-provided config/read/parse failures.
- Do not add mocks, stubs, or fallback behavior that masks real failures.

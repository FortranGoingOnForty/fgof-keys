# fgof-keys

Terminal key decoding helpers for modern Fortran tools.

`fgof-keys` is intended to be a small, standalone library for normalizing raw
terminal input into app-author-friendly key events.

It is part of the [FortranGoingOnForty lib-modules](https://github.com/FortranGoingOnForty/lib-modules)
catalog, but it is intended to stand on its own as a normal `fpm` package.

Current v1 target:

- normalize printable keys and named keys into one event model
- decode common control keys and ANSI escape sequences
- expose modifier state without forcing apps to parse raw bytes
- compose cleanly with `fgof-termios`, `fgof-pty`, and `fgof-lineedit`
- stay terminal-focused instead of turning into a full screen or widget toolkit

Future scope:

- richer terminal capability detection
- tighter line-edit integration examples
- higher-level screen composition in a future `fgof-screen`

## Status

Initial scaffold is in place.

Implemented today:

- public `fgof_keys` and `fgof_keys_types` modules
- stable event and modifier types
- small constructor helpers for printable and named key events
- CI and `fpm test` baseline wiring

Still to implement:

- byte-stream decoding
- CSI and SS3 escape parsing
- modifier-rich named key normalization
- bracketed paste handling
- integration examples with `fgof-termios` and `fgof-lineedit`

## Why Use It

- key handling is one of the most brittle parts of terminal apps
- existing Fortran coverage is stronger for ncurses bindings than for reusable
  key normalization
- `fgof-termios` now gives us the right foundation to decode keys cleanly

## Public API Shape

Primary modules:

- `fgof_keys`
- `fgof_keys_types`

Public types:

- `key_event`
- `key_modifiers`

Current public procedures:

- `clear_event`
- `named_key_event`
- `printable_key_event`

## Quick Start

```fortran
program demo_keys
  use fgof_keys, only : named_key_event, printable_key_event
  use fgof_keys_types, only : key_event
  implicit none

  type(key_event) :: key

  key = printable_key_event("a")
  key = named_key_event("up")
end program demo_keys
```

## Build And Test

```bash
fpm test
```

That is the baseline verification command locally and in CI.

## Supported Platforms

- macOS
- Linux

## Boundaries

- intended to stay independently versioned and releasable
- focused on key decoding and normalization, not terminal mode policy
- future `fgof-screen` should sit above this package, not inside it

## Composition

- `fgof-termios` should own raw or cbreak mode transitions
- `fgof-pty` should own PTY lifecycle and transport
- `fgof-lineedit` should consume normalized key events instead of parsing bytes

## License

MIT

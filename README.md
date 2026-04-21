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
- stable event, modifier, and decoder-state types
- explicit event-kind constants for printable, named, paste, and unknown flows
- small constructor helpers for printable and named key events
- pending-input buffering helpers for partial escape-sequence decoding
- printable-byte and single-byte control-key decoding
- CSI and SS3 decoding for arrows, home/end, page keys, insert/delete, and `F1`-`F4`
- modifier normalization for common parameterized CSI key sequences
- bracketed paste handling as a dedicated paste event
- incomplete buffering for bare `ESC` and partial CSI or SS3 prefixes
- CI and `fpm test` baseline wiring

Still to implement:

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
- `key_decoder_state`

Public constants:

- `FGOF_KEY_UP`
- `FGOF_KEY_DOWN`
- `FGOF_KEY_LEFT`
- `FGOF_KEY_RIGHT`
- `FGOF_KEY_HOME`
- `FGOF_KEY_END`
- `FGOF_KEY_PAGEUP`
- `FGOF_KEY_PAGEDOWN`
- `FGOF_KEY_ENTER`
- `FGOF_KEY_ESCAPE`
- `FGOF_KEY_TAB`
- `FGOF_KEY_BACKSPACE`
- `FGOF_KEY_INSERT`
- `FGOF_KEY_DELETE`
- `FGOF_KEY_F1`
- `FGOF_KEY_F2`
- `FGOF_KEY_F3`
- `FGOF_KEY_F4`

Current public procedures:

- `buffer_input`
- `clear_event`
- `clear_decoder_state`
- `decode_bytes`
- `decode_next_event`
- `has_pending_input`
- `mark_escape_pending`
- `named_key_event`
- `paste_key_event`
- `printable_key_event`
- `take_pending_input`
- `unknown_key_event`

## Quick Start

```fortran
program demo_keys
  use fgof_keys, only : clear_decoder_state, decode_bytes
  use fgof_keys_types, only : FGOF_KEY_EVENT_PASTE, FGOF_KEY_UP, key_decoder_state, key_event
  implicit none

  type(key_decoder_state) :: state
  type(key_event) :: key

  state = clear_decoder_state()
  key = decode_bytes(state, achar(27) // "[A")
  if (key%key_name == FGOF_KEY_UP) then
    key = decode_bytes(state, achar(27) // "[200~hello" // achar(27) // "[201~")
  end if

  if (key%kind == FGOF_KEY_EVENT_PASTE) then
    print *, key%text
  end if
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

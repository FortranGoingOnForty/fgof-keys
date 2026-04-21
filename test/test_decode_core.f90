program test_decode_core
  use fgof_keys, only : clear_decoder_state, decode_bytes, decode_next_event, has_pending_input
  use fgof_keys_types, only : FGOF_KEY_BACKSPACE, FGOF_KEY_ENTER, FGOF_KEY_ESCAPE, FGOF_KEY_EVENT_NONE, &
                              FGOF_KEY_EVENT_UNKNOWN, FGOF_KEY_TAB, key_decoder_state, key_event
  implicit none

  call test_decode_printable_byte()
  call test_decode_tab()
  call test_decode_enter()
  call test_decode_backspace()
  call test_decode_ctrl_letter()
  call test_decode_escape_as_pending()
  call test_decode_alt_printable_byte()
  call test_decode_split_alt_printable_byte()
  call test_leave_partial_csi_prefix_buffered()

contains

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) then
      print *, "FAIL:", trim(message)
      error stop 1
    end if
  end subroutine require

  subroutine test_decode_printable_byte()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, "x")

    call require(event%recognized, "printable bytes should decode to recognized events")
    call require(event%printable, "printable bytes should stay printable")
    call require(event%text == "x", "printable decode should preserve the text byte")
    call require(.not. has_pending_input(state), "printable decode should consume the byte")
  end subroutine test_decode_printable_byte

  subroutine test_decode_tab()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(9))

    call require(event%recognized, "tab should decode to a recognized key event")
    call require(event%key_name == FGOF_KEY_TAB, "tab should normalize to the tab key name")
    call require(event%raw_bytes == achar(9), "tab should preserve raw bytes")
  end subroutine test_decode_tab

  subroutine test_decode_enter()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(10))

    call require(event%recognized, "enter should decode to a recognized key event")
    call require(event%key_name == FGOF_KEY_ENTER, "LF should normalize to enter")
    call require(event%raw_bytes == achar(10), "enter should preserve raw bytes")
  end subroutine test_decode_enter

  subroutine test_decode_backspace()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(127))

    call require(event%recognized, "backspace should decode to a recognized key event")
    call require(event%key_name == FGOF_KEY_BACKSPACE, "DEL should normalize to backspace")
    call require(event%raw_bytes == achar(127), "backspace should preserve raw bytes")
  end subroutine test_decode_backspace

  subroutine test_decode_ctrl_letter()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(1))

    call require(event%recognized, "ctrl-letter bytes should decode as recognized events")
    call require(event%printable, "ctrl-letter bytes should keep a printable payload")
    call require(event%text == "a", "ctrl-A should normalize to printable a")
    call require(event%modifiers%ctrl, "ctrl-letter bytes should set ctrl")
    call require(.not. event%modifiers%alt, "plain ctrl-letter bytes should not set alt")
    call require(event%raw_bytes == achar(1), "ctrl-letter bytes should preserve raw bytes")
  end subroutine test_decode_ctrl_letter

  subroutine test_decode_escape_as_pending()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27))

    call require(event%incomplete, "bare escape should remain incomplete until more bytes arrive")
    call require(event%escape_sequence, "bare escape should mark escape-sequence state")
    call require(state%escape_pending, "bare escape should set escape_pending")
    call require(has_pending_input(state), "bare escape should remain buffered")
  end subroutine test_decode_escape_as_pending

  subroutine test_decode_alt_printable_byte()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "a")

    call require(event%recognized, "ESC-prefixed printable bytes should decode as recognized events")
    call require(event%printable, "ESC-prefixed printable bytes should stay printable")
    call require(event%text == "a", "alt-printable decode should preserve the text byte")
    call require(event%modifiers%alt, "ESC-prefixed printable bytes should set alt")
    call require(event%escape_sequence, "alt-printable bytes should mark escape-sequence origin")
    call require(.not. has_pending_input(state), "alt-printable decode should consume both bytes")
  end subroutine test_decode_alt_printable_byte

  subroutine test_decode_split_alt_printable_byte()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27))
    call require(event%incomplete, "bare escape should stay pending before a split alt sequence completes")
    call require(has_pending_input(state), "bare escape should remain buffered before a split alt sequence completes")

    event = decode_bytes(state, "a")
    call require(event%recognized, "split ESC plus printable should decode once complete")
    call require(event%printable, "split alt-printable should stay printable")
    call require(event%text == "a", "split alt-printable should preserve text")
    call require(event%modifiers%alt, "split alt-printable should set alt")
    call require(.not. has_pending_input(state), "split alt-printable should drain the buffer")
  end subroutine test_decode_split_alt_printable_byte

  subroutine test_leave_partial_csi_prefix_buffered()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[")

    call require(event%incomplete, "partial CSI prefixes should remain incomplete")
    call require(event%escape_sequence, "partial CSI prefixes should mark escape-sequence state")
    call require(state%escape_pending, "partial CSI prefixes should keep escape_pending true")
    call require(has_pending_input(state), "partial CSI prefixes should stay buffered")

    event = decode_next_event(state)
    call require(event%incomplete, "re-decoding an untouched partial CSI prefix should stay incomplete")
    call require(has_pending_input(state), "re-decoding should not drop partial CSI bytes early")
  end subroutine test_leave_partial_csi_prefix_buffered

end program test_decode_core

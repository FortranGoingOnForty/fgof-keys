program test_decode_core
  use fgof_keys, only : clear_decoder_state, decode_bytes, decode_next_event, has_pending_input
  use fgof_keys_types, only : FGOF_KEY_BACKSPACE, FGOF_KEY_ENTER, FGOF_KEY_ESCAPE, FGOF_KEY_EVENT_NONE, &
                              FGOF_KEY_EVENT_UNKNOWN, FGOF_KEY_TAB, key_decoder_state, key_event
  implicit none

  call test_decode_printable_byte()
  call test_decode_tab()
  call test_decode_enter()
  call test_decode_backspace()
  call test_decode_unknown_control()
  call test_decode_escape_as_pending()
  call test_decode_escape_key_then_plain_byte()
  call test_leave_csi_prefix_buffered()

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

  subroutine test_decode_unknown_control()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(1))

    call require(.not. event%recognized, "unsupported control bytes should stay unrecognized")
    call require(event%kind == FGOF_KEY_EVENT_UNKNOWN, "unsupported control bytes should use unknown kind")
    call require(event%raw_bytes == achar(1), "unknown control bytes should preserve raw bytes")
  end subroutine test_decode_unknown_control

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

  subroutine test_decode_escape_key_then_plain_byte()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "a")

    call require(event%recognized, "ESC followed by a plain byte should emit escape for now")
    call require(event%key_name == FGOF_KEY_ESCAPE, "ESC followed by a plain byte should emit escape first")
    call require(has_pending_input(state), "ESC followed by a plain byte should leave trailing bytes pending")

    event = decode_next_event(state)
    call require(event%recognized, "trailing plain byte should decode on the next pass")
    call require(event%printable, "trailing plain byte should decode as printable")
    call require(event%text == "a", "trailing plain byte should be preserved")
    call require(.not. has_pending_input(state), "follow-up decode should drain the buffer")
  end subroutine test_decode_escape_key_then_plain_byte

  subroutine test_leave_csi_prefix_buffered()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[A")

    call require(event%incomplete, "CSI prefixes should remain incomplete until Sprint 03 decoding lands")
    call require(event%escape_sequence, "CSI prefixes should mark escape-sequence state")
    call require(state%escape_pending, "CSI prefixes should keep escape_pending true")
    call require(has_pending_input(state), "CSI prefixes should stay buffered")

    event = decode_next_event(state)
    call require(event%incomplete, "re-decoding an untouched CSI prefix should stay incomplete")
    call require(has_pending_input(state), "re-decoding should not drop CSI bytes early")
  end subroutine test_leave_csi_prefix_buffered

end program test_decode_core

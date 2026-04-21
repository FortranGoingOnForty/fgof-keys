program test_decode_sequences
  use fgof_keys, only : clear_decoder_state, decode_bytes, decode_next_event, has_pending_input
  use fgof_keys_types, only : FGOF_KEY_DELETE, FGOF_KEY_END, FGOF_KEY_F1, FGOF_KEY_HOME, &
                              FGOF_KEY_INSERT, FGOF_KEY_LEFT, FGOF_KEY_PAGEDOWN, FGOF_KEY_PAGEUP, &
                              FGOF_KEY_RIGHT, FGOF_KEY_UP, key_decoder_state, key_event
  implicit none

  call test_decode_csi_arrows()
  call test_decode_split_csi_arrow()
  call test_decode_csi_navigation()
  call test_decode_ss3_navigation()
  call test_decode_ss3_function_key()
  call test_decode_modifier_csi_without_modifiers_yet()
  call test_keep_partial_csi_buffered()

contains

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) then
      print *, "FAIL:", trim(message)
      error stop 1
    end if
  end subroutine require

  subroutine test_decode_csi_arrows()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[A")
    call require(event%recognized, "CSI up should decode as a recognized key")
    call require(event%key_name == FGOF_KEY_UP, "CSI A should map to up")
    call require(event%raw_bytes == achar(27) // "[A", "CSI arrow should preserve raw bytes")

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[C")
    call require(event%key_name == FGOF_KEY_RIGHT, "CSI C should map to right")
  end subroutine test_decode_csi_arrows

  subroutine test_decode_split_csi_arrow()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[")
    call require(event%incomplete, "partial CSI should remain incomplete")
    call require(has_pending_input(state), "partial CSI should stay buffered")

    event = decode_bytes(state, "D")
    call require(event%recognized, "completing a split CSI should decode a key")
    call require(event%key_name == FGOF_KEY_LEFT, "CSI D should map to left")
    call require(.not. has_pending_input(state), "completed CSI should drain the buffer")
  end subroutine test_decode_split_csi_arrow

  subroutine test_decode_csi_navigation()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[1~")
    call require(event%key_name == FGOF_KEY_HOME, "CSI 1~ should map to home")

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[2~")
    call require(event%key_name == FGOF_KEY_INSERT, "CSI 2~ should map to insert")

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[3~")
    call require(event%key_name == FGOF_KEY_DELETE, "CSI 3~ should map to delete")

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[4~")
    call require(event%key_name == FGOF_KEY_END, "CSI 4~ should map to end")

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[5~")
    call require(event%key_name == FGOF_KEY_PAGEUP, "CSI 5~ should map to pageup")

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[6~")
    call require(event%key_name == FGOF_KEY_PAGEDOWN, "CSI 6~ should map to pagedown")
  end subroutine test_decode_csi_navigation

  subroutine test_decode_ss3_navigation()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "OH")
    call require(event%recognized, "SS3 home should decode as recognized")
    call require(event%key_name == FGOF_KEY_HOME, "SS3 H should map to home")
  end subroutine test_decode_ss3_navigation

  subroutine test_decode_ss3_function_key()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "OP")
    call require(event%recognized, "SS3 P should decode as a recognized function key")
    call require(event%key_name == FGOF_KEY_F1, "SS3 P should map to f1")
  end subroutine test_decode_ss3_function_key

  subroutine test_decode_modifier_csi_without_modifiers_yet()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[1;5A")
    call require(event%recognized, "parameterized CSI arrows should already decode")
    call require(event%key_name == FGOF_KEY_UP, "parameterized CSI A should still map to up")
    call require(.not. event%modifiers%ctrl, "modifier normalization should remain deferred for now")
  end subroutine test_decode_modifier_csi_without_modifiers_yet

  subroutine test_keep_partial_csi_buffered()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[1;")
    call require(event%incomplete, "unfinished parameterized CSI should remain incomplete")
    call require(has_pending_input(state), "unfinished parameterized CSI should stay buffered")

    event = decode_next_event(state)
    call require(event%incomplete, "re-reading unfinished CSI should still stay incomplete")
    call require(has_pending_input(state), "unfinished CSI should remain buffered until complete")
  end subroutine test_keep_partial_csi_buffered

end program test_decode_sequences

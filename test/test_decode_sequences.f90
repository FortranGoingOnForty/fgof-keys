program test_decode_sequences
  use fgof_keys, only : clear_decoder_state, decode_bytes, decode_next_event, has_pending_input, take_pending_input
  use fgof_keys_types, only : FGOF_KEY_DELETE, FGOF_KEY_END, FGOF_KEY_EVENT_PASTE, FGOF_KEY_F1, FGOF_KEY_F10, &
                              FGOF_KEY_F2, FGOF_KEY_F5, FGOF_KEY_F6, FGOF_KEY_HOME, &
                              FGOF_KEY_INSERT, FGOF_KEY_LEFT, FGOF_KEY_PAGEDOWN, FGOF_KEY_PAGEUP, &
                              FGOF_KEY_RIGHT, FGOF_KEY_UP, key_decoder_state, key_event
  implicit none

  call test_decode_csi_arrows()
  call test_decode_split_csi_arrow()
  call test_decode_csi_navigation()
  call test_decode_ss3_navigation()
  call test_decode_ss3_function_key()
  call test_decode_csi_function_keys()
  call test_decode_modifier_csi_arrow()
  call test_decode_modifier_csi_tilde()
  call test_decode_bracketed_paste()
  call test_decode_split_bracketed_paste()
  call test_take_pending_input_resets_partial_paste()
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

  subroutine test_decode_csi_function_keys()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[11~")
    call require(event%recognized, "CSI 11~ should decode as a recognized function key")
    call require(event%key_name == FGOF_KEY_F1, "CSI 11~ should map to f1")

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[12~")
    call require(event%key_name == FGOF_KEY_F2, "CSI 12~ should map to f2")

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[15~")
    call require(event%key_name == FGOF_KEY_F5, "CSI 15~ should map to f5")

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[17~")
    call require(event%key_name == FGOF_KEY_F6, "CSI 17~ should map to f6")

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[21~")
    call require(event%key_name == FGOF_KEY_F10, "CSI 21~ should map to f10")
  end subroutine test_decode_csi_function_keys

  subroutine test_decode_modifier_csi_arrow()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[1;5A")
    call require(event%recognized, "parameterized CSI arrows should already decode")
    call require(event%key_name == FGOF_KEY_UP, "parameterized CSI A should still map to up")
    call require(event%modifiers%ctrl, "CSI 1;5A should set ctrl")
    call require(.not. event%modifiers%shift, "CSI 1;5A should not set shift")
  end subroutine test_decode_modifier_csi_arrow

  subroutine test_decode_modifier_csi_tilde()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[5;2~")
    call require(event%recognized, "parameterized CSI tilde keys should decode")
    call require(event%key_name == FGOF_KEY_PAGEUP, "CSI 5;2~ should map to pageup")
    call require(event%modifiers%shift, "CSI 5;2~ should set shift")
    call require(.not. event%modifiers%ctrl, "CSI 5;2~ should not set ctrl")
  end subroutine test_decode_modifier_csi_tilde

  subroutine test_decode_bracketed_paste()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[200~hello" // achar(27) // "[201~")

    call require(event%recognized, "complete bracketed paste should decode as recognized")
    call require(event%paste, "complete bracketed paste should mark paste events")
    call require(event%kind == FGOF_KEY_EVENT_PASTE, "complete bracketed paste should use paste kind")
    call require(event%text == "hello", "paste payload should preserve pasted text")
    call require(event%raw_bytes == achar(27) // "[200~hello" // achar(27) // "[201~", &
      "complete bracketed paste should preserve full raw bytes")
    call require(.not. has_pending_input(state), "complete bracketed paste should drain the buffer")
    call require(.not. state%bracketed_paste_active, "complete bracketed paste should clear active paste state")
  end subroutine test_decode_bracketed_paste

  subroutine test_decode_split_bracketed_paste()
    type(key_decoder_state) :: state
    type(key_event) :: event

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[200~he")
    call require(event%incomplete, "partial bracketed paste should remain incomplete")
    call require(event%paste, "partial bracketed paste should still identify as paste")
    call require(state%bracketed_paste_active, "partial paste should keep paste mode active")
    call require(has_pending_input(state), "partial paste payload should stay buffered")

    event = decode_bytes(state, "llo" // achar(27) // "[201~")
    call require(event%recognized, "completed split paste should decode as recognized")
    call require(event%paste, "completed split paste should remain a paste event")
    call require(event%text == "hello", "split paste should preserve full payload")
    call require(event%raw_bytes == achar(27) // "[200~hello" // achar(27) // "[201~", &
      "split paste should preserve full raw bytes")
    call require(.not. state%bracketed_paste_active, "completed split paste should clear paste mode")
    call require(.not. has_pending_input(state), "completed split paste should drain the buffer")
  end subroutine test_decode_split_bracketed_paste

  subroutine test_take_pending_input_resets_partial_paste()
    type(key_decoder_state) :: state
    type(key_event) :: event
    character(len=:), allocatable :: pending

    state = clear_decoder_state()
    event = decode_bytes(state, achar(27) // "[200~he")
    call require(event%incomplete, "partial paste should remain incomplete before taking pending input")
    call require(state%bracketed_paste_active, "partial paste should mark paste mode active")

    pending = take_pending_input(state)
    call require(pending == achar(27) // "[200~he", "take_pending_input should return the full partial paste stream")
    call require(.not. state%bracketed_paste_active, "take_pending_input should clear active paste state")
    call require(.not. has_pending_input(state), "take_pending_input should drain pending paste bytes")

    event = decode_bytes(state, "x")
    call require(event%recognized, "decoder should recover cleanly after draining partial paste bytes")
    call require(event%text == "x", "decoder should treat later plain bytes normally after draining partial paste")
    call require(.not. event%paste, "decoder should not stay stuck in paste mode after draining partial paste")
  end subroutine test_take_pending_input_resets_partial_paste

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

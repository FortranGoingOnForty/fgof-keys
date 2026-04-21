program termios_setup_demo
  use fgof_keys, only : clear_decoder_state, decode_bytes
  use fgof_keys_types, only : key_decoder_state, key_event
  use fgof_termios, only : bind_guard, enter_raw_mode, restore_guard
  use fgof_termios_types, only : FGOF_TERMIOS_ERR_NONE, termios_guard
  implicit none

  type(key_decoder_state) :: decoder
  type(key_event) :: event
  type(termios_guard) :: guard

  decoder = clear_decoder_state()

  call bind_guard(guard)
  if (guard%last_error_code == FGOF_TERMIOS_ERR_NONE) then
    call enter_raw_mode(guard)
  end if

  event = decode_bytes(decoder, achar(27) // "[A")
  if (event%recognized) then
    print "(a)", event%key_name
  end if

  if (guard%last_error_code == FGOF_TERMIOS_ERR_NONE) then
    call restore_guard(guard)
  end if
end program termios_setup_demo

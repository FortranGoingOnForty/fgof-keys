module fgof_keys
  use fgof_keys_types, only : FGOF_KEY_BACKSPACE, FGOF_KEY_ENTER, FGOF_KEY_ESCAPE, FGOF_KEY_EVENT_NAMED, &
                              FGOF_KEY_EVENT_NONE, FGOF_KEY_EVENT_PRINTABLE, FGOF_KEY_EVENT_UNKNOWN, &
                              FGOF_KEY_TAB, key_decoder_state, key_event, key_modifiers
  implicit none
  private

  public :: buffer_input
  public :: clear_event
  public :: clear_decoder_state
  public :: decode_bytes
  public :: decode_next_event
  public :: has_pending_input
  public :: mark_escape_pending
  public :: named_key_event
  public :: printable_key_event
  public :: take_pending_input
  public :: unknown_key_event

contains

  pure function clear_event() result(event)
    type(key_event) :: event

    event%kind = FGOF_KEY_EVENT_NONE
    event%recognized = .false.
    event%printable = .false.
    event%escape_sequence = .false.
    event%paste = .false.
    event%incomplete = .false.
  end function clear_event

  pure function clear_decoder_state() result(state)
    type(key_decoder_state) :: state

    state%escape_pending = .false.
    state%bracketed_paste_active = .false.
  end function clear_decoder_state

  pure function printable_key_event(text, modifiers) result(event)
    character(len=*), intent(in) :: text
    type(key_modifiers), intent(in), optional :: modifiers
    type(key_event) :: event

    event = clear_event()
    event%kind = FGOF_KEY_EVENT_PRINTABLE
    event%text = text
    event%raw_bytes = text
    event%recognized = .true.
    event%printable = .true.
    if (present(modifiers)) event%modifiers = modifiers
  end function printable_key_event

  pure function named_key_event(name, modifiers, raw_bytes, escape_sequence) result(event)
    character(len=*), intent(in) :: name
    type(key_modifiers), intent(in), optional :: modifiers
    character(len=*), intent(in), optional :: raw_bytes
    logical, intent(in), optional :: escape_sequence
    type(key_event) :: event

    event = clear_event()
    event%kind = FGOF_KEY_EVENT_NAMED
    event%key_name = name
    event%recognized = .true.
    if (present(modifiers)) event%modifiers = modifiers
    if (present(raw_bytes)) event%raw_bytes = raw_bytes
    if (present(escape_sequence)) then
      event%escape_sequence = escape_sequence
    end if
  end function named_key_event

  pure function unknown_key_event(raw_bytes, escape_sequence) result(event)
    character(len=*), intent(in) :: raw_bytes
    logical, intent(in), optional :: escape_sequence
    type(key_event) :: event

    event = clear_event()
    event%kind = FGOF_KEY_EVENT_UNKNOWN
    event%raw_bytes = raw_bytes
    if (present(escape_sequence)) event%escape_sequence = escape_sequence
  end function unknown_key_event

  subroutine buffer_input(state, fragment)
    type(key_decoder_state), intent(inout) :: state
    character(len=*), intent(in) :: fragment

    if (.not. allocated(state%pending_bytes)) then
      state%pending_bytes = fragment
    else
      state%pending_bytes = state%pending_bytes // fragment
    end if
  end subroutine buffer_input

  pure logical function has_pending_input(state)
    type(key_decoder_state), intent(in) :: state

    has_pending_input = allocated(state%pending_bytes)
    if (has_pending_input) then
      has_pending_input = len(state%pending_bytes) > 0
    end if
  end function has_pending_input

  subroutine mark_escape_pending(state, pending)
    type(key_decoder_state), intent(inout) :: state
    logical, intent(in), optional :: pending

    state%escape_pending = .true.
    if (present(pending)) state%escape_pending = pending
  end subroutine mark_escape_pending

  function take_pending_input(state) result(fragment)
    type(key_decoder_state), intent(inout) :: state
    character(len=:), allocatable :: fragment

    if (allocated(state%pending_bytes)) then
      fragment = state%pending_bytes
      deallocate(state%pending_bytes)
    else
      fragment = ""
    end if
    state%escape_pending = .false.
  end function take_pending_input

  function decode_bytes(state, fragment) result(event)
    type(key_decoder_state), intent(inout) :: state
    character(len=*), intent(in) :: fragment
    type(key_event) :: event

    call buffer_input(state, fragment)
    event = decode_next_event(state)
  end function decode_bytes

  function decode_next_event(state) result(event)
    type(key_decoder_state), intent(inout) :: state
    type(key_event) :: event

    character(len=:), allocatable :: bytes
    character(len=1) :: first_byte
    integer :: byte_code

    event = clear_event()
    if (.not. has_pending_input(state)) return

    bytes = state%pending_bytes
    first_byte = bytes(1:1)
    byte_code = iachar(first_byte)

    select case (byte_code)
    case (27)
      if (len(bytes) == 1) then
        event = incomplete_key_event(first_byte, escape_sequence=.true.)
        state%escape_pending = .true.
        return
      end if

      if (bytes(2:2) == "[" .or. bytes(2:2) == "O") then
        event = incomplete_key_event(bytes, escape_sequence=.true.)
        state%escape_pending = .true.
        return
      end if

      call consume_pending_bytes(state, 1)
      event = named_key_event(FGOF_KEY_ESCAPE, raw_bytes=first_byte, escape_sequence=.true.)
      state%escape_pending = .false.
    case (9)
      call consume_pending_bytes(state, 1)
      event = named_key_event(FGOF_KEY_TAB, raw_bytes=first_byte)
      state%escape_pending = .false.
    case (10, 13)
      call consume_pending_bytes(state, 1)
      event = named_key_event(FGOF_KEY_ENTER, raw_bytes=first_byte)
      state%escape_pending = .false.
    case (8, 127)
      call consume_pending_bytes(state, 1)
      event = named_key_event(FGOF_KEY_BACKSPACE, raw_bytes=first_byte)
      state%escape_pending = .false.
    case (32:126)
      call consume_pending_bytes(state, 1)
      event = printable_key_event(first_byte)
      state%escape_pending = .false.
    case default
      call consume_pending_bytes(state, 1)
      event = unknown_key_event(first_byte)
      state%escape_pending = .false.
    end select
  end function decode_next_event

  pure function incomplete_key_event(raw_bytes, escape_sequence) result(event)
    character(len=*), intent(in) :: raw_bytes
    logical, intent(in), optional :: escape_sequence
    type(key_event) :: event

    event = clear_event()
    event%raw_bytes = raw_bytes
    event%incomplete = .true.
    if (present(escape_sequence)) event%escape_sequence = escape_sequence
  end function incomplete_key_event

  subroutine consume_pending_bytes(state, count)
    type(key_decoder_state), intent(inout) :: state
    integer, intent(in) :: count

    integer :: remaining

    if (.not. allocated(state%pending_bytes)) return

    remaining = len(state%pending_bytes) - count
    if (remaining <= 0) then
      deallocate(state%pending_bytes)
    else
      state%pending_bytes = state%pending_bytes(count + 1:)
    end if
  end subroutine consume_pending_bytes

end module fgof_keys

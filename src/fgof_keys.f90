module fgof_keys
  use fgof_keys_types, only : FGOF_KEY_EVENT_NAMED, FGOF_KEY_EVENT_NONE, FGOF_KEY_EVENT_PRINTABLE, &
                              key_decoder_state, key_event, key_modifiers
  implicit none
  private

  public :: buffer_input
  public :: clear_event
  public :: clear_decoder_state
  public :: has_pending_input
  public :: mark_escape_pending
  public :: named_key_event
  public :: printable_key_event
  public :: take_pending_input

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

  pure function named_key_event(name, modifiers, escape_sequence) result(event)
    character(len=*), intent(in) :: name
    type(key_modifiers), intent(in), optional :: modifiers
    logical, intent(in), optional :: escape_sequence
    type(key_event) :: event

    event = clear_event()
    event%kind = FGOF_KEY_EVENT_NAMED
    event%key_name = name
    event%recognized = .true.
    if (present(modifiers)) event%modifiers = modifiers
    if (present(escape_sequence)) then
      event%escape_sequence = escape_sequence
    end if
  end function named_key_event

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

end module fgof_keys

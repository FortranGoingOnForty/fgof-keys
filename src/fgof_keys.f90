module fgof_keys
  use fgof_keys_types, only : key_event, key_modifiers
  implicit none
  private

  public :: clear_event
  public :: named_key_event
  public :: printable_key_event

contains

  pure function clear_event() result(event)
    type(key_event) :: event

    event%recognized = .false.
    event%printable = .false.
    event%escape_sequence = .false.
  end function clear_event

  pure function printable_key_event(text, modifiers) result(event)
    character(len=*), intent(in) :: text
    type(key_modifiers), intent(in), optional :: modifiers
    type(key_event) :: event

    event = clear_event()
    event%text = text
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
    event%key_name = name
    event%recognized = .true.
    if (present(modifiers)) event%modifiers = modifiers
    if (present(escape_sequence)) then
      event%escape_sequence = escape_sequence
    end if
  end function named_key_event

end module fgof_keys

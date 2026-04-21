program test_scaffold
  use fgof_keys, only : clear_event, named_key_event, printable_key_event
  use fgof_keys_types, only : key_event, key_modifiers
  implicit none

  call test_clear_event()
  call test_printable_key_event()
  call test_named_key_event()

contains

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) then
      print *, "FAIL:", trim(message)
      error stop 1
    end if
  end subroutine require

  subroutine test_clear_event()
    type(key_event) :: event

    event = clear_event()
    call require(.not. event%recognized, "clear_event should start unrecognized")
    call require(.not. event%printable, "clear_event should not be printable")
    call require(.not. event%escape_sequence, "clear_event should not mark escape sequences")
  end subroutine test_clear_event

  subroutine test_printable_key_event()
    type(key_event) :: event
    type(key_modifiers) :: modifiers

    modifiers%shift = .true.
    event = printable_key_event("A", modifiers)

    call require(event%recognized, "printable_key_event should recognize the event")
    call require(event%printable, "printable_key_event should mark printable input")
    call require(event%text == "A", "printable_key_event should preserve text")
    call require(event%modifiers%shift, "printable_key_event should keep modifiers")
  end subroutine test_printable_key_event

  subroutine test_named_key_event()
    type(key_event) :: event

    event = named_key_event("up", escape_sequence=.true.)

    call require(event%recognized, "named_key_event should recognize the event")
    call require(.not. event%printable, "named_key_event should not mark printable input")
    call require(event%key_name == "up", "named_key_event should preserve key names")
    call require(event%escape_sequence, "named_key_event should allow escape tagging")
  end subroutine test_named_key_event

end program test_scaffold

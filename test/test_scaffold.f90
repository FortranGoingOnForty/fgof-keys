program test_scaffold
  use fgof_keys, only : buffer_input, clear_decoder_state, clear_event, has_pending_input, &
                        mark_escape_pending, named_key_event, printable_key_event, take_pending_input
  use fgof_keys_types, only : FGOF_KEY_EVENT_NAMED, FGOF_KEY_EVENT_NONE, FGOF_KEY_EVENT_PRINTABLE, &
                              FGOF_KEY_UP, key_decoder_state, key_event, key_modifiers
  implicit none

  call test_clear_event()
  call test_clear_decoder_state()
  call test_key_constants()
  call test_printable_key_event()
  call test_named_key_event()
  call test_buffer_input()
  call test_take_pending_input()
  call test_mark_escape_pending()

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
    call require(event%kind == FGOF_KEY_EVENT_NONE, "clear_event should reset event kind")
    call require(.not. event%recognized, "clear_event should start unrecognized")
    call require(.not. event%printable, "clear_event should not be printable")
    call require(.not. event%escape_sequence, "clear_event should not mark escape sequences")
    call require(.not. event%paste, "clear_event should not mark paste events")
    call require(.not. event%incomplete, "clear_event should not mark incomplete events")
  end subroutine test_clear_event

  subroutine test_clear_decoder_state()
    type(key_decoder_state) :: state

    state = clear_decoder_state()
    call require(.not. has_pending_input(state), "clear_decoder_state should start with no pending input")
    call require(.not. state%escape_pending, "clear_decoder_state should clear escape state")
    call require(.not. state%bracketed_paste_active, "clear_decoder_state should clear paste state")
  end subroutine test_clear_decoder_state

  subroutine test_key_constants()
    call require(FGOF_KEY_UP == "up", "key constants should expose stable canonical names")
  end subroutine test_key_constants

  subroutine test_printable_key_event()
    type(key_event) :: event
    type(key_modifiers) :: modifiers

    modifiers%shift = .true.
    event = printable_key_event("A", modifiers)

    call require(event%recognized, "printable_key_event should recognize the event")
    call require(event%printable, "printable_key_event should mark printable input")
    call require(event%kind == FGOF_KEY_EVENT_PRINTABLE, "printable_key_event should set printable kind")
    call require(event%text == "A", "printable_key_event should preserve text")
    call require(event%raw_bytes == "A", "printable_key_event should preserve raw bytes")
    call require(event%modifiers%shift, "printable_key_event should keep modifiers")
  end subroutine test_printable_key_event

  subroutine test_named_key_event()
    type(key_event) :: event

    event = named_key_event("up", escape_sequence=.true.)

    call require(event%recognized, "named_key_event should recognize the event")
    call require(.not. event%printable, "named_key_event should not mark printable input")
    call require(event%kind == FGOF_KEY_EVENT_NAMED, "named_key_event should set named-event kind")
    call require(event%key_name == "up", "named_key_event should preserve key names")
    call require(event%escape_sequence, "named_key_event should allow escape tagging")
  end subroutine test_named_key_event

  subroutine test_buffer_input()
    type(key_decoder_state) :: state

    state = clear_decoder_state()
    call buffer_input(state, achar(27))
    call buffer_input(state, "[A")

    call require(has_pending_input(state), "buffer_input should accumulate pending bytes")
    call require(state%pending_bytes == achar(27) // "[A", "buffer_input should append fragments in order")
  end subroutine test_buffer_input

  subroutine test_take_pending_input()
    type(key_decoder_state) :: state
    character(len=:), allocatable :: fragment

    state = clear_decoder_state()
    call buffer_input(state, achar(27) // "[")
    call mark_escape_pending(state)
    fragment = take_pending_input(state)

    call require(fragment == achar(27) // "[", "take_pending_input should return buffered bytes")
    call require(.not. has_pending_input(state), "take_pending_input should clear the buffer")
    call require(.not. state%escape_pending, "take_pending_input should clear escape-pending state")
  end subroutine test_take_pending_input

  subroutine test_mark_escape_pending()
    type(key_decoder_state) :: state

    state = clear_decoder_state()
    call mark_escape_pending(state)
    call require(state%escape_pending, "mark_escape_pending should default to true")
    call mark_escape_pending(state, .false.)
    call require(.not. state%escape_pending, "mark_escape_pending should accept explicit false")
  end subroutine test_mark_escape_pending

end program test_scaffold

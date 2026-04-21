program test_editor_helpers
  use fgof_keys, only : editor_action_for_key, event_text, named_key_event, paste_key_event, printable_key_event
  use fgof_keys_types, only : FGOF_EDITOR_ACTION_ACCEPT_LINE, FGOF_EDITOR_ACTION_DELETE_LEFT, &
                              FGOF_EDITOR_ACTION_HISTORY_PREVIOUS, FGOF_EDITOR_ACTION_INSERT_TEXT, &
                              FGOF_EDITOR_ACTION_MOVE_LEFT, FGOF_EDITOR_ACTION_NONE, FGOF_KEY_BACKSPACE, &
                              FGOF_KEY_ENTER, FGOF_KEY_F1, FGOF_KEY_LEFT, FGOF_KEY_UP, key_event, key_modifiers
  implicit none

  call test_printable_maps_to_insert()
  call test_paste_maps_to_insert()
  call test_modified_printable_stays_policy_neutral()
  call test_navigation_maps_to_edit_actions()
  call test_unmapped_keys_stay_none()

contains

  subroutine require(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

    if (.not. condition) then
      print *, "FAIL:", trim(message)
      error stop 1
    end if
  end subroutine require

  subroutine test_printable_maps_to_insert()
    type(key_event) :: event

    event = printable_key_event("x")
    call require(editor_action_for_key(event) == FGOF_EDITOR_ACTION_INSERT_TEXT, &
      "printable keys should map to insert-text actions")
    call require(event_text(event) == "x", "printable keys should expose their text payload")
  end subroutine test_printable_maps_to_insert

  subroutine test_paste_maps_to_insert()
    type(key_event) :: event

    event = paste_key_event("hello")
    call require(editor_action_for_key(event) == FGOF_EDITOR_ACTION_INSERT_TEXT, &
      "paste events should also map to insert-text actions")
    call require(event_text(event) == "hello", "paste events should expose their full payload")
  end subroutine test_paste_maps_to_insert

  subroutine test_modified_printable_stays_policy_neutral()
    type(key_event) :: event
    type(key_modifiers) :: modifiers

    modifiers%alt = .true.
    event = printable_key_event("x", modifiers=modifiers, raw_bytes=achar(27) // "x")

    call require(editor_action_for_key(event) == FGOF_EDITOR_ACTION_NONE, &
      "modified printable bytes should not auto-map to insert-text actions")
    call require(event_text(event) == "x", "modified printable bytes should still expose text payload")
  end subroutine test_modified_printable_stays_policy_neutral

  subroutine test_navigation_maps_to_edit_actions()
    type(key_event) :: event

    event = named_key_event(FGOF_KEY_ENTER)
    call require(editor_action_for_key(event) == FGOF_EDITOR_ACTION_ACCEPT_LINE, &
      "enter should map to accept-line")

    event = named_key_event(FGOF_KEY_LEFT)
    call require(editor_action_for_key(event) == FGOF_EDITOR_ACTION_MOVE_LEFT, &
      "left should map to move-left")

    event = named_key_event(FGOF_KEY_BACKSPACE)
    call require(editor_action_for_key(event) == FGOF_EDITOR_ACTION_DELETE_LEFT, &
      "backspace should map to delete-left")

    event = named_key_event(FGOF_KEY_UP)
    call require(editor_action_for_key(event) == FGOF_EDITOR_ACTION_HISTORY_PREVIOUS, &
      "up should map to history-previous")
  end subroutine test_navigation_maps_to_edit_actions

  subroutine test_unmapped_keys_stay_none()
    type(key_event) :: event

    event = named_key_event(FGOF_KEY_F1)
    call require(editor_action_for_key(event) == FGOF_EDITOR_ACTION_NONE, &
      "function keys should stay unmapped unless the app decides otherwise")
    call require(event_text(event) == "", "non-text keys should expose empty text payloads")
  end subroutine test_unmapped_keys_stay_none

end program test_editor_helpers

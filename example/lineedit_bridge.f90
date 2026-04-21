program lineedit_bridge
  use fgof_keys, only : clear_decoder_state, decode_bytes, editor_action_for_key, event_text
  use fgof_keys_types, only : FGOF_EDITOR_ACTION_DELETE_LEFT, FGOF_EDITOR_ACTION_HISTORY_NEXT, &
                              FGOF_EDITOR_ACTION_HISTORY_PREVIOUS, FGOF_EDITOR_ACTION_INSERT_TEXT, &
                              FGOF_EDITOR_ACTION_MOVE_END, FGOF_EDITOR_ACTION_MOVE_HOME, &
                              FGOF_EDITOR_ACTION_MOVE_LEFT, FGOF_EDITOR_ACTION_MOVE_RIGHT, &
                              key_decoder_state, key_event
  use fgof_lineedit, only : delete_left, history_next, history_previous, init_lineedit, insert_text, &
                            lineedit_state, move_cursor_end, move_cursor_home, move_cursor_left, &
                            move_cursor_right
  implicit none

  type(key_decoder_state) :: decoder
  type(lineedit_state) :: editor
  type(key_event) :: event

  decoder = clear_decoder_state()
  call init_lineedit(editor)

  event = decode_bytes(decoder, "h")
  call apply_editor_action(editor, event)
  event = decode_bytes(decoder, "i")
  call apply_editor_action(editor, event)
  event = decode_bytes(decoder, achar(27) // "[D")
  call apply_editor_action(editor, event)
  event = decode_bytes(decoder, achar(27) // "[200~!" // achar(27) // "[201~")
  call apply_editor_action(editor, event)

  print "(a)", editor%buffer
  print "(a,i0)", "cursor=", editor%cursor

contains

  subroutine apply_editor_action(editor, event)
    type(lineedit_state), intent(inout) :: editor
    type(key_event), intent(in) :: event
    logical :: changed

    select case (editor_action_for_key(event))
    case (FGOF_EDITOR_ACTION_INSERT_TEXT)
      call insert_text(editor, event_text(event))
    case (FGOF_EDITOR_ACTION_MOVE_LEFT)
      changed = move_cursor_left(editor)
    case (FGOF_EDITOR_ACTION_MOVE_RIGHT)
      changed = move_cursor_right(editor)
    case (FGOF_EDITOR_ACTION_MOVE_HOME)
      call move_cursor_home(editor)
    case (FGOF_EDITOR_ACTION_MOVE_END)
      call move_cursor_end(editor)
    case (FGOF_EDITOR_ACTION_DELETE_LEFT)
      changed = delete_left(editor)
    case (FGOF_EDITOR_ACTION_HISTORY_PREVIOUS)
      changed = history_previous(editor)
    case (FGOF_EDITOR_ACTION_HISTORY_NEXT)
      changed = history_next(editor)
    end select
  end subroutine apply_editor_action

end program lineedit_bridge

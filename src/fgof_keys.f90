module fgof_keys
  use fgof_keys_types, only : FGOF_KEY_BACKSPACE, FGOF_KEY_DELETE, FGOF_KEY_DOWN, FGOF_KEY_END, &
                              FGOF_KEY_ENTER, FGOF_KEY_ESCAPE, FGOF_KEY_EVENT_NAMED, FGOF_KEY_EVENT_NONE, &
                              FGOF_KEY_EVENT_PASTE, FGOF_KEY_EVENT_PRINTABLE, FGOF_KEY_EVENT_UNKNOWN, &
                              FGOF_KEY_F1, FGOF_KEY_F2, FGOF_KEY_F3, FGOF_KEY_F4, FGOF_KEY_F5, &
                              FGOF_KEY_F6, FGOF_KEY_F7, FGOF_KEY_F8, FGOF_KEY_F9, FGOF_KEY_F10, FGOF_KEY_HOME, &
                              FGOF_KEY_INSERT, FGOF_KEY_LEFT, FGOF_KEY_PAGEDOWN, FGOF_KEY_PAGEUP, &
                              FGOF_KEY_RIGHT, FGOF_KEY_TAB, FGOF_KEY_UP, FGOF_EDITOR_ACTION_ACCEPT_LINE, &
                              FGOF_EDITOR_ACTION_CANCEL, FGOF_EDITOR_ACTION_COMPLETE, &
                              FGOF_EDITOR_ACTION_DELETE_LEFT, FGOF_EDITOR_ACTION_DELETE_RIGHT, &
                              FGOF_EDITOR_ACTION_HISTORY_NEXT, FGOF_EDITOR_ACTION_HISTORY_PREVIOUS, &
                              FGOF_EDITOR_ACTION_INSERT_TEXT, FGOF_EDITOR_ACTION_MOVE_END, &
                              FGOF_EDITOR_ACTION_MOVE_HOME, FGOF_EDITOR_ACTION_MOVE_LEFT, &
                              FGOF_EDITOR_ACTION_MOVE_RIGHT, FGOF_EDITOR_ACTION_NONE, &
                              key_decoder_state, key_event, key_modifiers
  implicit none
  private

  public :: buffer_input
  public :: clear_event
  public :: clear_decoder_state
  public :: decode_bytes
  public :: decode_next_event
  public :: editor_action_for_key
  public :: event_text
  public :: has_pending_input
  public :: mark_escape_pending
  public :: named_key_event
  public :: paste_key_event
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

  pure function printable_key_event(text, modifiers, raw_bytes) result(event)
    character(len=*), intent(in) :: text
    type(key_modifiers), intent(in), optional :: modifiers
    character(len=*), intent(in), optional :: raw_bytes
    type(key_event) :: event

    event = clear_event()
    event%kind = FGOF_KEY_EVENT_PRINTABLE
    event%text = text
    if (present(raw_bytes)) then
      event%raw_bytes = raw_bytes
    else
      event%raw_bytes = text
    end if
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

  pure function paste_key_event(text, raw_bytes) result(event)
    character(len=*), intent(in) :: text
    character(len=*), intent(in), optional :: raw_bytes
    type(key_event) :: event

    event = clear_event()
    event%kind = FGOF_KEY_EVENT_PASTE
    event%text = text
    if (present(raw_bytes)) then
      event%raw_bytes = raw_bytes
    else
      event%raw_bytes = text
    end if
    event%recognized = .true.
    event%paste = .true.
  end function paste_key_event

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
    state%bracketed_paste_active = .false.
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

    if (state%bracketed_paste_active) then
      event = decode_bracketed_paste_or_incomplete(state)
      return
    end if

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

      if (bytes(2:2) == "[") then
        event = decode_csi_or_incomplete(state, bytes)
        return
      end if

      if (bytes(2:2) == "O") then
        event = decode_ss3_or_incomplete(state, bytes)
        return
      end if

      event = decode_alt_prefixed_or_escape(state, bytes)
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
    case (1:7, 11:12, 14:26)
      call consume_pending_bytes(state, 1)
      event = ctrl_printable_key_event(byte_code, first_byte)
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

  pure integer function editor_action_for_key(event) result(action)
    type(key_event), intent(in) :: event

    action = FGOF_EDITOR_ACTION_NONE

    if (event%paste) then
      action = FGOF_EDITOR_ACTION_INSERT_TEXT
      return
    end if

    if (event%printable) then
      if (.not. event%modifiers%alt .and. .not. event%modifiers%ctrl .and. .not. event%modifiers%super) then
        action = FGOF_EDITOR_ACTION_INSERT_TEXT
      end if
      return
    end if

    if (.not. event%recognized) return

    select case (event%key_name)
    case (FGOF_KEY_ENTER)
      action = FGOF_EDITOR_ACTION_ACCEPT_LINE
    case (FGOF_KEY_TAB)
      action = FGOF_EDITOR_ACTION_COMPLETE
    case (FGOF_KEY_ESCAPE)
      action = FGOF_EDITOR_ACTION_CANCEL
    case (FGOF_KEY_LEFT)
      action = FGOF_EDITOR_ACTION_MOVE_LEFT
    case (FGOF_KEY_RIGHT)
      action = FGOF_EDITOR_ACTION_MOVE_RIGHT
    case (FGOF_KEY_HOME)
      action = FGOF_EDITOR_ACTION_MOVE_HOME
    case (FGOF_KEY_END)
      action = FGOF_EDITOR_ACTION_MOVE_END
    case (FGOF_KEY_BACKSPACE)
      action = FGOF_EDITOR_ACTION_DELETE_LEFT
    case (FGOF_KEY_DELETE)
      action = FGOF_EDITOR_ACTION_DELETE_RIGHT
    case (FGOF_KEY_UP)
      action = FGOF_EDITOR_ACTION_HISTORY_PREVIOUS
    case (FGOF_KEY_DOWN)
      action = FGOF_EDITOR_ACTION_HISTORY_NEXT
    end select
  end function editor_action_for_key

  pure function event_text(event) result(text)
    type(key_event), intent(in) :: event
    character(len=:), allocatable :: text

    if (event%paste .or. event%printable) then
      if (allocated(event%text)) then
        text = event%text
      else
        text = ""
      end if
    else
      text = ""
    end if
  end function event_text

  function decode_alt_prefixed_or_escape(state, bytes) result(event)
    type(key_decoder_state), intent(inout) :: state
    character(len=*), intent(in) :: bytes
    type(key_event) :: event

    character(len=1) :: second_byte
    integer :: second_code

    second_byte = bytes(2:2)
    second_code = iachar(second_byte)

    select case (second_code)
    case (9)
      call consume_pending_bytes(state, 2)
      event = alt_named_key_event(FGOF_KEY_TAB, achar(27) // second_byte)
    case (10, 13)
      call consume_pending_bytes(state, 2)
      event = alt_named_key_event(FGOF_KEY_ENTER, achar(27) // second_byte)
    case (8, 127)
      call consume_pending_bytes(state, 2)
      event = alt_named_key_event(FGOF_KEY_BACKSPACE, achar(27) // second_byte)
    case (1:7, 11:12, 14:26)
      call consume_pending_bytes(state, 2)
      event = ctrl_printable_key_event(second_code, achar(27) // second_byte, alt=.true.)
    case (32:126)
      call consume_pending_bytes(state, 2)
      event = alt_printable_key_event(second_byte, achar(27) // second_byte)
    case default
      call consume_pending_bytes(state, 1)
      event = named_key_event(FGOF_KEY_ESCAPE, raw_bytes=bytes(1:1), escape_sequence=.true.)
    end select
  end function decode_alt_prefixed_or_escape

  pure function incomplete_key_event(raw_bytes, escape_sequence) result(event)
    character(len=*), intent(in) :: raw_bytes
    logical, intent(in), optional :: escape_sequence
    type(key_event) :: event

    event = clear_event()
    event%raw_bytes = raw_bytes
    event%incomplete = .true.
    if (present(escape_sequence)) event%escape_sequence = escape_sequence
  end function incomplete_key_event

  pure function incomplete_paste_event(raw_bytes) result(event)
    character(len=*), intent(in) :: raw_bytes
    type(key_event) :: event

    event = clear_event()
    event%kind = FGOF_KEY_EVENT_PASTE
    event%raw_bytes = raw_bytes
    event%paste = .true.
    event%incomplete = .true.
  end function incomplete_paste_event

  pure function alt_named_key_event(name, raw_bytes) result(event)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: raw_bytes
    type(key_event) :: event
    type(key_modifiers) :: modifiers

    modifiers%alt = .true.
    event = named_key_event(name, modifiers=modifiers, raw_bytes=raw_bytes, escape_sequence=.true.)
  end function alt_named_key_event

  pure function alt_printable_key_event(text, raw_bytes) result(event)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: raw_bytes
    type(key_event) :: event
    type(key_modifiers) :: modifiers

    modifiers%alt = .true.
    event = printable_key_event(text, modifiers=modifiers, raw_bytes=raw_bytes)
    event%escape_sequence = .true.
  end function alt_printable_key_event

  pure function ctrl_printable_key_event(byte_code, raw_bytes, alt) result(event)
    integer, intent(in) :: byte_code
    character(len=*), intent(in) :: raw_bytes
    logical, intent(in), optional :: alt
    type(key_event) :: event
    type(key_modifiers) :: modifiers
    character(len=1) :: text

    text = achar(iachar("a") + byte_code - 1)
    modifiers%ctrl = .true.
    if (present(alt)) modifiers%alt = alt
    event = printable_key_event(text, modifiers=modifiers, raw_bytes=raw_bytes)
    if (present(alt)) then
      if (alt) event%escape_sequence = .true.
    end if
  end function ctrl_printable_key_event

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

  function decode_csi_or_incomplete(state, bytes) result(event)
    type(key_decoder_state), intent(inout) :: state
    character(len=*), intent(in) :: bytes
    type(key_event) :: event

    integer :: sequence_length

    sequence_length = csi_sequence_length(bytes)
    if (sequence_length == 0) then
      event = incomplete_key_event(bytes, escape_sequence=.true.)
      state%escape_pending = .true.
      return
    end if

    event = decode_csi_sequence(bytes(:sequence_length), state)
    if (state%bracketed_paste_active) then
      if (has_pending_input(state)) then
        event = decode_bracketed_paste_or_incomplete(state)
      else
        event = incomplete_paste_event("")
      end if
      return
    end if
    call consume_pending_bytes(state, sequence_length)
    state%escape_pending = .false.
  end function decode_csi_or_incomplete

  function decode_ss3_or_incomplete(state, bytes) result(event)
    type(key_decoder_state), intent(inout) :: state
    character(len=*), intent(in) :: bytes
    type(key_event) :: event

    integer :: sequence_length

    sequence_length = ss3_sequence_length(bytes)
    if (sequence_length == 0) then
      event = incomplete_key_event(bytes, escape_sequence=.true.)
      state%escape_pending = .true.
      return
    end if

    event = decode_ss3_sequence(bytes(:sequence_length))
    call consume_pending_bytes(state, sequence_length)
    state%escape_pending = .false.
  end function decode_ss3_or_incomplete

  pure integer function csi_sequence_length(bytes)
    character(len=*), intent(in) :: bytes

    integer :: i

    csi_sequence_length = 0
    if (len(bytes) < 3) return

    do i = 3, len(bytes)
      if (is_csi_final(bytes(i:i))) then
        csi_sequence_length = i
        return
      end if
    end do
  end function csi_sequence_length

  pure integer function ss3_sequence_length(bytes)
    character(len=*), intent(in) :: bytes

    ss3_sequence_length = 0
    if (len(bytes) < 3) return
    if (is_csi_final(bytes(3:3))) ss3_sequence_length = 3
  end function ss3_sequence_length

  pure logical function is_csi_final(byte)
    character(len=1), intent(in) :: byte
    integer :: code

    code = iachar(byte)
    is_csi_final = code >= 64 .and. code <= 126
  end function is_csi_final

  function decode_csi_sequence(sequence, state) result(event)
    character(len=*), intent(in) :: sequence
    type(key_decoder_state), intent(inout) :: state
    type(key_event) :: event

    character(len=1) :: final_byte
    character(len=:), allocatable :: parameters
    type(key_modifiers) :: modifiers
    integer :: first_parameter

    final_byte = sequence(len(sequence):len(sequence))
    if (len(sequence) > 3) then
      parameters = sequence(3:len(sequence) - 1)
    else
      parameters = ""
    end if
    modifiers = csi_modifiers(parameters)
    first_parameter = first_csi_parameter(parameters)

    select case (final_byte)
    case ("A")
      event = named_key_event(FGOF_KEY_UP, modifiers, sequence, .true.)
    case ("B")
      event = named_key_event(FGOF_KEY_DOWN, modifiers, sequence, .true.)
    case ("C")
      event = named_key_event(FGOF_KEY_RIGHT, modifiers, sequence, .true.)
    case ("D")
      event = named_key_event(FGOF_KEY_LEFT, modifiers, sequence, .true.)
    case ("F")
      event = named_key_event(FGOF_KEY_END, modifiers, sequence, .true.)
    case ("H")
      event = named_key_event(FGOF_KEY_HOME, modifiers, sequence, .true.)
    case ("~")
      if (first_parameter == 200) then
        state%bracketed_paste_active = .true.
        state%escape_pending = .false.
        event = incomplete_paste_event(sequence)
      else
        event = decode_csi_tilde_sequence(parameters, sequence, modifiers)
      end if
    case default
      event = unknown_key_event(sequence, escape_sequence=.true.)
    end select
  end function decode_csi_sequence

  pure function decode_csi_tilde_sequence(parameters, sequence, modifiers) result(event)
    character(len=*), intent(in) :: parameters
    character(len=*), intent(in) :: sequence
    type(key_modifiers), intent(in) :: modifiers
    type(key_event) :: event

    integer :: code

    code = first_csi_parameter(parameters)
    select case (code)
    case (1, 7)
      event = named_key_event(FGOF_KEY_HOME, modifiers, sequence, .true.)
    case (2)
      event = named_key_event(FGOF_KEY_INSERT, modifiers, sequence, .true.)
    case (3)
      event = named_key_event(FGOF_KEY_DELETE, modifiers, sequence, .true.)
    case (4, 8)
      event = named_key_event(FGOF_KEY_END, modifiers, sequence, .true.)
    case (5)
      event = named_key_event(FGOF_KEY_PAGEUP, modifiers, sequence, .true.)
    case (6)
      event = named_key_event(FGOF_KEY_PAGEDOWN, modifiers, sequence, .true.)
    case (11)
      event = named_key_event(FGOF_KEY_F1, modifiers, sequence, .true.)
    case (12)
      event = named_key_event(FGOF_KEY_F2, modifiers, sequence, .true.)
    case (13)
      event = named_key_event(FGOF_KEY_F3, modifiers, sequence, .true.)
    case (14)
      event = named_key_event(FGOF_KEY_F4, modifiers, sequence, .true.)
    case (15)
      event = named_key_event(FGOF_KEY_F5, modifiers, sequence, .true.)
    case (17)
      event = named_key_event(FGOF_KEY_F6, modifiers, sequence, .true.)
    case (18)
      event = named_key_event(FGOF_KEY_F7, modifiers, sequence, .true.)
    case (19)
      event = named_key_event(FGOF_KEY_F8, modifiers, sequence, .true.)
    case (20)
      event = named_key_event(FGOF_KEY_F9, modifiers, sequence, .true.)
    case (21)
      event = named_key_event(FGOF_KEY_F10, modifiers, sequence, .true.)
    case default
      event = unknown_key_event(sequence, escape_sequence=.true.)
    end select
  end function decode_csi_tilde_sequence

  pure integer function first_csi_parameter(parameters)
    character(len=*), intent(in) :: parameters

    integer :: i
    integer :: digit

    first_csi_parameter = -1
    if (len(parameters) == 0) return

    first_csi_parameter = 0
    do i = 1, len(parameters)
      if (parameters(i:i) == ";") exit
      digit = iachar(parameters(i:i)) - iachar("0")
      if (digit < 0 .or. digit > 9) then
        first_csi_parameter = -1
        return
      end if
      first_csi_parameter = first_csi_parameter * 10 + digit
    end do
  end function first_csi_parameter

  pure integer function second_csi_parameter(parameters)
    character(len=*), intent(in) :: parameters

    integer :: i
    integer :: digit
    logical :: after_separator

    second_csi_parameter = -1
    if (len(parameters) == 0) return

    after_separator = .false.
    do i = 1, len(parameters)
      if (.not. after_separator) then
        if (parameters(i:i) == ";") then
          after_separator = .true.
          second_csi_parameter = 0
        end if
        cycle
      end if

      if (parameters(i:i) == ";") exit
      digit = iachar(parameters(i:i)) - iachar("0")
      if (digit < 0 .or. digit > 9) then
        second_csi_parameter = -1
        return
      end if
      second_csi_parameter = second_csi_parameter * 10 + digit
    end do
  end function second_csi_parameter

  pure function csi_modifiers(parameters) result(modifiers)
    character(len=*), intent(in) :: parameters
    type(key_modifiers) :: modifiers

    integer :: code

    code = second_csi_parameter(parameters)
    select case (code)
    case (2)
      modifiers%shift = .true.
    case (3)
      modifiers%alt = .true.
    case (4)
      modifiers%shift = .true.
      modifiers%alt = .true.
    case (5)
      modifiers%ctrl = .true.
    case (6)
      modifiers%shift = .true.
      modifiers%ctrl = .true.
    case (7)
      modifiers%alt = .true.
      modifiers%ctrl = .true.
    case (8)
      modifiers%shift = .true.
      modifiers%alt = .true.
      modifiers%ctrl = .true.
    case (9)
      modifiers%super = .true.
    case (10)
      modifiers%shift = .true.
      modifiers%super = .true.
    case (11)
      modifiers%alt = .true.
      modifiers%super = .true.
    case (12)
      modifiers%shift = .true.
      modifiers%alt = .true.
      modifiers%super = .true.
    case (13)
      modifiers%ctrl = .true.
      modifiers%super = .true.
    case (14)
      modifiers%shift = .true.
      modifiers%ctrl = .true.
      modifiers%super = .true.
    case (15)
      modifiers%alt = .true.
      modifiers%ctrl = .true.
      modifiers%super = .true.
    case (16)
      modifiers%shift = .true.
      modifiers%alt = .true.
      modifiers%ctrl = .true.
      modifiers%super = .true.
    end select
  end function csi_modifiers

  pure function decode_ss3_sequence(sequence) result(event)
    character(len=*), intent(in) :: sequence
    type(key_event) :: event

    select case (sequence(3:3))
    case ("A")
      event = named_key_event(FGOF_KEY_UP, raw_bytes=sequence, escape_sequence=.true.)
    case ("B")
      event = named_key_event(FGOF_KEY_DOWN, raw_bytes=sequence, escape_sequence=.true.)
    case ("C")
      event = named_key_event(FGOF_KEY_RIGHT, raw_bytes=sequence, escape_sequence=.true.)
    case ("D")
      event = named_key_event(FGOF_KEY_LEFT, raw_bytes=sequence, escape_sequence=.true.)
    case ("F")
      event = named_key_event(FGOF_KEY_END, raw_bytes=sequence, escape_sequence=.true.)
    case ("H")
      event = named_key_event(FGOF_KEY_HOME, raw_bytes=sequence, escape_sequence=.true.)
    case ("P")
      event = named_key_event(FGOF_KEY_F1, raw_bytes=sequence, escape_sequence=.true.)
    case ("Q")
      event = named_key_event(FGOF_KEY_F2, raw_bytes=sequence, escape_sequence=.true.)
    case ("R")
      event = named_key_event(FGOF_KEY_F3, raw_bytes=sequence, escape_sequence=.true.)
    case ("S")
      event = named_key_event(FGOF_KEY_F4, raw_bytes=sequence, escape_sequence=.true.)
    case default
      event = unknown_key_event(sequence, escape_sequence=.true.)
    end select
  end function decode_ss3_sequence

  function decode_bracketed_paste_or_incomplete(state) result(event)
    type(key_decoder_state), intent(inout) :: state
    type(key_event) :: event

    character(len=*), parameter :: start_marker = achar(27) // "[200~"
    character(len=*), parameter :: end_marker = achar(27) // "[201~"
    integer :: marker_index
    integer :: payload_start
    character(len=:), allocatable :: payload
    character(len=:), allocatable :: raw_bytes

    marker_index = index(state%pending_bytes, end_marker)
    if (marker_index == 0) then
      event = incomplete_paste_event(state%pending_bytes)
      return
    end if

    payload_start = len(start_marker) + 1
    if (marker_index > payload_start) then
      payload = state%pending_bytes(payload_start:marker_index - 1)
    else
      payload = ""
    end if
    raw_bytes = state%pending_bytes(:marker_index + len(end_marker) - 1)

    event = paste_key_event(payload, raw_bytes)
    call consume_pending_bytes(state, len(raw_bytes))
    state%bracketed_paste_active = .false.
    state%escape_pending = .false.
  end function decode_bracketed_paste_or_incomplete

end module fgof_keys

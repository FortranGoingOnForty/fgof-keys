module fgof_keys_types
  implicit none
  private

  public :: FGOF_KEY_EVENT_NAMED
  public :: FGOF_KEY_EVENT_NONE
  public :: FGOF_KEY_EVENT_PASTE
  public :: FGOF_KEY_EVENT_PRINTABLE
  public :: FGOF_KEY_EVENT_UNKNOWN
  public :: FGOF_KEY_BACKSPACE
  public :: FGOF_KEY_DELETE
  public :: FGOF_KEY_DOWN
  public :: FGOF_KEY_END
  public :: FGOF_KEY_ENTER
  public :: FGOF_KEY_ESCAPE
  public :: FGOF_KEY_F1
  public :: FGOF_KEY_F2
  public :: FGOF_KEY_F3
  public :: FGOF_KEY_F4
  public :: FGOF_KEY_HOME
  public :: FGOF_KEY_INSERT
  public :: FGOF_KEY_LEFT
  public :: FGOF_KEY_PAGEDOWN
  public :: FGOF_KEY_PAGEUP
  public :: FGOF_KEY_RIGHT
  public :: FGOF_KEY_TAB
  public :: FGOF_KEY_UP
  public :: key_decoder_state
  public :: key_event
  public :: key_modifiers

  integer, parameter :: FGOF_KEY_EVENT_NONE = 0
  integer, parameter :: FGOF_KEY_EVENT_PRINTABLE = 1
  integer, parameter :: FGOF_KEY_EVENT_NAMED = 2
  integer, parameter :: FGOF_KEY_EVENT_PASTE = 3
  integer, parameter :: FGOF_KEY_EVENT_UNKNOWN = 4

  character(len=*), parameter :: FGOF_KEY_UP = "up"
  character(len=*), parameter :: FGOF_KEY_DOWN = "down"
  character(len=*), parameter :: FGOF_KEY_LEFT = "left"
  character(len=*), parameter :: FGOF_KEY_RIGHT = "right"
  character(len=*), parameter :: FGOF_KEY_HOME = "home"
  character(len=*), parameter :: FGOF_KEY_END = "end"
  character(len=*), parameter :: FGOF_KEY_PAGEUP = "pageup"
  character(len=*), parameter :: FGOF_KEY_PAGEDOWN = "pagedown"
  character(len=*), parameter :: FGOF_KEY_ENTER = "enter"
  character(len=*), parameter :: FGOF_KEY_ESCAPE = "escape"
  character(len=*), parameter :: FGOF_KEY_TAB = "tab"
  character(len=*), parameter :: FGOF_KEY_BACKSPACE = "backspace"
  character(len=*), parameter :: FGOF_KEY_INSERT = "insert"
  character(len=*), parameter :: FGOF_KEY_DELETE = "delete"
  character(len=*), parameter :: FGOF_KEY_F1 = "f1"
  character(len=*), parameter :: FGOF_KEY_F2 = "f2"
  character(len=*), parameter :: FGOF_KEY_F3 = "f3"
  character(len=*), parameter :: FGOF_KEY_F4 = "f4"

  type :: key_modifiers
    logical :: shift = .false.
    logical :: alt = .false.
    logical :: ctrl = .false.
    logical :: super = .false.
  end type key_modifiers

  type :: key_event
    integer :: kind = FGOF_KEY_EVENT_NONE
    character(len=:), allocatable :: text
    character(len=:), allocatable :: key_name
    character(len=:), allocatable :: raw_bytes
    type(key_modifiers) :: modifiers
    logical :: recognized = .false.
    logical :: printable = .false.
    logical :: escape_sequence = .false.
    logical :: paste = .false.
    logical :: incomplete = .false.
  end type key_event

  type :: key_decoder_state
    character(len=:), allocatable :: pending_bytes
    logical :: escape_pending = .false.
    logical :: bracketed_paste_active = .false.
  end type key_decoder_state

end module fgof_keys_types

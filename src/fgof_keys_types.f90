module fgof_keys_types
  implicit none
  private

  public :: FGOF_KEY_BACKSPACE
  public :: FGOF_KEY_DELETE
  public :: FGOF_KEY_DOWN
  public :: FGOF_KEY_END
  public :: FGOF_KEY_ENTER
  public :: FGOF_KEY_ESCAPE
  public :: FGOF_KEY_HOME
  public :: FGOF_KEY_LEFT
  public :: FGOF_KEY_PAGEDOWN
  public :: FGOF_KEY_PAGEUP
  public :: FGOF_KEY_RIGHT
  public :: FGOF_KEY_TAB
  public :: FGOF_KEY_UP
  public :: key_event
  public :: key_modifiers

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
  character(len=*), parameter :: FGOF_KEY_DELETE = "delete"

  type :: key_modifiers
    logical :: shift = .false.
    logical :: alt = .false.
    logical :: ctrl = .false.
    logical :: super = .false.
  end type key_modifiers

  type :: key_event
    character(len=:), allocatable :: text
    character(len=:), allocatable :: key_name
    type(key_modifiers) :: modifiers
    logical :: recognized = .false.
    logical :: printable = .false.
    logical :: escape_sequence = .false.
  end type key_event

end module fgof_keys_types

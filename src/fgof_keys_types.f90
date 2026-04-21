module fgof_keys_types
  implicit none
  private

  public :: key_event
  public :: key_modifiers

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

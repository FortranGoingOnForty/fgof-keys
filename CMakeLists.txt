cmake_minimum_required(VERSION 3.25)

project(fgof_keys VERSION 0.1.0 LANGUAGES Fortran)

add_library(fgof_keys
  src/fgof_keys_types.f90
  src/fgof_keys.f90
)
add_library(fgof::keys ALIAS fgof_keys)

set_target_properties(fgof_keys PROPERTIES
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/mod"
)

target_include_directories(fgof_keys PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
)

target_compile_options(fgof_keys PRIVATE
  $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-std=f2018 -Wall -Wextra -Wpedantic -Werror>
)

include(CTest)

if(BUILD_TESTING)
  foreach(test_name IN ITEMS scaffold decode_core decode_sequences editor_helpers)
    add_executable(fgof_keys_${test_name}_test test/test_${test_name}.f90)
    target_link_libraries(fgof_keys_${test_name}_test PRIVATE fgof_keys)
    target_compile_options(fgof_keys_${test_name}_test PRIVATE
      $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-std=f2018 -Wall -Wextra -Wpedantic -Werror>
    )
    add_test(NAME fgof_keys_${test_name} COMMAND fgof_keys_${test_name}_test)
  endforeach()
endif()

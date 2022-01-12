add_custom_target(generate_version_h COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${CMAKE_CURRENT_SOURCE_DIR}"
  -Dinput_file="${CMAKE_CURRENT_SOURCE_DIR}/version.h.in"
  -Doutput_file="${CMAKE_CURRENT_BINARY_DIR}/version.h"
  -P "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/version.cmake"
)

find_program(LUA51DLL lua51.dll REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)
add_custom_target(generate_lua51dl COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${CMAKE_CURRENT_BINARY_DIR}"
  -Ddll="${LUA51DLL}"
  -Ddef="${CMAKE_CURRENT_BINARY_DIR}/lua51.def"
  -Dlib="${CMAKE_CURRENT_BINARY_DIR}/liblua51_dynamic.a"
  -Ddelayed_lib="${CMAKE_CURRENT_BINARY_DIR}/liblua51_delayed.a"
  -P "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/makelib.cmake"
)

find_program(CLANG_FORMAT_EXE clang-format)
file(GLOB sources "*.c" "*.h")
add_custom_target(${PROJECT_NAME}-format ALL
  COMMAND ${CLANG_FORMAT_EXE} -style=file -i ${sources}
)

option(LEAK_DETECTOR "" ON)
option(ALLOCATE_LOGGER "" ON)
add_subdirectory(3rd/ovbase)

include_directories(
  "${ovbase_SOURCE_DIR}/include"
  "${CMAKE_CURRENT_BINARY_DIR}" # for version.h
)
link_directories(
  "${CMAKE_CURRENT_BINARY_DIR}" # for lua51dl.a
)

add_compile_definitions(
  __STDC_NO_THREADS__
  _WIN32_WINNT=0x0502
  _WINDOWS
  USE_STR
  USE_WSTR
  $<$<BOOL:${LEAK_DETECTOR}>:LEAK_DETECTOR>
  $<$<BOOL:${ALLOCATE_LOGGER}>:ALLOCATE_LOGGER>
  $<$<CONFIG:Release>:NDEBUG>
)
add_compile_options(
  --rtlib=compiler-rt
  -mstackrealign
  -Wall
  -Wextra
  -Werror
  -Weverything
  -Wshadow
  -Werror=return-type
  -pedantic-errors
  -ffunction-sections
  -fdata-sections
  $<$<CONFIG:Debug>:-O0>
  $<$<CONFIG:Release>:-O2>
)
add_link_options(
  -Wl,--gc-sections
  # -Wl,--print-gc-sections
  --rtlib=compiler-rt
  -no-pthread
  -Wl,--kill-at
  $<$<CONFIG:Release>:-s>
)

add_library(psdtoolkit_assist SHARED
  assist.c
  assist.rc
  aviutl.c
  cache.c
  error_ptk.c
  find.c
  ipc.c
  luafuncs.c
  luastr.c
  popupmenu.c
  ptklayer.c
  setting.c
  speak.c
  util.c
)
set_target_properties(psdtoolkit_assist PROPERTIES
  OUTPUT_NAME "PSDToolKit.auf"
  PREFIX ""
  SUFFIX ""
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)
add_dependencies(psdtoolkit_assist generate_version_h generate_lua51dl)
target_link_libraries(psdtoolkit_assist PRIVATE
  lua51_delayed
  ovbase
)

add_library(psdtoolkit_bridge SHARED
  bridge.c
)
set_target_properties(psdtoolkit_bridge PROPERTIES
  OUTPUT_NAME "PSDToolKitBridge.dll"
  PREFIX ""
  SUFFIX ""
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin/script/PSDToolKit"
)
target_link_libraries(psdtoolkit_bridge PRIVATE
  lua51_dynamic
)
add_dependencies(psdtoolkit_bridge generate_lua51dl)

option(USE_COMPILER_RT "use compiler-rt runtime" OFF)
cmake_minimum_required(VERSION 3.10)
project(psdtoolkit_c C)
enable_language(RC)

find_program(CLANG_FORMAT_EXE clang-format)
file(GLOB sources "${CMAKE_CURRENT_SOURCE_DIR}/*.c" "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
add_custom_target(${PROJECT_NAME}_format
  COMMAND ${CLANG_FORMAT_EXE} -style=file -i ${sources}
)

add_subdirectory(3rd/ovbase)
add_subdirectory(3rd/ovutil)

add_custom_target(${PROJECT_NAME}_generate_version_h SOURCES
  ${CMAKE_CURRENT_BINARY_DIR}/version.h
)
add_custom_command(
OUTPUT
  ${CMAKE_CURRENT_BINARY_DIR}/version.h
COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${CMAKE_CURRENT_SOURCE_DIR}"
  -Dinput_file="${CMAKE_CURRENT_SOURCE_DIR}/version.h.in"
  -Doutput_file="${CMAKE_CURRENT_BINARY_DIR}/version.h"
  -P "${ovutil_SOURCE_DIR}/src/cmake/version.cmake"
DEPENDS
  ${CMAKE_CURRENT_SOURCE_DIR}/version.h.in
)

find_program(LUA51DLL lua51.dll CMAKE_FIND_ROOT_PATH_BOTH)
add_custom_target(generate_importlib SOURCES
  ${CMAKE_CURRENT_BINARY_DIR}/lua51.def
  ${CMAKE_CURRENT_BINARY_DIR}/liblua51.a
)
add_custom_command(
OUTPUT
  ${CMAKE_CURRENT_BINARY_DIR}/lua51.def
  ${CMAKE_CURRENT_BINARY_DIR}/liblua51.a
COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${CMAKE_CURRENT_BINARY_DIR}"
  -Ddll="${LUA51DLL}"
  -Ddef="${CMAKE_CURRENT_BINARY_DIR}/lua51.def"
  -Dlib="${CMAKE_CURRENT_BINARY_DIR}/liblua51.a"
  -P "${ovutil_SOURCE_DIR}/src/cmake/makelib-llvm.cmake"
DEPENDS
  ${LUA51DLL}
)

set(is_clang "$<C_COMPILER_ID:Clang>")
set(v16_or_later "$<VERSION_GREATER_EQUAL:$<C_COMPILER_VERSION>,16>")
add_library(psdtoolkit_intf INTERFACE)
target_include_directories(psdtoolkit_intf INTERFACE
  "${CMAKE_CURRENT_BINARY_DIR}" # for version.h
)
target_link_directories(psdtoolkit_intf INTERFACE
  "${CMAKE_CURRENT_BINARY_DIR}" # for liblua51_*.a
)
target_compile_definitions(psdtoolkit_intf INTERFACE
  $<$<BOOL:${WIN32}>:_WIN32_WINNT=0x0502>
  _WINDOWS
  $<$<CONFIG:Release>:NDEBUG>
)
target_compile_options(psdtoolkit_intf INTERFACE
  $<$<AND:$<BOOL:${WIN32}>,$<BOOL:${USE_COMPILER_RT}>>:--rtlib=compiler-rt>
  -mstackrealign
  -Wall
  -Wextra
  -Werror
  -Weverything
  -Wshadow
  -Werror=return-type
  -pedantic-errors
  -Wno-declaration-after-statement
  -Wno-padded
  $<$<AND:${is_clang},${v16_or_later}>:-Wno-unsafe-buffer-usage>
  -ffunction-sections
  -fdata-sections
  $<$<CONFIG:Debug>:-O0>
  $<$<CONFIG:Release>:-O2>
  -flto
)
target_link_options(psdtoolkit_intf INTERFACE
  -fuse-ld=lld
  -Wl,--gc-sections
  # -Wl,--print-gc-sections
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
)
set_target_properties(psdtoolkit_assist PROPERTIES
  OUTPUT_NAME "PSDToolKit.auf"
  PREFIX ""
  SUFFIX ""
  RUNTIME_OUTPUT_DIRECTORY "${EXPORT_DIR}"
)
add_dependencies(psdtoolkit_assist ${PROJECT_NAME}_format ${PROJECT_NAME}_generate_version_h generate_importlib)
target_link_libraries(psdtoolkit_assist PRIVATE
  psdtoolkit_intf
  lua51
  -Wl,-delayload,lua51.dll
  ovbase
  ovutil
)

add_library(psdtoolkit_bridge SHARED
  bridge.c
)
set_target_properties(psdtoolkit_bridge PROPERTIES
  OUTPUT_NAME "PSDToolKitBridge.dll"
  PREFIX ""
  SUFFIX ""
  RUNTIME_OUTPUT_DIRECTORY "${EXPORT_DIR}/script/PSDToolKit"
)
add_dependencies(psdtoolkit_bridge ${PROJECT_NAME}_format generate_importlib)
target_link_libraries(psdtoolkit_bridge PRIVATE
  psdtoolkit_intf
  lua51
)

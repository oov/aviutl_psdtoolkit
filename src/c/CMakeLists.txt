option(USE_COMPILER_RT "use compiler-rt runtime" OFF)
cmake_minimum_required(VERSION 3.10)
project(psdtoolkit_c C)
enable_language(RC)
enable_testing()

find_program(CLANG_FORMAT_EXE clang-format)
file(GLOB_RECURSE sources LIST_DIRECTORIES false CONFIGURE_DEPENDS "*.c" "*.h")
list(FILTER sources EXCLUDE REGEX "${CMAKE_CURRENT_SOURCE_DIR}/3rd")
add_custom_target(${PROJECT_NAME}-format ALL
  COMMAND ${CLANG_FORMAT_EXE} -style=file -i ${sources}
)

add_subdirectory(3rd/ovbase)
add_subdirectory(3rd/ovutil)

set(LOCAL_INSTALL_PREFIX ${CMAKE_CURRENT_BINARY_DIR}/local)
file(MAKE_DIRECTORY ${LOCAL_INSTALL_PREFIX}/include)
include(ExternalProject)
ExternalProject_Add(budoux-c-proj
  GIT_REPOSITORY https://github.com/oov/budoux-c
  GIT_TAG 67d89ea8fd62bb798e35febc18d33ef73d19fa3c
  PREFIX "${LOCAL_INSTALL_PREFIX}/proj/budoux-c"
  SOURCE_DIR "${LOCAL_INSTALL_PREFIX}/src/budoux-c"
  CMAKE_ARGS "-DCMAKE_INSTALL_PREFIX=${LOCAL_INSTALL_PREFIX}" "-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}" -DBUILD_SHARED_LIBS=OFF
  BINARY_DIR "${LOCAL_INSTALL_PREFIX}/build/budoux-c"
  BUILD_BYPRODUCTS "${LOCAL_INSTALL_PREFIX}/lib/libbudouxc.a"
  UPDATE_COMMAND ""
)

add_library(budoux-c STATIC IMPORTED)
set_target_properties(budoux-c PROPERTIES
IMPORTED_LOCATION
  ${LOCAL_INSTALL_PREFIX}/lib/libbudouxc.a
INTERFACE_INCLUDE_DIRECTORIES
  ${LOCAL_INSTALL_PREFIX}/include
)
add_dependencies(budoux-c budoux-c-proj)

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
  # WW_DEBUG
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
  $<$<BOOL:${USE_ADDRESS_SANITIZER}>:-fsanitize=address>
  -flto
)
target_link_options(psdtoolkit_intf INTERFACE
  -fuse-ld=lld
  -Wl,--gc-sections
  # -Wl,--print-gc-sections
  -Wl,--kill-at
  $<$<BOOL:${USE_ADDRESS_SANITIZER}>:-fsanitize=address>
  $<$<CONFIG:Release>:-s>
)
add_dependencies(psdtoolkit_intf ${PROJECT_NAME}_format generate_importlib budoux-c)

add_subdirectory(wordwrap)

add_library(psdtoolkit_assist SHARED
  assist.c
  assist.rc
  aviutl.c
  cache.c
  error_ptk.c
  find.c
  ipc.c
  luafuncs.c
  luafuncs_wordwrap.c
  luautil.c
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
add_dependencies(psdtoolkit_assist ${PROJECT_NAME}_generate_version_h)
target_link_libraries(psdtoolkit_assist PRIVATE
  psdtoolkit_intf
  lua51
  -Wl,-delayload,lua51.dll
  ovbase
  ovutil
  budoux-c
  wordwrap
)

add_executable(test_luafuncs luafuncs_wordwrap_test.c aviutl.c luautil.c)
target_link_libraries(test_luafuncs PRIVATE
  psdtoolkit_intf
  lua51
  ovbase
  ovutil
  budoux-c
  wordwrap
)
add_test(NAME test_luafuncs COMMAND test_luafuncs)

add_library(psdtoolkit_bridge SHARED
  bridge.c
)
set_target_properties(psdtoolkit_bridge PROPERTIES
  OUTPUT_NAME "PSDToolKitBridge.dll"
  PREFIX ""
  SUFFIX ""
  RUNTIME_OUTPUT_DIRECTORY "${EXPORT_DIR}/script/PSDToolKit"
)
add_dependencies(psdtoolkit_bridge generate_importlib)
target_link_libraries(psdtoolkit_bridge PRIVATE
  psdtoolkit_intf
  lua51
)

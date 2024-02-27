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
  GIT_TAG 6931d394fe1c36c9778d9db8c9238e3a030e302c
  PREFIX "${LOCAL_INSTALL_PREFIX}/proj/budoux-c"
  SOURCE_DIR "${LOCAL_INSTALL_PREFIX}/src/budoux-c"
  CMAKE_ARGS "-DCMAKE_INSTALL_PREFIX=${LOCAL_INSTALL_PREFIX}" "-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}" -DBUILD_SHARED_LIBS=OFF -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
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

set(LUA51_PLATFORM i686)
set(LUA51_URL "https://github.com/oov/lua-5.1.5/releases/download/v5.1.5/lua_v5.1.5_${LUA51_PLATFORM}.zip")
string(REGEX MATCH "[^/]+$" LUA51_ARCHIVE_NAME "${LUA51_URL}")
set(LUA51_ARCHIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}/${LUA51_ARCHIVE_NAME}")
set(LUA51_DIR "${CMAKE_CURRENT_BINARY_DIR}/lua51")
set(LUA51_DLL "${LUA51_DIR}/bin/lua51.dll")
set(LUA51_INCLUDE "${LUA51_DIR}/include")
if(NOT EXISTS "${LUA51_DIR}")
  if(NOT EXISTS "${LUA51_ARCHIVE_PATH}")
    file(DOWNLOAD "${LUA51_URL}" "${LUA51_ARCHIVE_PATH}")
  endif()
  string(REGEX REPLACE "\\.[^.]+$" "" LUA51_ARCHIVE_NOEXT "${LUA51_ARCHIVE_NAME}")
  file(MAKE_DIRECTORY "${LUA51_DIR}")
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E tar xf ${LUA51_ARCHIVE_PATH}
    WORKING_DIRECTORY "${LUA51_DIR}"
  )
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E copy "${LUA51_DLL}" "${CMAKE_CURRENT_BINARY_DIR}/lua51.dll"
  )
endif()

set(is_clang "$<C_COMPILER_ID:Clang>")
set(v16_or_later "$<VERSION_GREATER_EQUAL:$<C_COMPILER_VERSION>,16>")
add_library(psdtoolkit_intf INTERFACE)
target_include_directories(psdtoolkit_intf INTERFACE
  "${CMAKE_CURRENT_BINARY_DIR}" # for version.h
  "${LUA51_INCLUDE}"
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
add_dependencies(psdtoolkit_intf ${PROJECT_NAME}_format budoux-c)

add_subdirectory(wordwrap)
add_subdirectory(kerning)

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
  "${LUA51_DLL}"
  -Wl,-delayload,lua51.dll
  ovbase
  ovutil
  budoux-c
  kerning
  wordwrap
)

add_executable(test_luafuncs luafuncs_wordwrap_test.c aviutl.c luautil.c)
target_link_libraries(test_luafuncs PRIVATE
  psdtoolkit_intf
  "${LUA51_DLL}"
  ovbase
  ovutil
  budoux-c
  kerning
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
target_link_libraries(psdtoolkit_bridge PRIVATE
  psdtoolkit_intf
  "${LUA51_DLL}"
)

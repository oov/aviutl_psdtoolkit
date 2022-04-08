cmake_minimum_required(VERSION 3.0)
project(psdtoolkit_go NONE)

add_custom_target(${PROJECT_NAME}_format
COMMAND ${CMAKE_COMMAND} -E env GOROOT="$ENV{MINGW_PREFIX}/lib/go" go fmt
WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
USES_TERMINAL
)

find_package(Git REQUIRED)
execute_process(
  COMMAND ${GIT_EXECUTABLE} tag --points-at HEAD
  OUTPUT_VARIABLE _git_tag
  ERROR_QUIET
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
if ("${_git_tag}" STREQUAL "")
  set(_git_tag "vX.X.X")
endif()
message(STATUS "git tag: ${_git_tag}")

execute_process(
  COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
  OUTPUT_VARIABLE _git_revision
  ERROR_QUIET
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
if ("${_git_revision}" STREQUAL "")
  set(_git_revision "unknown")
endif()
message(STATUS "git revision: ${_git_revision}")

add_custom_target(psdtoolkit_main
  COMMAND windres
    --output-format=coff
    -o "${CMAKE_CURRENT_SOURCE_DIR}/PSDToolKit.syso"
    "${CMAKE_CURRENT_SOURCE_DIR}/PSDToolKit.rc"
  COMMAND ${CMAKE_COMMAND} -E env GOROOT="$ENV{MINGW_PREFIX}/lib/go" go build -x
    -tags gdip
    "-ldflags=\"-s -X \\\"main.version=${_git_tag} ( ${_git_revision} )\\\"\""
    -o "${EXPORT_DIR}/script/PSDToolKit/PSDToolKit.exe"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  USES_TERMINAL
)
add_dependencies(psdtoolkit_main ${PROJECT_NAME}_format)

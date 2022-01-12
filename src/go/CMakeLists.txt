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
    -o PSDToolKit.syso
    PSDToolKit.rc
  COMMAND go build
    -tags gdip
    "-ldflags=\"-s -X \\\"main.version=${_git_tag} ( ${_git_revision} )\\\"\""
    -o "${CMAKE_BINARY_DIR}/bin/script/PSDToolKit/PSDToolKit.exe"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
)

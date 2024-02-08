cmake_minimum_required(VERSION 3.10)
project(psdtoolkit_go NONE)
enable_language(C)
enable_language(RC)

set(GO_DIR "${CMAKE_CURRENT_BINARY_DIR}/go")
if (CMAKE_HOST_WIN32)
  set(GO_URL "https://go.dev/dl/go1.20.14.windows-amd64.zip")
  set(GO_EXE "${GO_DIR}/bin/go.exe")
else()
  set(GO_URL "https://go.dev/dl/go1.20.14.linux-amd64.tar.gz")
  set(GO_EXE "${GO_DIR}/bin/go")
endif()
string(REGEX MATCH "[^/]+$" GO_ARCHIVE_NAME "${GO_URL}")
set(GO_ARCHIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}/${GO_ARCHIVE_NAME}")
string(REGEX REPLACE "\\.[^.]+$" "" GO_ARCHIVE_NOEXT "${GO_ARCHIVE_NAME}")
if(NOT EXISTS "${GO_ARCHIVE_PATH}")
  file(DOWNLOAD "${GO_URL}" "${GO_ARCHIVE_PATH}")
endif()
if(NOT EXISTS "${GO_DIR}")
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E tar xf ${GO_ARCHIVE_PATH}
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  )
endif()

add_custom_target(${PROJECT_NAME}_format
COMMAND ${CMAKE_COMMAND} -E env GOROOT="${GO_DIR}" ${GO_EXE} fmt
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

add_test(NAME jobqueue COMMAND ${CMAKE_COMMAND} -E env "${GO_EXE}" test WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/jobqueue")
add_test(NAME img COMMAND ${CMAKE_COMMAND} -E env "${GO_EXE}" test WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/img")
add_test(NAME img_prop COMMAND ${CMAKE_COMMAND} -E env "${GO_EXE}" test WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/img/prop")
add_test(NAME img_internal_packbits COMMAND ${CMAKE_COMMAND} -E env "${GO_EXE}" test WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/img/internal/packbits")

add_library(psdtoolkit_go_rc OBJECT PSDToolKit.rc)
add_custom_target(psdtoolkit_main ALL
  COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_OBJECTS:psdtoolkit_go_rc> "${CMAKE_CURRENT_SOURCE_DIR}/PSDToolKit.syso"
  COMMAND ${CMAKE_COMMAND} -E env
    CC="${CMAKE_C_COMPILER}"
    GOROOT="${GO_DIR}" GOOS="windows" GOARCH="amd64" CGO_ENABLED="1"
    "${GO_EXE}" build -x -tags gdip
    -ldflags -s\ -X\ main.gitTag=${_git_tag}\ -X\ main.gitRevision=${_git_revision}
    -o "${EXPORT_DIR}/script/PSDToolKit/PSDToolKit.exe"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  USES_TERMINAL
)
add_dependencies(psdtoolkit_main ${PROJECT_NAME}_format psdtoolkit_go_rc)

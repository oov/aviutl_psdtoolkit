cmake_minimum_required(VERSION 3.20)

string(REGEX MATCH "[^/]+$" FILENAME "${url}")
set(ZIP_PATH "${local_dir}/${FILENAME}")
if(NOT EXISTS "${ZIP_PATH}")
  file(DOWNLOAD "${url}" "${ZIP_PATH}")
endif()
file(REMOVE_RECURSE "${local_dir}/${dir}")
file(MAKE_DIRECTORY "${local_dir}/${dir}")

if ("${charset}" STREQUAL "sjis")
  # Special considerations are required when extracting archives
  # with filenames not encoded in UTF-8 on non-Japanese OS environments,
  # such as GitHub Actions.
  # Each environment needs to use a tool that allows specifying the character encoding.
  if (CMAKE_HOST_WIN32)
    # On Windows, use 7za.exe to specify the code page.
    find_program(SEVENZIP 7za REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)
    execute_process(
      COMMAND ${SEVENZIP} x -mcp=932 ${ZIP_PATH}
      WORKING_DIRECTORY "${local_dir}/${dir}"
    )
  else()
    # On Linux, specify the encoding with the unzip option,
    # but this option may not be supported on systems other than Ubuntu.
    find_program(UNZIP unzip REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)
    execute_process(
      COMMAND ${UNZIP} -O cp932 ${ZIP_PATH}
      WORKING_DIRECTORY "${local_dir}/${dir}"
    )
  endif()
else()
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E tar xzvf ${ZIP_PATH}
    WORKING_DIRECTORY "${local_dir}/${dir}"
  )
endif()

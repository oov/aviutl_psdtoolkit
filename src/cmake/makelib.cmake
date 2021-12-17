cmake_minimum_required(VERSION 3.0.0)

find_program(GENDEF gendef REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)
find_program(DLLTOOL dlltool REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)

execute_process(
  COMMAND ${GENDEF} - ${dll}
  WORKING_DIRECTORY "${local_dir}"
  OUTPUT_FILE ${def}
  ERROR_QUIET
)

execute_process(
  COMMAND ${DLLTOOL} -d ${def} -D ${dll} -l ${lib} -y ${delayed_lib}
  WORKING_DIRECTORY "${local_dir}"
  ERROR_QUIET
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

cmake_minimum_required(VERSION 3.0)
project(psdtoolkit C)
enable_language(RC)
enable_testing()

find_program(CLANG_FORMAT_EXE clang-format)
file(GLOB sources "${PROJECT_SOURCE_DIR}/src/c/*.c" "${PROJECT_SOURCE_DIR}/src/c/*.h")
add_custom_target(${PROJECT_NAME}-format ALL
  COMMAND ${CLANG_FORMAT_EXE} -style=file -i ${sources}
)

add_subdirectory(src/c)
add_subdirectory(src/go)
add_subdirectory(src/docs)

add_custom_target(extract_gcmzdrops COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${PROJECT_BINARY_DIR}"
  -Durl="https://github.com/oov/aviutl_gcmzdrops/releases/download/v0.4.0beta2/gcmzdrops_v0.4.0beta2.zip"
  -Ddir="gcmzdrops"
  -P "${ovutil_SOURCE_DIR}/src/cmake/extract-zip.cmake"
)
add_custom_target(extract_audiomixer COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${PROJECT_BINARY_DIR}"
  -Durl="https://github.com/oov/aviutl_audiomixer/releases/download/v0.3.0beta1/AudioMixer_v0.3.0beta1.zip"
  -Ddir="audiomixer"
  -P "${ovutil_SOURCE_DIR}/src/cmake/extract-zip.cmake"
)
add_custom_target(extract_forcepser COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${PROJECT_BINARY_DIR}"
  -Durl="https://github.com/oov/forcepser/releases/download/v1.5/forcepser_v1.5.zip"
  -Ddir="forcepser"
  -P "${ovutil_SOURCE_DIR}/src/cmake/extract-zip.cmake"
)
add_custom_target(extract_rampreview COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${PROJECT_BINARY_DIR}"
  -Durl="https://github.com/oov/aviutl_rampreview/releases/download/v0.3rc7/rampreview_v0.3rc7.zip"
  -Ddir="rampreview"
  -P "${ovutil_SOURCE_DIR}/src/cmake/extract-zip.cmake"
)
add_custom_target(extract_cachetext COMMAND
  ${CMAKE_COMMAND}
  -Dcharset="sjis"
  -Dlocal_dir="${PROJECT_BINARY_DIR}"
  -Durl="https://github.com/oov/aviutl_cachetext/releases/download/v0.8/CacheText20210119.zip"
  -Ddir="cachetext"
  -P "${ovutil_SOURCE_DIR}/src/cmake/extract-zip.cmake"
)
add_custom_target(copy_alias
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/README.md" "${PROJECT_BINARY_DIR}/bin/PSDToolKit.txt"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/TalkDetector.exa" "${PROJECT_BINARY_DIR}/bin/PSDToolKit/口パク準備.exa"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/Phoneme.exa" "${PROJECT_BINARY_DIR}/bin/PSDToolKit/口パク準備（音素のみ）.exa"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/Subtitle.exa" "${PROJECT_BINARY_DIR}/bin/PSDToolKit/字幕表示.exa"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/SubtitleFast.exa" "${PROJECT_BINARY_DIR}/bin/PSDToolKit/字幕表示（キャッシュ）.exa"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/PrepSubtitle.exa" "${PROJECT_BINARY_DIR}/bin/PSDToolKit/字幕準備.exa"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/MultiPurposeSlider.exa" "${PROJECT_BINARY_DIR}/bin/PSDToolKit/多目的スライダー.exa"
)
add_custom_target(copy_script
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/PSDToolKit.lua" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/PSDToolKit.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/PSDToolKitIndex.lua" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/json.lua" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/json.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/@PSD.anm" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/@PSD.anm"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/@PSDToolKit.anm" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/@PSDToolKit.anm"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/@PSDToolKit.obj" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/@PSDToolKit.obj"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/@subobj.anm" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/@subobj.anm"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/default.lua" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/default.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/setting.lua-template" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/setting.lua-template"
)
add_custom_target(copy_gcmz_script
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/GCMZDrops/psd.lua" "${PROJECT_BINARY_DIR}/bin/GCMZDrops/psdtoolkit_psd.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/GCMZDrops/wav.lua" "${PROJECT_BINARY_DIR}/bin/GCMZDrops/psdtoolkit_wav.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/GCMZDrops/srt.lua" "${PROJECT_BINARY_DIR}/bin/GCMZDrops/psdtoolkit_srt.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/GCMZDrops/lab.lua" "${PROJECT_BINARY_DIR}/bin/GCMZDrops/psdtoolkit_lab.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/exa/lipsync.exa" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/exa/lipsync.exa"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/exa/subtitle.exa" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/exa/subtitle.exa"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/exa/wav.exa" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/exa/wav.exa"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/exa/srt.exa" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/exa/srt.exa"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/exa/lab.exa" "${PROJECT_BINARY_DIR}/bin/script/PSDToolKit/exa/lab.exa"
)
add_custom_target(make_package
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_BINARY_DIR}/gcmzdrops" "${PROJECT_BINARY_DIR}/bin"
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_BINARY_DIR}/audiomixer" "${PROJECT_BINARY_DIR}/bin"
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_BINARY_DIR}/forcepser" "${PROJECT_BINARY_DIR}/bin/かんしくん"
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_BINARY_DIR}/rampreview" "${PROJECT_BINARY_DIR}/bin"
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_BINARY_DIR}/cachetext" "${PROJECT_BINARY_DIR}/bin"
)
add_dependencies(make_package
  extract_gcmzdrops
  extract_audiomixer
  extract_forcepser
  extract_rampreview
  extract_cachetext
  copy_alias
  copy_script
  copy_gcmz_script
)

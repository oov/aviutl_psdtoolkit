cmake_minimum_required(VERSION 3.10)
project(psdtoolkit NONE)
enable_testing()

set(EXPORT_DIR_NAME "bin" CACHE STRING "export directory")
if ("${EXPORT_DIR_NAME}" STREQUAL "")
  set(EXPORT_DIR_NAME "bin" CACHE STRING "export directory" FORCE)
endif()
set(EXPORT_DIR "${PROJECT_BINARY_DIR}/${EXPORT_DIR_NAME}")
if("${BUILD_DLL}" STREQUAL "1")
  add_subdirectory(src/c)
endif()
if("${BUILD_EXE}" STREQUAL "1")
  add_subdirectory(src/go)
endif()
if("${BUILD_DOCS}" STREQUAL "1")
  add_subdirectory(src/docs)
endif()

if("${BUILD_PACKAGE}" STREQUAL "1")
  set(EXA_SUFFIX ".exa" CACHE STRING "suffix for *.exa file names")

  set(GCMZ_VER "v0.4.1")
  add_custom_target(extract_gcmzdrops COMMAND
    ${CMAKE_COMMAND}
    -Dlocal_dir="${PROJECT_BINARY_DIR}"
    -Durl="https://github.com/oov/aviutl_gcmzdrops/releases/download/${GCMZ_VER}/gcmzdrops_${GCMZ_VER}.zip"
    -Ddir="gcmzdrops"
    -P "${CMAKE_SOURCE_DIR}/src/cmake/extract-zip.cmake"
  )
  set(AXR_VER "v0.3.0")
  add_custom_target(extract_audiomixer COMMAND
    ${CMAKE_COMMAND}
    -Dlocal_dir="${PROJECT_BINARY_DIR}"
    -Durl="https://github.com/oov/aviutl_audiomixer/releases/download/${AXR_VER}/AudioMixer_${AXR_VER}.zip"
    -Ddir="audiomixer"
    -P "${CMAKE_SOURCE_DIR}/src/cmake/extract-zip.cmake"
  )
  set(FORCEPSER_VER "v1.6.0beta1")
  add_custom_target(extract_forcepser COMMAND
    ${CMAKE_COMMAND}
    -Dlocal_dir="${PROJECT_BINARY_DIR}"
    -Durl="https://github.com/oov/forcepser/releases/download/${FORCEPSER_VER}/forcepser_${FORCEPSER_VER}.zip"
    -Ddir="forcepser"
    -P "${CMAKE_SOURCE_DIR}/src/cmake/extract-zip.cmake"
  )
  set(RAMPREVIEW_VER "v0.3rc7")
  add_custom_target(extract_rampreview COMMAND
    ${CMAKE_COMMAND}
    -Dlocal_dir="${PROJECT_BINARY_DIR}"
    -Durl="https://github.com/oov/aviutl_rampreview/releases/download/${RAMPREVIEW_VER}/rampreview_${RAMPREVIEW_VER}.zip"
    -Ddir="rampreview"
    -P "${CMAKE_SOURCE_DIR}/src/cmake/extract-zip.cmake"
  )
  add_custom_target(extract_cachetext COMMAND
    ${CMAKE_COMMAND}
    -Dcharset="sjis"
    -Dlocal_dir="${PROJECT_BINARY_DIR}"
    -Durl="https://github.com/oov/aviutl_cachetext/releases/download/v0.8/CacheText20210119.zip"
    -Ddir="cachetext"
    -P "${CMAKE_SOURCE_DIR}/src/cmake/extract-zip.cmake"
  )
  add_custom_target(copy_alias
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/README.md" "${EXPORT_DIR}/PSDToolKit.txt"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/TalkDetector${EXA_SUFFIX}" "${EXPORT_DIR}/PSDToolKit/口パク準備.exa"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/Phoneme${EXA_SUFFIX}" "${EXPORT_DIR}/PSDToolKit/口パク準備（音素のみ）.exa"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/Subtitle${EXA_SUFFIX}" "${EXPORT_DIR}/PSDToolKit/字幕表示.exa"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/SubtitleFast${EXA_SUFFIX}" "${EXPORT_DIR}/PSDToolKit/字幕表示（キャッシュ）.exa"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/PrepSubtitle${EXA_SUFFIX}" "${EXPORT_DIR}/PSDToolKit/字幕準備.exa"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/exa/MultiPurposeSlider${EXA_SUFFIX}" "${EXPORT_DIR}/PSDToolKit/多目的スライダー.exa"
  )
  add_custom_target(copy_script
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/PSDToolKit.lua" "${EXPORT_DIR}/script/PSDToolKit/PSDToolKit.lua"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/PSDToolKitIndex.lua" "${EXPORT_DIR}/script/PSDToolKit.lua"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/json.lua" "${EXPORT_DIR}/script/PSDToolKit/json.lua"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/@PSD.anm" "${EXPORT_DIR}/script/PSDToolKit/@PSD.anm"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/@PSDToolKit.anm" "${EXPORT_DIR}/script/PSDToolKit/@PSDToolKit.anm"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/@PSDToolKit.obj" "${EXPORT_DIR}/script/PSDToolKit/@PSDToolKit.obj"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/@subobj.anm" "${EXPORT_DIR}/script/PSDToolKit/@subobj.anm"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/default.lua" "${EXPORT_DIR}/script/PSDToolKit/default.lua"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/setting.lua-template" "${EXPORT_DIR}/script/PSDToolKit/setting.lua-template"
  )
  add_custom_target(copy_gcmz_script
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/GCMZDrops/psd.lua" "${EXPORT_DIR}/GCMZDrops/psdtoolkit_psd.lua"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/GCMZDrops/wav.lua" "${EXPORT_DIR}/GCMZDrops/psdtoolkit_wav.lua"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/GCMZDrops/srt.lua" "${EXPORT_DIR}/GCMZDrops/psdtoolkit_srt.lua"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/GCMZDrops/lab.lua" "${EXPORT_DIR}/GCMZDrops/psdtoolkit_lab.lua"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/exa/lipsync${EXA_SUFFIX}" "${EXPORT_DIR}/script/PSDToolKit/exa/lipsync.exa"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/exa/subtitle${EXA_SUFFIX}" "${EXPORT_DIR}/script/PSDToolKit/exa/subtitle.exa"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/exa/wav${EXA_SUFFIX}" "${EXPORT_DIR}/script/PSDToolKit/exa/wav.exa"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/exa/srt${EXA_SUFFIX}" "${EXPORT_DIR}/script/PSDToolKit/exa/srt.exa"
    COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/exa/lab${EXA_SUFFIX}" "${EXPORT_DIR}/script/PSDToolKit/exa/lab.exa"
  )
  add_custom_target(make_package
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_BINARY_DIR}/gcmzdrops" "${EXPORT_DIR}"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_BINARY_DIR}/audiomixer" "${EXPORT_DIR}"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_BINARY_DIR}/forcepser" "${EXPORT_DIR}/かんしくん"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_BINARY_DIR}/rampreview" "${EXPORT_DIR}"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_BINARY_DIR}/cachetext" "${EXPORT_DIR}"
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
endif()

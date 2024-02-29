cmake_minimum_required(VERSION 3.20)
project(psdtoolkit_docs NONE)

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

set(version "${_git_tag} \\( ${_git_revision} \\)")

string(TIMESTAMP datetime UTC)
message(STATUS "datetime: ${datetime}")

add_custom_target(make_doc ALL
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/assets" "${EXPORT_DIR}/PSDToolKitDocs/assets"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/rootindex.html" "${EXPORT_DIR}/PSDToolKit説明書.html"
)

set(PANDOC_VERSION "3.1.11.1")
set(PANDOC_DIR "${CMAKE_CURRENT_BINARY_DIR}/pandoc-${PANDOC_VERSION}")
if (CMAKE_HOST_WIN32)
  set(PANDOC_URL "https://github.com/jgm/pandoc/releases/download/${PANDOC_VERSION}/pandoc-${PANDOC_VERSION}-windows-x86_64.zip")
  set(PANDOC_EXE "${PANDOC_DIR}/pandoc.exe")
else()
  set(PANDOC_URL "https://github.com/jgm/pandoc/releases/download/${PANDOC_VERSION}/pandoc-${PANDOC_VERSION}-linux-amd64.tar.gz")
  set(PANDOC_EXE "${PANDOC_DIR}/bin/pandoc")
endif()
string(REGEX MATCH "[^/]+$" PANDOC_ARCHIVE_NAME "${PANDOC_URL}")
set(PANDOC_ARCHIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}/${PANDOC_ARCHIVE_NAME}")
if(NOT EXISTS "${PANDOC_ARCHIVE_PATH}")
  file(DOWNLOAD "${PANDOC_URL}" "${PANDOC_ARCHIVE_PATH}")
endif()
if(NOT EXISTS "${PANDOC_DIR}")
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E tar xf ${PANDOC_ARCHIVE_PATH}
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  )
endif()

macro(add_page id title)
  if("${id}" STREQUAL "psd")
    set(additional_script "script=assets/script-builder.js")
  else()
    set(additional_script "script_unused=")
  endif()
  add_custom_target(make_doc_page_${id}
    COMMAND ${CMAKE_COMMAND} -E make_directory "${EXPORT_DIR}/PSDToolKitDocs/"
    COMMAND ${PANDOC_EXE}
    "${CMAKE_CURRENT_SOURCE_DIR}/${id}.md"
    -o "${EXPORT_DIR}/PSDToolKitDocs/${id}.html"
    -f gfm
    -M curpage=${id}
    -M title=${title}
    -M author=oov
    -M date=${datetime}
    -M target=${version}
    -M ${additional_script}
    --lua-filter "${CMAKE_CURRENT_SOURCE_DIR}/filter/remove-colgroup.lua"
    --lua-filter "${CMAKE_CURRENT_SOURCE_DIR}/filter/link-md2html.lua"
    --template="${CMAKE_CURRENT_SOURCE_DIR}/template.html"
    -s
    --toc
    --shift-heading-level-by=1
    --toc-depth=5
  )
  add_dependencies(make_doc make_doc_page_${id})
endmacro(add_page)

add_page(index "はじめに")
add_page(tutorial "チュートリアル")
add_page(decorate "テキストの装飾")
add_page(forcepser "かんしくん")
add_page(obj "独自のオブジェクト")
add_page(psd "PSD アニメーション効果")
add_page(prep "準備オブジェクト")
add_page(pfv "PSDTool のお気に入りを使う")
add_page(subobj "subobj アニメーション効果")
add_page(otheranm "その他のアニメーション効果")
add_page(plugins "付属プラグイン")
add_page(faq "よくある質問")
add_page(setting "設定")

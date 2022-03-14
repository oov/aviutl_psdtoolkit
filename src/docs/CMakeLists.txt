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

find_program(DATE_EXECUTABLE date CMAKE_FIND_ROOT_PATH_BOTH)
execute_process(COMMAND ${DATE_EXECUTABLE} "+%Y-%m-%d %H:%M:%S" OUTPUT_VARIABLE datetime OUTPUT_STRIP_TRAILING_WHITESPACE)

add_custom_target(make_doc
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/assets" "${PROJECT_BINARY_DIR}/${EXPORT_DIR}/PSDToolKitDocs/assets"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/rootindex.html" "${PROJECT_BINARY_DIR}/${EXPORT_DIR}/PSDToolKit説明書.html"
)

find_program(PANDOC_EXECUTABLE pandoc CMAKE_FIND_ROOT_PATH_BOTH)
macro(add_page id title)
  if("${id}" STREQUAL "psd")
    set(additional_script "script=assets/script-builder.js")
  else()
    set(additional_script "script_unused=")
  endif()
  add_custom_target(make_doc_page_${id}
    COMMAND ${CMAKE_COMMAND} -E make_directory "${PROJECT_BINARY_DIR}/${EXPORT_DIR}/PSDToolKitDocs/"
    COMMAND ${PANDOC_EXECUTABLE}
    "${CMAKE_CURRENT_SOURCE_DIR}/${id}.md"
    -o "${PROJECT_BINARY_DIR}/${EXPORT_DIR}/PSDToolKitDocs/${id}.html"
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
    # --shift-heading-level-by=1 # pandoc 2.8~
    --base-header-level=2
    --toc-depth=5
  )
  add_dependencies(make_doc make_doc_page_${id})
endmacro(add_page)

add_page(index "はじめに")
add_page(tutorial "チュートリアル")
add_page(tutorial2 "ツール連携入門")
add_page(forcepser "かんしくん")
add_page(obj "独自のオブジェクト")
add_page(psd "PSD アニメーション効果")
add_page(prep "準備オブジェクト")
add_page(audio "音声フィルタ")
add_page(pfv "PSDTool のお気に入りを使う")
add_page(subobj "subobj アニメーション効果")
add_page(otheranm "その他のアニメーション効果")
add_page(plugins "付属プラグイン")
add_page(faq "よくある質問")
add_page(setting "設定")

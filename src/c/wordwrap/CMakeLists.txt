add_library(wordwrap STATIC
  aviutl_text.c
  bdx.c
  bdx_cache.c
  canvas.c
  glyph.c
  line_reader.c
  rule.c
  text_cache.c
)
target_link_libraries(wordwrap PRIVATE
  psdtoolkit_intf
  ovbase
  ovutil
  budoux-c
)

add_executable(test_aviutl_text aviutl_text.c aviutl_text_test.c)
target_link_libraries(test_aviutl_text PRIVATE
  psdtoolkit_intf
  ovbase
)
add_test(NAME test_aviutl_text COMMAND test_aviutl_text)

add_library(kerning STATIC
  convexhull.c
  distance.c
  glyphoutline.c
  kerning_pairs.c
  kerning.c
  point.c
)
target_link_libraries(kerning PRIVATE
ovbase
psdtoolkit_intf
)

add_executable(kerning_glyphoutline_test glyphoutline_test.c)
target_link_libraries(kerning_glyphoutline_test PRIVATE psdtoolkit_intf ovbase kerning)
add_test(NAME kerning_glyphoutline_test COMMAND kerning_glyphoutline_test)

add_executable(kerning_convexhull_test convexhull_test.c)
target_link_libraries(kerning_convexhull_test PRIVATE psdtoolkit_intf ovbase kerning)
add_test(NAME kerning_convexhull_test COMMAND kerning_convexhull_test)

add_executable(kerning_point_test point_test.c)
target_link_libraries(kerning_point_test PRIVATE psdtoolkit_intf ovbase kerning)
add_test(NAME kerning_point_test COMMAND kerning_point_test)

add_executable(kerning_pairs_test kerning_pairs_test.c)
target_link_libraries(kerning_pairs_test PRIVATE psdtoolkit_intf ovbase kerning)
add_test(NAME kerning_pairs_test COMMAND kerning_pairs_test)

add_executable(kerning_test kerning_test.c)
target_link_libraries(kerning_test PRIVATE psdtoolkit_intf ovbase kerning)
add_test(NAME kerning_test COMMAND kerning_test)

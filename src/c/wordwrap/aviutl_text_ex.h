#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef wchar_t aviutl_text_ex_char;

enum aviutl_text_ex_tag_type {
  aviutl_text_ex_tag_type_unknown,
  aviutl_text_ex_tag_type_position,
  aviutl_text_ex_tag_type_font,
  aviutl_text_ex_tag_type_wbr,
  aviutl_text_ex_tag_type_nobr,
  aviutl_text_ex_tag_type_nobr_close,
  aviutl_text_ex_tag_type_kerning,
  aviutl_text_ex_tag_type_kerning_close,
};

struct aviutl_text_ex_tag {
  enum aviutl_text_ex_tag_type type;
  size_t pos;
  size_t len;
  size_t value_pos[3];
  size_t value_len[3];
};

bool aviutl_text_ex_parse_tag(aviutl_text_ex_char const *const str,
                              size_t const len,
                              size_t const pos,
                              struct aviutl_text_ex_tag *const tag);

enum aviutl_text_ex_tag_position_type {
  aviutl_text_ex_tag_position_type_unknown,
  aviutl_text_ex_tag_position_type_absolute,
  aviutl_text_ex_tag_position_type_relative,
};

struct aviutl_text_ex_tag_position {
  double x, y, z;
  enum aviutl_text_ex_tag_position_type x_type, y_type, z_type;
};

void aviutl_text_ex_get_position(aviutl_text_ex_char const *const str,
                                 struct aviutl_text_ex_tag const *const tag,
                                 struct aviutl_text_ex_tag_position *const value);

struct aviutl_text_ex_tag_font {
  double size;
  aviutl_text_ex_char const *name;
  size_t name_len;
  bool bold, italic;
};

void aviutl_text_ex_get_font(aviutl_text_ex_char const *const str,
                             struct aviutl_text_ex_tag const *const tag,
                             struct aviutl_text_ex_tag_font *const value);

enum aviutl_text_ex_tag_kerning_method {
  aviutl_text_ex_tag_kerning_method_unknown,
  aviutl_text_ex_tag_kerning_method_convexhull,
  aviutl_text_ex_tag_kerning_method_box,
};

struct aviutl_text_ex_tag_kerning {
  double distance;
  double margin;
  enum aviutl_text_ex_tag_kerning_method method;
};

void aviutl_text_ex_get_kerning(aviutl_text_ex_char const *const str,
                                struct aviutl_text_ex_tag const *const tag,
                                struct aviutl_text_ex_tag_kerning *const value);

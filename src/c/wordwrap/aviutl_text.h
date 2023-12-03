#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef wchar_t aviutl_text_char;

enum aviutl_text_tag_type {
  aviutl_text_tag_type_unknown,
  aviutl_text_tag_type_numcharref,
  aviutl_text_tag_type_color,
  aviutl_text_tag_type_position,
  aviutl_text_tag_type_font,
  aviutl_text_tag_type_speed,
  aviutl_text_tag_type_wait,
  aviutl_text_tag_type_clear,
  aviutl_text_tag_type_script,
};

struct aviutl_text_tag {
  enum aviutl_text_tag_type type;
  size_t pos;
  size_t len;
  size_t value_pos[3];
  size_t value_len[3];
};

bool aviutl_text_parse_tag(aviutl_text_char const *const str,
                           size_t const len,
                           size_t const pos,
                           struct aviutl_text_tag *const tag);

struct aviutl_text_tag_numcharref {
  uint16_t ch;
};

void aviutl_text_get_numcharref(aviutl_text_char const *const str,
                                struct aviutl_text_tag const *const tag,
                                struct aviutl_text_tag_numcharref *const value);

struct aviutl_text_tag_color {
  uint32_t color[2];
};

void aviutl_text_get_color(aviutl_text_char const *const str,
                           struct aviutl_text_tag const *const tag,
                           struct aviutl_text_tag_color *const value);

enum aviutl_text_tag_position_type {
  aviutl_text_tag_position_type_unknown,
  aviutl_text_tag_position_type_absolute,
  aviutl_text_tag_position_type_relative,
};

struct aviutl_text_tag_position {
  double x, y, z;
  enum aviutl_text_tag_position_type x_type, y_type, z_type;
};

void aviutl_text_get_position(aviutl_text_char const *const str,
                              struct aviutl_text_tag const *const tag,
                              struct aviutl_text_tag_position *const value);

struct aviutl_text_tag_font {
  size_t size;
  aviutl_text_char const *name;
  size_t name_len;
  bool bold, italic;
};

void aviutl_text_get_font(aviutl_text_char const *const str,
                          struct aviutl_text_tag const *const tag,
                          struct aviutl_text_tag_font *const value);

struct aviutl_text_tag_speed {
  double v;
};

void aviutl_text_get_speed(aviutl_text_char const *const str,
                           struct aviutl_text_tag const *const tag,
                           struct aviutl_text_tag_speed *const value);

struct aviutl_text_tag_wait {
  double v;
  bool per_char;
};

void aviutl_text_get_wait(aviutl_text_char const *const str,
                          struct aviutl_text_tag const *const tag,
                          struct aviutl_text_tag_wait *const value);

struct aviutl_text_tag_clear {
  double v;
  bool per_char;
};

void aviutl_text_get_clear(aviutl_text_char const *const str,
                           struct aviutl_text_tag const *const tag,
                           struct aviutl_text_tag_clear *const value);

struct aviutl_text_tag_script {
  aviutl_text_char const *ptr;
  size_t len;
};

void aviutl_text_get_script(aviutl_text_char const *const str,
                            struct aviutl_text_tag const *const tag,
                            struct aviutl_text_tag_script *const value);

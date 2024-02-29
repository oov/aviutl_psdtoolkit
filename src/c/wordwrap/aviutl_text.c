#include "aviutl_text.h"

#include <ovnum.h>

static inline bool is_sign(aviutl_text_char const c) { return c == '+' || c == '-'; }

static inline bool is_digit(aviutl_text_char const c) { return '0' <= c && c <= '9'; }

static inline bool is_xdigit(aviutl_text_char const c) {
  return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F');
}

static inline bool is_decimal(aviutl_text_char const c, bool *const found_dot) {
  if (is_digit(c)) {
    return true;
  }
  if (c == '.') {
    if (*found_dot) {
      return false;
    }
    *found_dot = true;
    return true;
  }
  return false;
}

static bool parse_script_tag(aviutl_text_char const *const str,
                             size_t const len,
                             size_t const pos,
                             struct aviutl_text_tag *const tag) {
  if (len - pos < 4) {
    return false;
  }
  for (size_t end = pos + 3; end < len; ++end) {
    if (str[end - 1] == '?' && str[end] == '>') {
      tag->type = aviutl_text_tag_type_script;
      tag->pos = pos;
      tag->len = end - pos + 1;
      tag->value_pos[0] = pos + 2;
      tag->value_pos[1] = SIZE_MAX;
      tag->value_pos[2] = SIZE_MAX;
      tag->value_len[0] = tag->len - 4;
      tag->value_len[1] = 0;
      tag->value_len[2] = 0;
      return true;
    }
  }
  return false;
}

static bool parse_num_ref(aviutl_text_char const *const str,
                          size_t const len,
                          size_t const pos,
                          struct aviutl_text_tag *const tag) {
  if (len - pos < 4) {
    return false;
  }
  for (size_t i = pos + 2; i < len; ++i) {
    if (str[i] == ';') {
      tag->type = aviutl_text_tag_type_numcharref;
      tag->pos = pos;
      tag->len = i - pos + 1;
      tag->value_pos[0] = pos + 2;
      tag->value_pos[1] = SIZE_MAX;
      tag->value_pos[2] = SIZE_MAX;
      tag->value_len[0] = i - pos - 2;
      tag->value_len[1] = 0;
      tag->value_len[2] = 0;
      return true;
    }
    if (is_digit(str[i])) {
      continue;
    }
    return false;
  }
  return false;
}

bool aviutl_text_parse_tag(aviutl_text_char const *const str,
                           size_t const len,
                           size_t const pos,
                           struct aviutl_text_tag *const tag) {
  if (len - pos < 3) {
    return false;
  }
  if (str[pos] == '&' && str[pos + 1] == '#') {
    return parse_num_ref(str, len, pos, tag);
  }
  if (str[pos] != '<') {
    return false;
  }
  int type = aviutl_text_tag_type_unknown;
  switch (str[pos + 1]) {
  case '#':
    type = aviutl_text_tag_type_color;
    break;
  case 'p':
    type = aviutl_text_tag_type_position;
    break;
  case 's':
    type = aviutl_text_tag_type_font;
    break;
  case 'r':
    type = aviutl_text_tag_type_speed;
    break;
  case 'w':
    type = aviutl_text_tag_type_wait;
    break;
  case 'c':
    type = aviutl_text_tag_type_clear;
    break;
  case '?':
    return parse_script_tag(str, len, pos, tag);
  default:
    return false;
  }
  size_t end = pos + 2, token = 0;
  bool found_dot = false;
  size_t value_pos[3] = {pos + 2, SIZE_MAX, SIZE_MAX};
  size_t value_len[3] = {0, 0, 0};
  for (; end < len; ++end) {
    if (str[end] == ',') {
      value_len[token] = end - value_pos[token];
      ++token;
      if (token == 3) {
        return false; // too many tokens
      }
      found_dot = false;
      value_pos[token] = end + 1;
      continue;
    }
    // '<' is not allowed in most cases, except for font name.
    if (str[end] == '<' && (type != aviutl_text_tag_type_font || (type == aviutl_text_tag_type_font && token != 1))) {
      return false;
    }
    if (str[end] == '>') {
      value_len[token] = end - value_pos[token];
      if (token == 0 && value_len[0] == 0) {
        value_pos[0] = SIZE_MAX;
      }
      ++token;
      // final check
      switch (type) {
      case aviutl_text_tag_type_color:
        if (token > 2 || (value_len[0] != 0 && value_len[0] != 6) || (value_len[1] != 0 && value_len[1] != 6) ||
            (value_len[0] == 0 && value_len[1] == 6)) {
          return false;
        }
        break;
      case aviutl_text_tag_type_position:
        if (token == 1) {
          return false;
        }
        break;
      }
      tag->type = (enum aviutl_text_tag_type)type;
      tag->pos = pos;
      tag->len = end - pos + 1;
      tag->value_pos[0] = value_pos[0];
      tag->value_pos[1] = value_pos[1];
      tag->value_pos[2] = value_pos[2];
      tag->value_len[0] = value_len[0];
      tag->value_len[1] = value_len[1];
      tag->value_len[2] = value_len[2];
      return true;
    }
    if (type == aviutl_text_tag_type_color) {
      switch (token) {
      case 0:
      case 1:
        if (!is_xdigit(str[end])) {
          return false;
        }
        break;
      default:
        return false;
      }
    } else if (type == aviutl_text_tag_type_position) {
      if (is_sign(str[end])) {
        if (value_pos[token] != end) {
          return false;
        }
      } else if (!is_decimal(str[end], &found_dot)) {
        return false;
      }
    } else if (type == aviutl_text_tag_type_font) {
      switch (token) {
      case 0: // size
        if (!is_digit(str[end])) {
          return false;
        }
        break;
      case 1: // name
        break;
      case 2: // style
        if (str[end] != 'I' && str[end] != 'B') {
          return false;
        }
        break;
      default:
        return false;
      }
    } else if (type == aviutl_text_tag_type_speed) {
      switch (token) {
      case 0:
        if (!is_decimal(str[end], &found_dot)) {
          return false;
        }
        break;
      default:
        return false;
      }
    } else if (type == aviutl_text_tag_type_wait || type == aviutl_text_tag_type_clear) {
      switch (token) {
      case 0:
        if (str[end] == '*') {
          if (value_pos[token] != end) {
            return false;
          }
        } else if (!is_decimal(str[end], &found_dot)) {
          return false;
        }
        break;
      default:
        return false;
      }
    }
  }
  return false;
}

void aviutl_text_get_numcharref(aviutl_text_char const *const str,
                                struct aviutl_text_tag const *const tag,
                                struct aviutl_text_tag_numcharref *const value) {
  uint64_t u64 = 0;
  if (ov_atou_wchar(str + tag->value_pos[0], &u64, false)) {
    value->ch = (uint16_t)u64;
  } else {
    value->ch = 0xfffd;
  }
}

static inline uint32_t hex2dec(aviutl_text_char const c) {
  if ('0' <= c && c <= '9') {
    return c - '0';
  }
  return (c | 0x20) - 'a' + 10;
}

static uint32_t parse_color(aviutl_text_char const *const str) {
  uint32_t color = 0;
  for (size_t i = 0; i < 6; ++i) {
    color = (color << 4) | hex2dec(str[i]);
  }
  return 0xff000000 | color;
}

void aviutl_text_get_color(aviutl_text_char const *const str,
                           struct aviutl_text_tag const *const tag,
                           struct aviutl_text_tag_color *const value) {
  value->color[0] = tag->value_len[0] == 0 ? 0 : parse_color(str + tag->value_pos[0]);
  value->color[1] = tag->value_len[1] == 0 ? 0 : parse_color(str + tag->value_pos[1]);
}

void aviutl_text_get_position(aviutl_text_char const *const str,
                              struct aviutl_text_tag const *const tag,
                              struct aviutl_text_tag_position *const value) {
  if (tag->value_len[0] != 0) {
    double d = 0;
    ov_atof_wchar(str + tag->value_pos[0], &d, false);
    value->x = d;
  } else {
    value->x = 0;
  }
  value->x_type = tag->value_pos[0] == SIZE_MAX                              ? aviutl_text_tag_position_type_unknown
                  : tag->value_len[0] > 0 && is_sign(str[tag->value_pos[0]]) ? aviutl_text_tag_position_type_relative
                                                                             : aviutl_text_tag_position_type_absolute;

  if (tag->value_len[1] != 0) {
    double d = 0;
    ov_atof_wchar(str + tag->value_pos[1], &d, false);
    value->y = d;
  } else {
    value->y = 0;
  }
  value->y_type = tag->value_pos[1] == SIZE_MAX                              ? aviutl_text_tag_position_type_unknown
                  : tag->value_len[1] > 0 && is_sign(str[tag->value_pos[1]]) ? aviutl_text_tag_position_type_relative
                                                                             : aviutl_text_tag_position_type_absolute;

  if (tag->value_len[2] != 0) {
    double d = 0;
    ov_atof_wchar(str + tag->value_pos[2], &d, false);
    value->z = d;
  } else {
    value->z = 0;
  }
  value->z_type = tag->value_pos[2] == SIZE_MAX                              ? aviutl_text_tag_position_type_unknown
                  : tag->value_len[2] > 0 && is_sign(str[tag->value_pos[2]]) ? aviutl_text_tag_position_type_relative
                                                                             : aviutl_text_tag_position_type_absolute;
}

static size_t find_char_reverse(aviutl_text_char const *const str, size_t const pos, aviutl_text_char const ch) {
  for (size_t i = pos; i <= pos; --i) {
    if (str[i] == ch) {
      return i;
    }
  }
  return SIZE_MAX;
}

void aviutl_text_get_font(aviutl_text_char const *const str,
                          struct aviutl_text_tag const *const tag,
                          struct aviutl_text_tag_font *const value) {
  uint64_t u64 = 0;
  if (tag->value_len[0] != 0) {
    ov_atou_wchar(str + tag->value_pos[0], &u64, false);
    value->size = (size_t)u64;
  } else {
    value->size = 0;
  }

  if (tag->value_len[1] > 0) {
    value->name = str + tag->value_pos[1];
    value->name_len = tag->value_len[1];
  } else {
    value->name = NULL;
    value->name_len = 0;
  }

  value->bold =
      tag->value_len[2] > 0 && (find_char_reverse(str + tag->value_pos[2], tag->value_len[2] - 1, 'B') != SIZE_MAX);
  value->italic =
      tag->value_len[2] > 0 && (find_char_reverse(str + tag->value_pos[2], tag->value_len[2] - 1, 'I') != SIZE_MAX);
}

void aviutl_text_get_speed(aviutl_text_char const *const str,
                           struct aviutl_text_tag const *const tag,
                           struct aviutl_text_tag_speed *const value) {
  if (tag->value_len[0] != 0) {
    double d = 0;
    ov_atof_wchar(str + tag->value_pos[0], &d, false);
    value->v = d;
  } else {
    value->v = 0;
  }
}

void aviutl_text_get_wait(aviutl_text_char const *const str,
                          struct aviutl_text_tag const *const tag,
                          struct aviutl_text_tag_wait *const value) {
  value->per_char = tag->value_len[0] > 0 && str[tag->value_pos[0]] == '*';
  if (tag->value_len[0] != 0) {
    double d = 0;
    ov_atof_wchar(str + tag->value_pos[0] + (value->per_char ? 1 : 0), &d, false);
    value->v = d;
  } else {
    value->v = 0;
  }
}

void aviutl_text_get_clear(aviutl_text_char const *const str,
                           struct aviutl_text_tag const *const tag,
                           struct aviutl_text_tag_clear *const value) {
  value->per_char = tag->value_len[0] > 0 && str[tag->value_pos[0]] == '*';
  if (tag->value_len[0] != 0) {
    double d = 0;
    ov_atof_wchar(str + tag->value_pos[0] + (value->per_char ? 1 : 0), &d, false);
    value->v = d;
  } else {
    value->v = 0;
  }
}

void aviutl_text_get_script(aviutl_text_char const *const str,
                            struct aviutl_text_tag const *const tag,
                            struct aviutl_text_tag_script *const value) {
  value->ptr = str + tag->value_pos[0];
  value->len = tag->value_len[0];
}

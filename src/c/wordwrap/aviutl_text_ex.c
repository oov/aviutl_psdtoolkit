#include "aviutl_text_ex.h"

#include <ovnum.h>

static inline bool is_sign(aviutl_text_ex_char const c) { return c == '+' || c == '-'; }

static inline bool is_digit(aviutl_text_ex_char const c) { return '0' <= c && c <= '9'; }

static inline bool is_decimal(aviutl_text_ex_char const c, bool *const found_dot) {
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

bool aviutl_text_ex_parse_tag(aviutl_text_ex_char const *const str,
                              size_t const len,
                              size_t const pos,
                              struct aviutl_text_ex_tag *const tag) {
  if (len - pos < 4) {
    return false;
  }
  if (str[pos] != '<') {
    return false;
  }
  int type = aviutl_text_ex_tag_type_unknown;
  size_t end;
  if (str[pos + 1] == 'p' && str[pos + 2] == 'p') {
    type = aviutl_text_ex_tag_type_position;
    end = pos + 3;
  } else if (str[pos + 1] == 's' && str[pos + 2] == 's') {
    type = aviutl_text_ex_tag_type_font;
    end = pos + 3;
  } else if (len - pos >= 6 && str[pos + 1] == 'k' && str[pos + 2] == 'e' && str[pos + 3] == 'r' &&
             str[pos + 4] == 'n') {
    type = aviutl_text_ex_tag_type_kerning;
    end = pos + 5;
  } else if (len - pos >= 5 && str[pos + 1] == 'w' && str[pos + 2] == 'b' && str[pos + 3] == 'r' &&
             str[pos + 4] == '>') {
    type = aviutl_text_ex_tag_type_wbr;
    end = pos + 5;
  } else if (len - pos >= 6 && str[pos + 1] == 'n' && str[pos + 2] == 'o' && str[pos + 3] == 'b' &&
             str[pos + 4] == 'r' && str[pos + 5] == '>') {
    type = aviutl_text_ex_tag_type_nobr;
    end = pos + 6;
  } else if (len - pos >= 7 && str[pos + 1] == '/' && str[pos + 2] == 'n' && str[pos + 3] == 'o' &&
             str[pos + 4] == 'b' && str[pos + 5] == 'r' && str[pos + 6] == '>') {
    type = aviutl_text_ex_tag_type_nobr_close;
    end = pos + 7;
  } else if (len - pos >= 7 && str[pos + 1] == '/' && str[pos + 2] == 'k' && str[pos + 3] == 'e' &&
             str[pos + 4] == 'r' && str[pos + 5] == 'n' && str[pos + 6] == '>') {
    type = aviutl_text_ex_tag_type_kerning_close;
    end = pos + 7;
  } else {
    return false;
  }

  if (type == aviutl_text_ex_tag_type_wbr || type == aviutl_text_ex_tag_type_nobr ||
      type == aviutl_text_ex_tag_type_nobr_close || type == aviutl_text_ex_tag_type_kerning_close) {
    *tag = (struct aviutl_text_ex_tag){
        .type = (enum aviutl_text_ex_tag_type)type,
        .pos = pos,
        .len = end - pos,
        .value_pos = {SIZE_MAX, SIZE_MAX, SIZE_MAX},
        .value_len = {0, 0, 0},
    };
    return true;
  }

  size_t token = 0;
  bool found_dot = false;
  size_t value_pos[3] = {end, SIZE_MAX, SIZE_MAX};
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
    if (str[end] == '<' &&
        (type != aviutl_text_ex_tag_type_font || (type == aviutl_text_ex_tag_type_font && token != 1))) {
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
      case aviutl_text_ex_tag_type_position:
        if (token == 1) {
          return false;
        }
        break;
      }
      tag->type = (enum aviutl_text_ex_tag_type)type;
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
    if (type == aviutl_text_ex_tag_type_position) {
      if (is_sign(str[end])) {
        if (value_pos[token] != end) {
          return false;
        }
      } else if (!is_decimal(str[end], &found_dot)) {
        return false;
      }
    } else if (type == aviutl_text_ex_tag_type_font) {
      switch (token) {
      case 0: // size
        if (!is_decimal(str[end], &found_dot)) {
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
    } else if (type == aviutl_text_ex_tag_type_kerning) {
      switch (token) {
      case 0: // distance
        if (!is_decimal(str[end], &found_dot)) {
          return false;
        }
        break;
      case 1: // margin
        if (!is_decimal(str[end], &found_dot)) {
          return false;
        }
        break;
      case 2: // type
        if (str[end] != '0' && str[end] != '1') {
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

void aviutl_text_ex_get_position(aviutl_text_ex_char const *const str,
                                 struct aviutl_text_ex_tag const *const tag,
                                 struct aviutl_text_ex_tag_position *const value) {
  double d = 0;

  if (tag->value_len[0] != 0) {
    ov_atof_wchar(str + tag->value_pos[0], &d, false);
    value->x = d;
  } else {
    value->x = 0;
  }
  value->x_type = tag->value_pos[0] == SIZE_MAX ? aviutl_text_ex_tag_position_type_unknown
                  : tag->value_len[0] > 0 && is_sign(str[tag->value_pos[0]])
                      ? aviutl_text_ex_tag_position_type_relative
                      : aviutl_text_ex_tag_position_type_absolute;

  if (tag->value_len[1] != 0) {
    ov_atof_wchar(str + tag->value_pos[1], &d, false);
    value->y = d;
  } else {
    value->y = 0;
  }
  value->y_type = tag->value_pos[1] == SIZE_MAX ? aviutl_text_ex_tag_position_type_unknown
                  : tag->value_len[1] > 0 && is_sign(str[tag->value_pos[1]])
                      ? aviutl_text_ex_tag_position_type_relative
                      : aviutl_text_ex_tag_position_type_absolute;

  if (tag->value_len[2] != 0) {
    ov_atof_wchar(str + tag->value_pos[2], &d, false);
    value->z = d;
  } else {
    value->z = 0;
  }
  value->z_type = tag->value_pos[2] == SIZE_MAX ? aviutl_text_ex_tag_position_type_unknown
                  : tag->value_len[2] > 0 && is_sign(str[tag->value_pos[2]])
                      ? aviutl_text_ex_tag_position_type_relative
                      : aviutl_text_ex_tag_position_type_absolute;
}

static size_t find_char_reverse(aviutl_text_ex_char const *const str, size_t const pos, aviutl_text_ex_char const ch) {
  for (size_t i = pos; i <= pos; --i) {
    if (str[i] == ch) {
      return i;
    }
  }
  return SIZE_MAX;
}

void aviutl_text_ex_get_font(aviutl_text_ex_char const *const str,
                             struct aviutl_text_ex_tag const *const tag,
                             struct aviutl_text_ex_tag_font *const value) {
  double d = 0;
  if (tag->value_len[0] != 0) {
    ov_atof_wchar(str + tag->value_pos[0], &d, false);
    value->size = d;
  } else {
    value->size = 1;
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

void aviutl_text_ex_get_kerning(aviutl_text_ex_char const *const str,
                                struct aviutl_text_ex_tag const *const tag,
                                struct aviutl_text_ex_tag_kerning *const value) {
  double d = 0;
  if (tag->value_len[0] != 0) {
    ov_atof_wchar(str + tag->value_pos[0], &d, false);
    value->distance = d;
  } else {
    value->distance = 100;
  }

  if (tag->value_len[1] != 0) {
    ov_atof_wchar(str + tag->value_pos[1], &d, false);
    value->margin = d;
  } else {
    value->margin = .0;
  }

  if (tag->value_len[2] != 0) {
    switch (str[tag->value_pos[2]]) {
    case '0':
      value->method = aviutl_text_ex_tag_kerning_method_convexhull;
      break;
    case '1':
      value->method = aviutl_text_ex_tag_kerning_method_box;
      break;
    default:
      value->method = aviutl_text_ex_tag_kerning_method_unknown;
      break;
    }
  } else {
    value->method = aviutl_text_ex_tag_kerning_method_convexhull;
  }
}

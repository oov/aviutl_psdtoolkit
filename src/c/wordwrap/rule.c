#include "rule.h"

#include <stdbool.h>

#ifdef WW_DEBUG
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#endif

/**
 * @brief Checks if the given character is not a first line character, such as "）".
 *
 * This function checks if the given character is one of the characters that should not be the first character
 * in a line of text. These characters include various types of brackets and quotation marks.
 *
 * @param ch The character to check.
 * @return Returns true if the character is not a first line character, false otherwise.
 */
static bool is_no_first_line_char(wchar_t const ch) {
  switch (ch) {
  case L',':
  case L')':
  case L'）':
  case L']':
  case L'］':
  case L'}':
  case L'｝':
  case L'､':
  case L'、':
  case L'〕':
  case L'〉':
  case L'》':
  case L'｣':
  case L'」':
  case L'』':
  case L'】':
  case L'〙':
  case L'〗':
  case L'〟':
  case L'’':
  case L'”':
  case L'｠':
  case L'»':

  case L'‐':
  case L'゠':
  case L'–':
  case L'〜':
  case L'～':

  case L'?':
  case L'？':
  case L'!':
  case L'！':
  case L'‼':
  case L'⁇':
  case L'⁈':
  case L'⁉':

  case L'·':
  case L'・':
  case L':':
  case L'：':
  case L';':
  case L'；':
  case L'/':
  case L'／':

  case L'｡':
  case L'。':
  case L'.':
  case L'．':

  case L'゛':
  case L'ﾞ':
  case L'゜':
  case L'ﾟ':
  case L'ゝ':
  case L'ゞ':
  case L'々':
  case L'〻':
  case L'ー':
  case L'ァ':
  case L'ィ':
  case L'ゥ':
  case L'ェ':
  case L'ォ':
  case L'ッ':
  case L'ャ':
  case L'ュ':
  case L'ョ':
  case L'ヮ':
  case L'ヵ':
  case L'ヶ':
  case L'ぁ':
  case L'ぃ':
  case L'ぅ':
  case L'ぇ':
  case L'ぉ':
  case L'っ':
  case L'ゃ':
  case L'ゅ':
  case L'ょ':
  case L'ゎ':
  case L'ゕ':
  case L'ゖ':
  case L'ㇰ':
  case L'ㇱ':
  case L'ㇲ':
  case L'ㇳ':
  case L'ㇴ':
  case L'ㇵ':
  case L'ㇶ':
  case L'ㇷ':
  case L'ㇸ':
  case L'ㇹ':
  // case L'ㇷ゚': // U+31F7 + U+309A
  case L'ㇺ':
  case L'ㇻ':
  case L'ㇼ':
  case L'ㇽ':
  case L'ㇾ':
  case L'ㇿ':
  case L'ｧ':
  case L'ｨ':
  case L'ｩ':
  case L'ｪ':
  case L'ｫ':
  case L'ｯ':
  case L'ｬ':
  case L'ｭ':
  case L'ｮ':
    return true;
  }
  return false;
}

/**
 * @brief Checks if the given character is not a last line character, such as "（".
 *
 * This function checks if the given character is one of the characters that should not be the last character
 * in a line of text. These characters include various types of brackets and quotation marks.
 *
 * @param ch The character to check.
 * @return Returns true if the character is not a last line character, false otherwise.
 */
static bool is_no_last_line_char(wchar_t const ch) {
  switch (ch) {
  case L'(':
  case L'（':
  case L'[':
  case L'［':
  case L'{':
  case L'｛':
  case L'〔':
  case L'〈':
  case L'《':
  case L'｢':
  case L'「':
  case L'『':
  case L'【':
  case L'〘':
  case L'〖':
  case L'〝':
  case L'‘':
  case L'“':
  case L'｟':
  case L'«':
    return true;
  }
  return false;
}

/**
 * @brief Check if the given wide character is a non-breaking character, such as "…".
 *
 * @param ch The character to check.
 * @return true if the character is a non-breaking character, false otherwise.
 */
static bool is_no_break_char(wchar_t const ch) {
  switch (ch) {
  case L'—':
  case L'…':
  case L'‥':
  case L'〳':
  case L'〴':
  case L'〵':
    return true;
  }
  return false;
}

/**
 * @brief Tests if a given character is a low surrogate in Unicode.
 *
 * @param ch The character to test.
 * @return Returns true if the character is a low surrogate, false otherwise.
 */
static bool is_surrogate_low(wchar_t const ch) { return 0xdc00 <= ch && ch < 0xe000; }

/**
 * @brief Checks if the given characters represent a signed integer.
 *
 * @param ch1 The first character, expected to be '+' or '-' for signed integers.
 * @param ch2 The second character, expected to be a digit (0-9).
 * @return Returns true if the characters represent a signed integer (e.g., '+1', '-2'), false otherwise.
 */
static bool is_signed_number(wchar_t const ch1, wchar_t const ch2) {
  return (ch1 == L'-' || ch1 == L'+') && (L'0' <= ch2 && ch2 <= L'9');
}

static bool is_graph(wchar_t const ch) { return L'!' <= ch && ch <= L'~'; }

static bool is_ascii_word(wchar_t const ch1, wchar_t const ch2) { return is_graph(ch1) && is_graph(ch2); }

static bool is_non_ascii_word(wchar_t const ch1, wchar_t const ch2) { return ch1 >= 128 && ch2 >= 128; }

enum break_rule rule_is_breakable(wchar_t const ch1, wchar_t const ch2, int enabled_rules) {
  enum break_rule br = br_breakable;
  if ((enabled_rules & br_no_first_line_char) != 0 && is_no_first_line_char(ch2)) {
    br = br_no_first_line_char;
  } else if ((enabled_rules & br_no_last_line_char) != 0 && is_no_last_line_char(ch1)) {
    br = br_no_last_line_char;
  } else if ((enabled_rules & br_no_break_char) != 0 && is_no_break_char(ch2)) {
    br = br_no_break_char;
  } else if ((enabled_rules & br_surrogate_low) != 0 && is_surrogate_low(ch1)) {
    br = br_surrogate_low;
  } else if ((enabled_rules & br_signed_number) != 0 && is_signed_number(ch1, ch2)) {
    br = br_signed_number;
  } else if ((enabled_rules & br_ascii_word) != 0 && is_ascii_word(ch1, ch2)) {
    br = br_ascii_word;
  } else if ((enabled_rules & br_non_ascii_word) != 0 && is_non_ascii_word(ch1, ch2)) {
    br = br_non_ascii_word;
  }
#ifdef WW_DEBUG
  wchar_t buf[128];
  wchar_t const *t = NULL;
  switch (br) {
  case br_breakable:
    t = L"br_breakable";
    break;
  case br_no_first_line_char:
    t = L"no_first_line_char";
    break;
  case br_no_last_line_char:
    t = L"no_last_line_char";
    break;
  case br_no_break_char:
    t = L"no_break_char";
    break;
  case br_surrogate_low:
    t = L"surrogate_low";
    break;
  case br_signed_number:
    t = L"signed_number";
    break;
  case br_ascii_word:
    t = L"ascii_word";
    break;
  case br_non_ascii_word:
    t = L"non_ascii_word";
    break;
  case br_budoux:
    t = L"budoux";
    break;
  }
  wsprintfW(buf, L"ch1: %lc ch2: %lc %ls", ch1, ch2, t);
  OutputDebugStringW(buf);
#endif
  return br;
}

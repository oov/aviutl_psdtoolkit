#pragma once

#include <wchar.h>

enum break_rule {
  br_breakable = 0,
  br_no_first_line_char = 1,
  br_no_last_line_char = 2,
  br_no_break_char = 4,
  br_surrogate_low = 8,
  br_signed_number = 16,
  br_ascii_word = 32,
  br_non_ascii_word = 64,
  br_budoux = 128,
};

enum break_rule rule_is_breakable(wchar_t const ch1, wchar_t const ch2, int enabled_rules);

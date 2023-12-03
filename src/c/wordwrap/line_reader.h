#pragma once

#include <ovbase.h>

#include "glyph.h"

struct line_reader {
  struct glyph const *glyphs;
  size_t linehead;
};

/**
 * @brief Searches for a glyph from a specified position in the left direction within the glyphs of a line_reader.
 *
 * @param lr The line_reader structure to search within.
 * @param pos The position to start the search from.
 * @return size_t The position of the first glyph found. Returns SIZE_MAX if no glyph is found.
 */
size_t line_reader_find_left(struct line_reader const *const lr, size_t const pos);

/**
 * @brief Searches for a glyph from a specified position in the right direction within the glyphs of a line_reader.
 *
 * @param lr The line_reader structure to search within.
 * @param pos The position to start the search from.
 * @return size_t The position of the first glyph found. Returns SIZE_MAX if no glyph is found.
 */
size_t line_reader_find_right(struct line_reader const *const lr, size_t const pos);

/**
 * @brief Finds the nearest breakable glyph from a given position.
 *
 * This function searches for the nearest glyph that can be broken from a given position.
 * It first searches to the left of the position, and if no breakable glyph is found, it then searches to the right.
 * A glyph is considered breakable based on the break_rule parameter and the glyph's own properties.
 *
 * @param lr Pointer to the line_reader structure.
 * @param pos The position from which to start the search.
 * @param break_rule The rule to determine if a glyph is breakable.
 * @return size_t The position of the nearest breakable glyph. If no breakable glyph is found, returns SIZE_MAX.
 */
size_t line_reader_find_breakable(struct line_reader const *const lr, size_t const pos, int const break_rule);

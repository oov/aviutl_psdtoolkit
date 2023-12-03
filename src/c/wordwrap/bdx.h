#pragma once

#include <budoux-c.h>
#include <ovbase.h>

#include "line_reader.h"

/**
 * @brief Writes markers to text boundaries.
 *
 * @param glyphs Pointer to the glyphs.
 * @param linehead The head of the line.
 * @param model Pointer to the BudouX model.
 * @return Error code or eok() if successful.
 *
 * Parses the text using the BudouX model to find word boundaries and writes markers to glyphs.
 * Returns an error if memory allocation fails.
 */
NODISCARD error bdx_write_markers(struct glyph *const glyphs,
                                  size_t const linehead,
                                  struct budouxc *const model,
                                  size_t **const boundaries);

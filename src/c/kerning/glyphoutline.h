#pragma once

#include <stdbool.h>
#include <stddef.h>

#include "point.h"

/**
 * @brief Analyze the data obtained from GetGlyphOutline with GGO_NATIVE.
 *        Approximate curves with straight lines.
 *
 * @param p Data obtained from GetGlyphOutline with GGO_NATIVE.
 * @param sz Size of p.
 * @param tolerance Tolerance for approximating curves.
 * @param fn Callback function for each element of the obtained shape.
 * @param userdata User data passed to opfn.
 * @return true Successfully analyzed.
 * @return false Error occurred during analysis or opfn returned false.
 */
bool glyphoutline_parse(
    void const *const p, size_t const sz, double const tolerance, point_op_fn const fn, void *const userdata);

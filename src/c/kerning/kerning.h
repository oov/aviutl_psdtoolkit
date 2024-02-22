#pragma once

#include <ovbase.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "distance.h"
#include "point.h"

struct kerning_context;

NODISCARD error kerning_context_create(struct kerning_context **const ctx);
void kerning_context_destroy(struct kerning_context **const ctx);

struct kerning_style {
  double distance;
  double margin;
  enum distance_find_nearest_method method;

  double margin_unit;
};

NODISCARD error kerning_calculate_distance(struct kerning_context *const ctx,
                                           struct kerning_style const *const ks,
                                           HDC const hdc,
                                           wchar_t const ch,
                                           struct point *const distance);
void kerning_reset(struct kerning_context *const ctx);

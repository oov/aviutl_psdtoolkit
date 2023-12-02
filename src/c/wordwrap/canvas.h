#pragma once

#include <ovbase.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

struct canvas {
  LOGFONTW *initial_font;

  HWND window;
  HDC dc;
  TEXTMETRICW current_font_text_metric;
  HFONT old_font;
};

NODISCARD error canvas_create(struct canvas **const ctxpp);
void canvas_destroy(struct canvas **const ctxpp);
NODISCARD error canvas_set_font(struct canvas *const ctx, LOGFONTW const *lf, bool const high_resolution);
NODISCARD error canvas_set_initial_font(struct canvas *const ctx, LOGFONTW *const lf, bool const high_resolution);
NODISCARD error canvas_set_font_params(struct canvas *const ctx,
                                       wchar_t const *const name,
                                       size_t const size,
                                       bool const bold,
                                       bool const italic,
                                       bool const high_resolution);
bool canvas_get_metrics(struct canvas *const ctx,
                        wchar_t const ch,
                        GLYPHMETRICS *const gm,
                        bool const monospace,
                        bool const high_resolution);

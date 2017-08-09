package nkhelper

/*
// This is very poor approach...
#cgo CFLAGS: -I ../../../../../github.com/golang-ui/nuklear/nk/
#define NK_INCLUDE_FIXED_TYPES 1
#define NK_INCLUDE_STANDARD_IO 1
#define NK_INCLUDE_DEFAULT_ALLOCATOR 1
#define NK_INCLUDE_FONT_BAKING 1
#define NK_INCLUDE_DEFAULT_FONT 1
#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT 1
#include "nuklear.h"
#include <stdio.h>

const nk_rune nk_font_japanese_glyph_ranges[] = {
    0x0020, 0x00FF,
    0x2200, 0x22FF, // Mathematical Operators
    0x3000, 0x303F, // CJK Symbols and Punctuation
    0x3040, 0x309F, // Hiragana
    0x30A0, 0x30FF, // Katakana
    0x0370, 0x03FF, // Greek and Coptic
    0xFF00, 0xFFEF, // Halfwidth and Fullwidth Forms
    0x4E00, 0x9FFF, // CJK Unified Ideographs
    0
};
void set_japanese_glyph_ranges(void *p) {
	struct nk_font_config *fc = p;
	fc->range = &nk_font_japanese_glyph_ranges[0];
}

void *style_text_color(void *p) {
	struct nk_context *ctx = p;
	return &ctx->style.text.color;
}

void *style_button_normal(void *p) {
	struct nk_context *ctx = p;
	return &ctx->style.button.normal;
}

void *style_button_text_normal_color(void *p) {
	struct nk_context *ctx = p;
	return &ctx->style.button.text_normal;
}

void *style_button_border(void *p) {
	struct nk_context *ctx = p;
	return &ctx->style.button.border;
}

void *style_button_padding(void *p) {
	struct nk_context *ctx = p;
	return &ctx->style.button.padding;
}

struct nk_vec2 window_scrollbar_offset(void *p) {
	struct nk_window *w = p;
	struct nk_vec2 r = {w->scrollbar.x, w->scrollbar.y};
	return r;
}

struct nk_rect window_bounds(void *p) {
	struct nk_window *w = p;
	return w->bounds;
}

void set_window_scrollbar_offset(void *p, void *o) {
	struct nk_window *w = p;
	struct nk_vec2 *offset = o;
	w->scrollbar.x = offset->x;
	w->scrollbar.y = offset->y;
}

struct nk_vec2 panel_offset(void *p) {
	struct nk_panel *pn = p;
	struct nk_vec2 r = {*pn->offset_x, *pn->offset_y};
	return r;
}

void set_panel_offset(void *p, void *o) {
	struct nk_panel *pn = p;
	struct nk_vec2 *offset = o;
	*pn->offset_x = offset->x;
	*pn->offset_y = offset->y;
}

*/
import "C"

import (
	"unsafe"

	"github.com/golang-ui/nuklear/nk"
)

func SetJapaneseGlyphRanges(fc *nk.FontConfig) {
	C.set_japanese_glyph_ranges(unsafe.Pointer(fc.Ref()))
}

func GetStyleTextColorPtr(ctx *nk.Context) *nk.Color {
	return (*nk.Color)(C.style_text_color(unsafe.Pointer(ctx.Ref())))
}

func GetStyleButtonNormalPtr(ctx *nk.Context) *nk.StyleItem {
	return (*nk.StyleItem)(C.style_button_normal(unsafe.Pointer(ctx.Ref())))
}

func GetStyleButtonTextNormalColorPtr(ctx *nk.Context) *nk.Color {
	return (*nk.Color)(C.style_button_text_normal_color(unsafe.Pointer(ctx.Ref())))
}

func GetStyleButtonBorderPtr(ctx *nk.Context) *float32 {
	return (*float32)(C.style_button_border(unsafe.Pointer(ctx.Ref())))
}

func GetStyleButtonPaddingPtr(ctx *nk.Context) *nk.Vec2 {
	return (*nk.Vec2)(C.style_button_padding(unsafe.Pointer(ctx.Ref())))
}

func GetWindowScrollBarOffset(w *nk.Window) nk.Vec2 {
	r := C.window_scrollbar_offset(unsafe.Pointer(w.Ref()))
	return *(*nk.Vec2)(unsafe.Pointer(&r))
}

func GetWindowBounds(w *nk.Window) nk.Rect {
	r := C.window_bounds(unsafe.Pointer(w.Ref()))
	return *(*nk.Rect)(unsafe.Pointer(&r))
}

func SetWindowScrollBarOffset(w *nk.Window, offset nk.Vec2) {
	C.set_window_scrollbar_offset(unsafe.Pointer(w.Ref()), unsafe.Pointer(offset.Ref()))
}

func GetPanelOffset(p *nk.Panel) nk.Vec2 {
	r := C.panel_offset(unsafe.Pointer(p.Ref()))
	return *(*nk.Vec2)(unsafe.Pointer(&r))
}

func SetPanelOffset(p *nk.Panel, offset nk.Vec2) {
	C.set_panel_offset(unsafe.Pointer(p.Ref()), unsafe.Pointer(offset.Ref()))
}

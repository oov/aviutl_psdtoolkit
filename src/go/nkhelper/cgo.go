//go:generate go run genrange.go

package nkhelper

/*
// This is very poor approach...
#cgo CFLAGS: -I ../../../../../../github.com/golang-ui/nuklear/nk/
#define NK_INCLUDE_FIXED_TYPES 1
#define NK_INCLUDE_STANDARD_IO 1
#define NK_INCLUDE_DEFAULT_ALLOCATOR 1
#define NK_INCLUDE_FONT_BAKING 1
#define NK_INCLUDE_DEFAULT_FONT 1
#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT 1
#include "nuklear.h"
#include <stdio.h>

#include "jprange.h"
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

void *style_window_padding(void *p) {
	struct nk_context *ctx = p;
	return &ctx->style.window.padding;
}

void *style_window_group_padding(void *p) {
	struct nk_context *ctx = p;
	return &ctx->style.window.group_padding;
}

float font_height(void *p) {
	struct nk_user_font *f = p;
	return f->height;
}

float text_width(void *p, void *s, int len) {
	struct nk_user_font *f = p;
	return f->width(f->userdata, f->height, s, len);
}

int is_pressed(void *p, int key) {
	struct nk_input *input = p;
	char *text = input->keyboard.text;
	int len = input->keyboard.text_len;
	for (int i = 0; i < len; ++i, ++text) {
		if (*text == key) {
			input->keyboard.text_len = 0;
			return 1;
		}
	}
	return 0;
}

int reset_scroll_delta(void *p) {
	struct nk_input *input = p;
	struct nk_vec2 *d = &input->mouse.scroll_delta;
	d->x = 0;
	d->y = 0;
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

func GetStyleWindowPaddingPtr(ctx *nk.Context) *nk.Vec2 {
	return (*nk.Vec2)(C.style_window_padding(unsafe.Pointer(ctx.Ref())))
}

func GetStyleWindowGroupPaddingPtr(ctx *nk.Context) *nk.Vec2 {
	return (*nk.Vec2)(C.style_window_group_padding(unsafe.Pointer(ctx.Ref())))
}

func FontHeight(p *nk.UserFont) float32 {
	return float32(C.font_height(unsafe.Pointer(p.Ref())))
}

func TextWidth(p *nk.UserFont, s string) float32 {
	if s == "" {
		return 0
	}
	return float32(C.text_width(unsafe.Pointer(p.Ref()), unsafe.Pointer(&[]byte(s)[0]), C.int(len(s))))
}

func IsPressed(ctx *nk.Context, key int) bool {
	return C.is_pressed(unsafe.Pointer(ctx.Input().Ref()), C.int(key)) != 0
}

func ResetScrollDelta(ctx *nk.Context) {
	C.reset_scroll_delta(unsafe.Pointer(ctx.Input().Ref()))
}

// +build gdip

package main

import (
	"github.com/golang-ui/nuklear/nk"
	"github.com/golang-ui/nuklear/nk/w32"
	"github.com/oov/aviutl_psdtoolkit/src/go/assets"
)

type font struct {
	f *nk.GdipFont
}

func (g *gui) initFont() error {
	sans, err := nk.NkCreateFontFromBytes(assets.MustAsset("Ohruri-Regular.ttf"), 20)
	if err != nil {
		return err
	}
	symbol, err := nk.NkCreateFontFromBytes(assets.MustAsset("symbols.ttf"), 14)
	if err != nil {
		sans.Close()
		return err
	}
	g.Font.Sans = &font{sans}
	g.Font.Symbol = &font{symbol}

	g.Font.SansHandle = g.Font.Sans.f.Handle()
	g.Font.SymbolHandle = g.Font.Symbol.f.Handle()
	nk.NkStyleSetFont(g.Context, g.Font.SansHandle)
	g.LayerView.MainFontHandle = g.Font.SansHandle
	g.LayerView.SymbolFontHandle = g.Font.SymbolHandle
	return nil
}

func (g *gui) freeFont() {
	g.Font.SymbolHandle.Free()
	g.Font.Symbol.f.Close()
	g.Font.SansHandle.Free()
	g.Font.Sans.f.Close()
}

func (g *gui) pollEvents() {
	w32.PollEvents()
}

func (g *gui) terminate() {
	w32.Terminate()
}

type window struct {
	w *w32.Window
}

func newWindow(width, height int, title string) (*window, *nk.Context, error) {
	if err := w32.Init(); err != nil {
		return nil, nil, err
	}
	win, err := w32.CreateWindow(width, height, title, nil, nil)
	if err != nil {
		return nil, nil, err
	}
	ctx := nk.NkPlatformInit(win, nk.PlatformInstallCallbacks)
	return &window{w: win}, ctx, nil
}

func (w *window) Show()                 { w.w.Show() }
func (w *window) Hide()                 { w.w.Hide() }
func (w *window) GetSize() (int, int)   { return w.w.GetSize() }
func (w *window) SetShouldClose(v bool) { w.w.SetShouldClose(v) }
func (w *window) ShouldClose() bool     { return w.w.ShouldClose() }
func (w *window) NativeWindow() uintptr { return uintptr(w.w.Handle) }
func (w *window) SetDropCallback(fn func(w *window, filenames []string)) {
	w.w.SetDropCallback(func(_ *w32.Window, filenames []string) {
		fn(w, filenames)
	})
}

func (w *window) Render() {
	var clearColor nk.Color
	nk.NkPlatformRender(nk.AntiAliasingOff, clearColor)
}

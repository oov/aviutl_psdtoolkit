// +build gdip

package gui

import (
	"github.com/golang-ui/nuklear/nk"
	"github.com/golang-ui/nuklear/nk/w32"
)

type font struct {
	f *nk.GdipFont
}

func (g *GUI) initFont(mainFont, symbolFont []byte) error {
	main, err := nk.NkCreateFontFromBytes(mainFont, 20)
	if err != nil {
		return err
	}
	symbol, err := nk.NkCreateFontFromBytes(symbolFont, 14)
	if err != nil {
		main.Close()
		return err
	}
	g.font.Main = &font{main}
	g.font.Symbol = &font{symbol}

	g.font.MainHandle = g.font.Main.f.Handle()
	g.font.SymbolHandle = g.font.Symbol.f.Handle()
	nk.NkStyleSetFont(g.context, g.font.MainHandle)
	return nil
}

func (g *GUI) freeFont() {
	g.font.SymbolHandle.Free()
	g.font.Symbol.f.Close()
	g.font.MainHandle.Free()
	g.font.Main.f.Close()
}

func (g *GUI) pollEvents() {
	w32.PollEvents()
}

func (g *GUI) terminate() {
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

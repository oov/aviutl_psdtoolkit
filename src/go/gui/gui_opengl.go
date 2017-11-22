// +build !gdip

package gui

import (
	"unsafe"

	"github.com/go-gl/gl/v3.2-core/gl"
	"github.com/go-gl/glfw/v3.2/glfw"
	"github.com/golang-ui/nuklear/nk"
	"github.com/oov/aviutl_psdtoolkit/src/go/nkhelper"
	"github.com/pkg/errors"
)

type font struct {
	f *nk.Font
}

func (g *GUI) initFont(mainFont, symbolFont []byte) error {
	atlas := nk.NewFontAtlas()
	nk.NkFontStashBegin(&atlas)
	fc := nk.NkFontConfig(15)
	nkhelper.SetJapaneseGlyphRanges(&fc)
	g.font.Main = &font{nk.NkFontAtlasAddFromBytes(atlas, mainFont, 20, &fc)}
	g.font.Symbol = &font{nk.NkFontAtlasAddFromBytes(atlas, symbolFont, 14, nil)}
	nk.NkFontStashEnd()
	g.font.MainHandle = g.font.Main.f.Handle()
	g.font.SymbolHandle = g.font.Symbol.f.Handle()

	nk.NkStyleSetFont(g.context, g.font.MainHandle)
	return nil
}

func (g *GUI) freeFont() {
	g.font.SymbolHandle.Free()
	g.font.Symbol.f.Free()
	g.font.MainHandle.Free()
	g.font.Main.f.Free()
}

func (g *GUI) pollEvents() {
	glfw.PollEvents()
}

func (g *GUI) terminate() {
	glfw.Terminate()
}

type window struct {
	w *glfw.Window
}

func newWindow(width, height int, title string) (*window, *nk.Context, error) {
	if err := glfw.Init(); err != nil {
		return nil, nil, errors.Wrap(err, "glfw.Init failed")
	}
	glfw.WindowHint(glfw.ContextVersionMajor, 3)
	glfw.WindowHint(glfw.ContextVersionMinor, 2)
	glfw.WindowHint(glfw.OpenGLProfile, glfw.OpenGLCoreProfile)
	glfw.WindowHint(glfw.OpenGLForwardCompatible, glfw.True)
	glfw.WindowHint(glfw.Visible, glfw.False)
	win, err := glfw.CreateWindow(width, height, title, nil, nil)
	if err != nil {
		return nil, nil, errors.Wrap(err, "glfw.CreateWindow failed")
	}
	win.MakeContextCurrent()
	if err := gl.Init(); err != nil {
		return nil, nil, errors.Wrap(err, "gl.Init failed")
	}
	ctx := nk.NkPlatformInit(win, nk.PlatformInstallCallbacks)
	return &window{w: win}, ctx, nil
}

func (w *window) Show()                 { w.w.Show() }
func (w *window) Hide()                 { w.w.Hide() }
func (w *window) GetSize() (int, int)   { return w.w.GetSize() }
func (w *window) SetShouldClose(v bool) { w.w.SetShouldClose(v) }
func (w *window) ShouldClose() bool     { return w.w.ShouldClose() }
func (w *window) NativeWindow() uintptr { return uintptr(unsafe.Pointer(w.w.GetWin32Window())) }

func (w *window) SetDropCallback(fn func(w *window, filenames []string)) {
	w.w.SetDropCallback(func(_ *glfw.Window, filenames []string) {
		fn(w, filenames)
	})
}

func (w *window) Render() {
	const (
		maxVertexBuffer  = 512 * 1024
		maxElementBuffer = 128 * 1024
	)

	width, height := w.w.GetSize()
	gl.Viewport(0, 0, int32(width), int32(height))
	gl.Clear(gl.COLOR_BUFFER_BIT)
	gl.ClearColor(0, 0, 0, 1)
	nk.NkPlatformRender(nk.AntiAliasingOn, maxVertexBuffer, maxElementBuffer)
	w.w.SwapBuffers()
}

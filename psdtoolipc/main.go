//go:generate go-bindata assets

package main

import (
	"bytes"
	"context"
	"flag"
	"image"
	"image/png"
	"log"
	"os"
	"runtime"
	"runtime/debug"
	"time"
	"unsafe"

	// _ "net/http/pprof"

	"github.com/go-gl/gl/v3.2-core/gl"
	"github.com/go-gl/glfw/v3.2/glfw"
	"github.com/golang-ui/nuklear/nk"

	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/img"
	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/ipc"
	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/layerview"
	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/mainview"
	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/ods"
	"github.com/oov/psd/layertree"
)

const (
	winWidth         = 1024
	winHeight        = 768
	topPaneHeight    = 56
	bottomPaneHeight = 56
	layerPaneWidth   = 320
	layerPaneHeight  = winHeight - bottomPaneHeight - topPaneHeight
	mainViewWidth    = winWidth - layerPaneWidth
	mainViewHeight   = layerPaneHeight

	maxVertexBuffer  = 512 * 1024
	maxElementBuffer = 128 * 1024
)

func init() {
	runtime.LockOSThread()
}

type file struct {
	*os.File
	size int64
}

func (f *file) Size() int64 {
	return f.size
}

type loadadData struct {
	Root *layertree.Root
	Err  error
}

type viewResizeMode int

const (
	vrmNone viewResizeMode = iota
	vrmFast
	vrmBeautiful
)

type gui struct {
	IPC     ipc.IPC
	Window  *glfw.Window
	Context *nk.Context

	cancelRender        context.CancelFunc
	cancelUpdateResized context.CancelFunc
	cancelViewResize    context.CancelFunc
	viewResizeRunning   viewResizeMode
	viewResizeQueued    viewResizeMode

	img           *img.Image
	renderedImage *image.RGBA

	minZoom  float32
	maxZoom  float32
	stepZoom float32
	zoom     float64
	zooming  bool

	LayerView layerview.LayerView
	MainView  mainview.MainView

	Font struct {
		Sans       *nk.Font
		SansHandle *nk.UserFont

		Symbol       *nk.Font
		SymbolHandle *nk.UserFont
	}
}

var mainfunc = make(chan func())

func do(f func()) {
	done := make(chan struct{})
	mainfunc <- func() {
		f()
		done <- struct{}{}
	}
	<-done
}

func freeMemory(d time.Duration, exitCh <-chan struct{}) {
	fpsTicker := time.NewTicker(d)
	for {
		select {
		case <-exitCh:
			fpsTicker.Stop()
			return
		case <-fpsTicker.C:
			debug.FreeOSMemory()
		}
	}
}

func main() {
	// go http.ListenAndServe(":6060", nil)

	defer func() {
		if err := recover(); err != nil {
			ods.Recover(err)
		}
	}()

	flag.Parse()

	g := &gui{
		minZoom:  -5,
		maxZoom:  0,
		stepZoom: 0.001,
		LayerView: layerview.LayerView{
			ImageListSelectedIndex: -1,
		},
	}
	g.IPC.Init()
	g.IPC.ShowGUI = func() (uintptr, error) {
		do(func() {
			g.Window.Show()
		})
		return uintptr(unsafe.Pointer(g.Window.GetWin32Window())), nil
	}

	exitCh := make(chan struct{})
	go g.IPC.Main(exitCh)
	go freeMemory(time.Second, exitCh)

	// psd.Debug = log.New(os.Stdout, "psd: ", log.Lshortfile)
	if err := glfw.Init(); err != nil {
		log.Fatalln(err)
	}
	glfw.WindowHint(glfw.ContextVersionMajor, 3)
	glfw.WindowHint(glfw.ContextVersionMinor, 2)
	glfw.WindowHint(glfw.OpenGLProfile, glfw.OpenGLCoreProfile)
	glfw.WindowHint(glfw.OpenGLForwardCompatible, glfw.True)
	glfw.WindowHint(glfw.Visible, glfw.False)
	win, err := glfw.CreateWindow(winWidth, winHeight, "PSDToolIPC", nil, nil)
	if err != nil {
		log.Fatalln(err)
	}
	g.Window = win

	win.MakeContextCurrent()
	if err := gl.Init(); err != nil {
		log.Fatalln("opengl: init failed:", err)
	}
	width, height := win.GetSize()
	gl.Viewport(0, 0, int32(width), int32(height))

	win.SetDropCallback(func(w *glfw.Window, filenames []string) {
		img, err := g.IPC.Image(0, extractPSDAndPFV(filenames))
		if err != nil {
			ods.ODS("error: %v", err)
			return
		}
		g.intializeView(img)
	})

	g.Context = nk.NkPlatformInit(win, nk.PlatformInstallCallbacks)
	g.initFont()
	g.LayerView.Init()

	bg, err := png.Decode(bytes.NewReader(MustAsset("assets/bg.png")))
	if err != nil {
		log.Fatalln("could not decode bg.png:", err)
	}
	g.MainView.Init(bg)
	// g.Window.Show()
	g.MainLoop(exitCh)
}

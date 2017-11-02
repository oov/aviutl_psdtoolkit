package main

import (
	"bytes"
	"context"
	"flag"
	"image"
	"image/png"
	"runtime"

	// _ "net/http/pprof"

	"github.com/golang-ui/nuklear/nk"
	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolkit/src/go/assets"
	"github.com/oov/aviutl_psdtoolkit/src/go/gc"
	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/ipc"
	"github.com/oov/aviutl_psdtoolkit/src/go/layerview"
	"github.com/oov/aviutl_psdtoolkit/src/go/mainview"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
)

const (
	winWidth         = 1024
	winHeight        = 768
	topPaneHeight    = 48
	bottomPaneHeight = 56
	layerPaneWidth   = 320
	layerPaneHeight  = winHeight - bottomPaneHeight - topPaneHeight
	mainViewWidth    = winWidth - layerPaneWidth
	mainViewHeight   = layerPaneHeight
)

func init() {
	runtime.LockOSThread()
}

type viewResizeMode int

const (
	vrmNone viewResizeMode = iota
	vrmFast
	vrmBeautiful
)

type gui struct {
	IPC     ipc.IPC
	Window  *window
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

	ImageList              []string
	ImageListSelected      string
	ImageListSelectedIndex int

	LayerView layerview.LayerView
	MainView  mainview.MainView

	Font struct {
		Sans       *font
		SansHandle *nk.UserFont

		Symbol       *font
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

		ImageListSelectedIndex: -1,
	}
	g.IPC.Init()
	g.IPC.ShowGUI = func() (uintptr, error) {
		do(func() {
			g.Window.Show()
		})
		return g.Window.NativeWindow(), nil
	}
	g.LayerView.CopyToClipboard = func(sliderName, name, value string) {
		if err := g.IPC.CopyFaviewValue(*g.img.FilePath, sliderName, name, value); err != nil {
			ods.ODS("cannot copy to the clipboard: %v", err)
		}
	}
	g.LayerView.ExportFaviewSlider = func(sliderName string, names, values []string) {
		if err := g.IPC.ExportFaviewSlider(*g.img.FilePath, sliderName, names, values); err != nil {
			ods.ODS("cannot export faview slider: %v", err)
		}
	}

	exitCh := make(chan struct{})
	go g.IPC.Main(exitCh)
	gcDone := gc.Start(exitCh)

	// psd.Debug = log.New(os.Stdout, "psd: ", log.Lshortfile)
	var err error
	if g.Window, g.Context, err = newWindow(winWidth, winHeight, "PSDToolKit "+version); err != nil {
		ipc.Fatal(err)
	}

	dropCB := func(w *window, filenames []string) {
		gc.EnterCS()
		go do(gc.LeaveCS)
		img, idx, stateKey, err := g.IPC.Image(0, extractPSDAndPFV(filenames))
		if err != nil {
			ods.ODS("error: %v", err)
			return
		}
		g.ImageListSelectedIndex = idx
		g.ImageListSelected = stateKey.String()
		g.intializeView(img)
	}
	g.Window.SetDropCallback(dropCB)

	if err = g.initFont(); err != nil {
		ipc.Fatal(errors.Wrap(err, "gui.initFont failed"))
	}
	g.LayerView.Init()

	bg, err := png.Decode(bytes.NewReader(assets.MustAsset("bg.png")))
	if err != nil {
		ipc.Fatal(errors.Wrap(err, "could not decode bg.png"))
	}
	g.MainView.Init(bg)
	// g.Window.Show()
	g.MainLoop(exitCh)
	<-gcDone
}

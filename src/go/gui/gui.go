package gui

import (
	"bytes"
	"context"
	"image"
	"image/png"
	"math"
	"time"

	"github.com/golang-ui/nuklear/nk"
	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolkit/src/go/gc"
	"github.com/oov/aviutl_psdtoolkit/src/go/gui/layerview"
	"github.com/oov/aviutl_psdtoolkit/src/go/gui/mainview"
	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/editing"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/source"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
)

const (
	winWidth          = 1024
	winHeight         = 768
	topPaneHeight     = 48
	bottomPaneHeight  = 56
	topRightPaneWidth = 280
	layerPaneWidth    = 320
	layerPaneHeight   = winHeight - bottomPaneHeight - topPaneHeight
	mainViewWidth     = winWidth - layerPaneWidth
	mainViewHeight    = layerPaneHeight
)

type viewResizeMode int

const (
	vrmNone viewResizeMode = iota
	vrmFast
	vrmBeautiful
)

type GUI struct {
	queue chan func()

	window  *window
	context *nk.Context

	edImg editing.Editing

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

	layerView *layerview.LayerView
	mainView  *mainview.MainView

	font struct {
		Main       *font
		MainHandle *nk.UserFont

		Symbol       *font
		SymbolHandle *nk.UserFont
	}

	SendEditingImageState func(path, state string) error
	CopyFaviewValue       func(path, sliderName, name, value string) error
	ExportFaviewSlider    func(path, sliderName string, names, values []string) error
	DropFiles             func(filenames []string)
}

func New(Srcs *source.Sources) *GUI {
	g := &GUI{
		queue: make(chan func()),
		edImg: editing.Editing{Srcs: Srcs},

		minZoom:  -5,
		maxZoom:  0,
		stepZoom: 0.001,
	}
	return g
}

func (g *GUI) AddFile(path string) error {
	// TODO: We do not want to rely on gc package.
	gc.EnterCS()
	go g.do(gc.LeaveCS)

	if _, err := g.edImg.Add(path); err != nil {
		return err
	}
	g.changeSelectedImage()
	return nil
}

func (g *GUI) Init(caption string, bgImg, mainFont, symbolFont []byte) error {
	var err error
	if g.window, g.context, err = newWindow(winWidth, winHeight, caption); err != nil {
		return errors.Wrap(err, "gui: failed to create a new window")
	}
	g.window.SetDropCallback(func(w *window, filenames []string) {
		g.DropFiles(filenames)
	})

	if err = g.initFont(mainFont, symbolFont); err != nil {
		return errors.Wrap(err, "gui: failed to load a font")
	}
	g.layerView, err = layerview.New(g.font.MainHandle, g.font.SymbolHandle)
	if err != nil {
		return errors.Wrap(err, "gui: failed to initialize layerview")
	}
	g.layerView.ReportError = g.ReportError
	g.layerView.CopyFaviewValue = func(path, sliderName, name, value string) {
		if err := g.CopyFaviewValue(path, sliderName, name, value); err != nil {
			g.ReportError(errors.Wrap(err, "gui: cannot copy to the clipboard"))
		}
	}
	g.layerView.ExportFaviewSlider = func(path, sliderName string, names, values []string) {
		if err := g.ExportFaviewSlider(path, sliderName, names, values); err != nil {
			g.ReportError(errors.Wrap(err, "gui: cannot export faview slider"))
		}
	}

	bg, err := png.Decode(bytes.NewReader(bgImg))
	if err != nil {
		return errors.Wrap(err, "gui: could not decode bg.png")
	}
	g.mainView, err = mainview.New(bg)
	if err != nil {
		return errors.Wrap(err, "gui: failed to initialize mainview")
	}
	return nil
}

func (g *GUI) do(f func()) {
	done := make(chan struct{})
	g.queue <- func() {
		f()
		done <- struct{}{}
	}
	<-done
}

func (g *GUI) Main(exitCh <-chan struct{}) {
	defer func() {
		if err := recover(); err != nil {
			ods.Recover(err)
		}
		g.freeFont()
		nk.NkPlatformShutdown()
		g.terminate()
	}()
	fpsTicker := time.NewTicker(time.Second / 30)
	for {
		select {
		case f := <-g.queue:
			f()

		case <-exitCh:
			fpsTicker.Stop()
			return

		case <-fpsTicker.C:
			g.pollEvents()
			if g.window.ShouldClose() {
				g.window.Hide()
				g.window.SetShouldClose(false)
			}
			g.update()
		}
	}
}

func b2i(b bool) int32 {
	if b {
		return 1
	}
	return 0
}

func (g *GUI) changeSelectedImage() {
	img := g.edImg.SelectedImage()
	g.img = img
	g.renderedImage = nil
	g.mainView.Clear()
	if img == nil {
		return
	}

	// makes fit to the main view at initial look
	targetRect := g.mainView.LatestActiveRect()
	if targetRect.Empty() {
		targetRect.Max.X += winWidth - layerPaneWidth
		targetRect.Max.Y += winHeight - bottomPaneHeight
	}
	z := float64(targetRect.Dx()) / float64(img.PSD.CanvasRect.Dx())
	if z*float64(img.PSD.CanvasRect.Dy()) > float64(targetRect.Dy()) {
		z = float64(targetRect.Dy()) / float64(img.PSD.CanvasRect.Dy())
	}
	g.zoom = math.Log(z) / math.Ln2
	g.mainView.ScrollToCenter()

	updateRenderedImage(g, g.img)
	g.layerView.UpdateThumbnails(img.PSD, 24, g.do)
}

func (g *GUI) update() {
	ctx := g.context
	nk.NkPlatformNewFrame()
	width, height := g.window.GetSize()

	modified := false
	if nk.NkBegin(ctx, "TopLeftPane", nk.NkRect(0, 0, float32(width-topRightPaneWidth), topPaneHeight), 0) != 0 {
		nk.NkLayoutRowDynamic(ctx, 28, 1)
		n0 := g.edImg.SelectedIndex
		n1 := int(nk.NkComboString(ctx, g.edImg.StringList(), int32(n0), int32(g.edImg.Len()), 28, nk.NkVec2(600, float32(height))))
		if n0 != n1 {
			g.edImg.SelectedIndex = n1
			g.changeSelectedImage()
		}
	}
	nk.NkEnd(ctx)

	if nk.NkBegin(ctx, "TopRightPane", nk.NkRect(float32(width-topRightPaneWidth), 0, float32(topRightPaneWidth), topPaneHeight), 0) != 0 {
		nk.NkLayoutRowDynamic(ctx, 28, 3)
		if g.img != nil {
			fx, fy := g.img.FlipX(), g.img.FlipY()
			if (nk.NkSelectLabel(ctx, "⇆", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(fx)) != 0) != fx {
				modified = g.img.SetFlipX(!fx) || modified
			}
			if (nk.NkSelectLabel(ctx, "⇅", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(fy)) != 0) != fy {
				modified = g.img.SetFlipY(!fy) || modified
			}
			if nk.NkButtonLabel(ctx, "送る") != 0 {
				g.sendEditingImage()
			}
		}
	}
	nk.NkEnd(ctx)

	modified = g.layerView.Render(ctx, nk.NkRect(0, topPaneHeight, layerPaneWidth, float32(height-topPaneHeight)), g.img) || modified
	if modified {
		g.img.Modified = true
		g.img.Layers.Normalize(g.img.Flip)
		updateRenderedImage(g, g.img)
	}

	if nk.NkBegin(ctx, "BottomPane", nk.NkRect(layerPaneWidth, float32(height-bottomPaneHeight), float32(width-layerPaneWidth), bottomPaneHeight), 0) != 0 {
		nk.NkLayoutRowDynamic(ctx, 0, 1)
		if z := float64(nk.NkSlideFloat(ctx, g.minZoom, float32(g.zoom), g.maxZoom, g.stepZoom)); z != g.zoom {
			if !g.zooming {
				g.zooming = true
			}
			g.zoom = z
			if g.renderedImage != nil {
				updateViewImage(g, g.renderedImage, true)
			}
		} else if (ctx.LastWidgetState()&nk.WidgetStateActive) != nk.WidgetStateActive && g.zooming {
			g.zooming = false
			if g.renderedImage != nil {
				updateViewImage(g, g.renderedImage, false)
			}
		}
	}
	nk.NkEnd(ctx)

	g.mainView.Render(ctx, nk.NkRect(layerPaneWidth, topPaneHeight, float32(width-layerPaneWidth), float32(height-bottomPaneHeight-topPaneHeight)), g.zoom)

	// Render
	g.window.Render()
}

func (g *GUI) sendEditingImage() {
	state, err := g.img.Serialize()
	if err != nil {
		g.ReportError(errors.Wrap(err, "gui: cannot serialize"))
		return
	}
	err = g.SendEditingImageState(*g.img.FilePath, state)
	if err != nil {
		g.ReportError(errors.Wrap(err, "gui: cannot send editing image state"))
	}
}

func (g *GUI) ReportError(err error) {
	//TODO: improve error handling
	ods.ODS("error: %v", err)
}

func (g *GUI) ShowWindow() (uintptr, error) {
	var h uintptr
	g.do(func() {
		g.window.Show()
		h = g.window.NativeWindow()
	})
	return h, nil
}

func (g *GUI) Serialize() (string, error) {
	var s string
	var err error
	g.do(func() {
		s, err = g.edImg.Serialize()
	})
	return s, err
}

func (g *GUI) Deserialize(state string) error {
	var err error
	g.do(func() {
		err = g.edImg.Deserialize(state)
		if err == nil {
			g.changeSelectedImage()
		}
	})
	return err
}

func (g *GUI) Touch() {
	g.do(func() {
		g.edImg.Touch()
	})

}

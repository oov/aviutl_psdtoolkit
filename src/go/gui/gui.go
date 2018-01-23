package gui

import (
	"bytes"
	"context"
	"image/png"
	"time"

	"github.com/golang-ui/nuklear/nk"
	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolkit/src/go/gc"
	"github.com/oov/aviutl_psdtoolkit/src/go/gui/layerview"
	"github.com/oov/aviutl_psdtoolkit/src/go/gui/mainview"
	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/editing"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/source"
	"github.com/oov/aviutl_psdtoolkit/src/go/nkhelper"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
)

const (
	winWidth          = 1024
	winHeight         = 768
	topPaneHeight     = 30
	topRightPaneWidth = 280
	layerPaneWidth    = 320
)

type GUI struct {
	queue chan func()

	window  *window
	context *nk.Context

	edImg editing.Editing

	cancelRender context.CancelFunc

	img           *img.Image

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
	g.mainView.SetZoomRange(-5, 0, 0.001)
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
	g.mainView.Clear()
	if img == nil {
		return
	}

	updateRenderedImage(g, img)
	g.layerView.UpdateThumbnails(img.PSD, 24, g.do)
}

func (g *GUI) update() {
	ctx := g.context
	nk.NkPlatformNewFrame()
	width, height := g.window.GetSize()
	const PADDING = 2

	modified := false

	nk.NkStylePushVec2(ctx, nkhelper.GetStyleWindowPaddingPtr(ctx), nk.NkVec2(0, 0))
	nk.NkStylePushVec2(ctx, nkhelper.GetStyleWindowGroupPaddingPtr(ctx), nk.NkVec2(0, 0))

	if nk.NkBegin(ctx, "MainWindow", nk.NkRect(0, 0, float32(width), float32(height)), nk.WindowNoScrollbar) != 0 {
		nk.NkLayoutRowBegin(ctx, nk.Static, topPaneHeight-PADDING, 2)

		nk.NkLayoutRowPush(ctx, float32(width-topRightPaneWidth-PADDING))
		if nk.NkGroupBegin(ctx, "TabPane", nk.WindowNoScrollbar) != 0 {
			nk.NkLayoutRowDynamic(ctx, 28, 1)
			n0 := g.edImg.SelectedIndex
			n1 := int(nk.NkComboString(ctx, g.edImg.StringList(), int32(n0), int32(g.edImg.Len()), 28, nk.NkVec2(600, float32(height))))
			if n0 != n1 {
				g.edImg.SelectedIndex = n1
				g.changeSelectedImage()
			}
			nk.NkGroupEnd(ctx)
		}

		nk.NkLayoutRowPush(ctx, float32(topRightPaneWidth-PADDING))
		if nk.NkGroupBegin(ctx, "FilpSendPane", nk.WindowNoScrollbar) != 0 {
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
			nk.NkGroupEnd(ctx)
		}

		nk.NkLayoutRowEnd(ctx)

		nk.NkLayoutRowBegin(ctx, nk.Static, float32(height-topPaneHeight-PADDING), 2)

		nk.NkLayoutRowPush(ctx, float32(layerPaneWidth-PADDING))
		if nk.NkGroupBegin(ctx, "LayerTreePane", nk.WindowNoScrollbar) != 0 {
			modified = g.layerView.Render(ctx, g.img) || modified
			if modified {
				g.img.Modified = true
				g.img.Layers.Normalize(g.img.Flip)
				updateRenderedImage(g, g.img)
			}
			nk.NkGroupEnd(ctx)
		}

		nk.NkLayoutRowPush(ctx, float32(width-layerPaneWidth-PADDING))
		if nk.NkGroupBegin(ctx, "MainPane", nk.WindowNoScrollbar) != 0 {
			g.mainView.Render(ctx)
			nk.NkGroupEnd(ctx)
		}
		nk.NkLayoutRowEnd(ctx)
	}
	nk.NkEnd(ctx)

	nk.NkStylePopVec2(ctx)
	nk.NkStylePopVec2(ctx)

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

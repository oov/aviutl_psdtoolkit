package main

import (
	"math"
	"time"

	"github.com/golang-ui/nuklear/nk"

	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
)

func (g *gui) MainLoop(exitCh <-chan struct{}) {
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
		case f := <-mainfunc:
			f()

		case <-exitCh:
			fpsTicker.Stop()
			return

		case <-fpsTicker.C:
			g.pollEvents()
			if g.Window.ShouldClose() {
				g.Window.Hide()
				g.Window.SetShouldClose(false)
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

func (g *gui) update() {
	ctx := g.Context
	nk.NkPlatformNewFrame()
	width, height := g.Window.GetSize()

	modified := false
	rootChanged := false
	if nk.NkBegin(ctx, "TopPane", nk.NkRect(0, 0, float32(width), topPaneHeight), 0) != 0 {
		nk.NkLayoutRowDynamic(ctx, 28, 4)
		n0 := g.ImageList.SelectedIndex
		n1 := int(nk.NkComboString(ctx, g.ImageList.List(), int32(n0), int32(g.ImageList.Len()), 28, nk.NkVec2(600, float32(height))))
		if n0 != n1 {
			rootChanged = true
			g.ImageList.SelectedIndex = n1
		}

		if g.img != nil {
			fx, fy := g.img.FlipX(), g.img.FlipY()
			if (nk.NkSelectLabel(ctx, "⇆", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(fx)) != 0) != fx {
				modified = g.img.SetFlipX(!fx) || modified
			}
			if (nk.NkSelectLabel(ctx, "⇅", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(fy)) != 0) != fy {
				modified = g.img.SetFlipY(!fy) || modified
			}
			if nk.NkButtonLabel(ctx, "送る") != 0 {
				if err := g.IPC.SendEditingImageState(*g.img.FilePath, g.img.Serialize()); err != nil {
					ods.ODS("error: %v", err)
				}
			}
		}
	}
	nk.NkEnd(ctx)

	modifiedLayer := g.LayerView.Render(ctx, nk.NkRect(0, topPaneHeight, layerPaneWidth, float32(height-topPaneHeight)), g.img)
	if rootChanged {
		if g.ImageList.Empty() {
			g.img = nil
			g.renderedImage = nil
			g.MainView.Clear()
		} else {
			key := g.ImageList.SelectedKey()
			img, err := g.IPC.Image(key.ID2, key.FilePath)
			if err != nil {
				ods.ODS("error: %v", err)
			} else {
				g.intializeView(img)
			}
		}
	} else if modified || modifiedLayer {
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

	g.MainView.Render(ctx, nk.NkRect(layerPaneWidth, topPaneHeight, float32(width-layerPaneWidth), float32(height-bottomPaneHeight-topPaneHeight)), g.zoom)

	// Render
	g.Window.Render()
}

func (g *gui) intializeView(img *img.Image) {
	g.img = img
	g.renderedImage = nil
	g.MainView.Clear()

	// makes fit to the main view at initial look
	targetRect := g.MainView.LatestActiveRect
	if targetRect.Empty() {
		targetRect.Max.X += winWidth - layerPaneWidth
		targetRect.Max.Y += winHeight - bottomPaneHeight
	}
	z := float64(targetRect.Dx()) / float64(img.PSD.CanvasRect.Dx())
	if z*float64(img.PSD.CanvasRect.Dy()) > float64(targetRect.Dy()) {
		z = float64(targetRect.Dy()) / float64(img.PSD.CanvasRect.Dy())
	}
	g.zoom = math.Log(z) / math.Ln2
	g.MainView.ForceScrollToCenter = true

	updateRenderedImage(g, g.img)
	g.LayerView.UpdateThumbnails(img.PSD, 24, do)
}

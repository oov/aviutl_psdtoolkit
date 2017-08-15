package main

import (
	"math"
	"time"

	"github.com/go-gl/gl/v3.2-core/gl"
	"github.com/go-gl/glfw/v3.2/glfw"
	"github.com/golang-ui/nuklear/nk"

	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/img"
	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/nkhelper"
	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/ods"
)

func (g *gui) initFont() {
	atlas := nk.NewFontAtlas()
	nk.NkFontStashBegin(&atlas)
	fc := nk.NkFontConfig(15)
	nkhelper.SetJapaneseGlyphRanges(&fc)
	g.Font.Sans = nk.NkFontAtlasAddFromBytes(atlas, MustAsset("assets/Ohruri-Regular.ttf"), 20, &fc)
	g.Font.Symbol = nk.NkFontAtlasAddFromBytes(atlas, MustAsset("assets/symbols.ttf"), 14, nil)
	nk.NkFontStashEnd()
	g.Font.SansHandle = g.Font.Sans.Handle()
	g.Font.SymbolHandle = g.Font.Symbol.Handle()

	nk.NkStyleSetFont(g.Context, g.Font.SansHandle)
	g.LayerView.SymbolFontHandle = g.Font.SymbolHandle
}

func (g *gui) freeFont() {
	g.Font.SymbolHandle.Free()
	g.Font.Symbol.Free()
	g.Font.SansHandle.Free()
	g.Font.Sans.Free()
}

func (g *gui) MainLoop(exitCh <-chan struct{}) {
	defer func() {
		if err := recover(); err != nil {
			ods.Recover(err)
		}
		g.freeFont()
		nk.NkPlatformShutdown()
		glfw.Terminate()
	}()
	fpsTicker := time.NewTicker(time.Second / 60)
	for {
		select {
		case f := <-mainfunc:
			f()

		case <-exitCh:
			fpsTicker.Stop()
			return

		case <-fpsTicker.C:
			if g.Window.ShouldClose() {
				g.Window.Hide()
				g.Window.SetShouldClose(false)
			}
			glfw.PollEvents()
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
	if nk.NkBegin(ctx, "TopPane", nk.NkRect(0, 0, float32(width), topPaneHeight), 0) != 0 {
		if g.img != nil {
			nk.NkLayoutRowDynamic(ctx, 0, 3)
			fx, fy := g.img.Layers.FlipX(), g.img.Layers.FlipY()
			if (nk.NkSelectLabel(ctx, "左右反転", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(fx)) != 0) != fx {
				modified = g.img.Layers.SetFlipX(!fx) || modified
			}
			if (nk.NkSelectLabel(ctx, "上下反転", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(fy)) != 0) != fy {
				modified = g.img.Layers.SetFlipY(!fy) || modified
			}
			if nk.NkButtonLabel(ctx, "送る") != 0 {
				if err := g.IPC.SendEditingImageState(g.img.Serialize()); err != nil {
					ods.ODS("error: %v", err)
				}
			}
		}
	}
	nk.NkEnd(ctx)

	imageList, keys := g.IPC.ImageList()
	g.LayerView.ImageList = imageList
	rootChanged, modifiedLayer := g.LayerView.Render(ctx, nk.NkRect(0, topPaneHeight, layerPaneWidth, float32(height-topPaneHeight)), g.img)
	if rootChanged {
		if g.LayerView.ImageListSelectedIndex != -1 {
			key := keys[g.LayerView.ImageListSelectedIndex]
			img, err := g.IPC.Image(key.ID, key.FilePath)
			if err != nil {
				ods.ODS("error: %v", err)
			} else {
				g.intializeView(img)
			}
		} else {
			g.img = nil
			g.renderedImage = nil
			g.MainView.Clear()
		}
	} else if modified || modifiedLayer {
		g.img.Modified = true
		g.img.Layers.Normalize()
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
	gl.Viewport(0, 0, int32(width), int32(height))
	gl.Clear(gl.COLOR_BUFFER_BIT)
	gl.ClearColor(0, 0, 0, 1)
	nk.NkPlatformRender(nk.AntiAliasingOn, maxVertexBuffer, maxElementBuffer)
	g.Window.SwapBuffers()
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
	g.LayerView.UpdateThumbnails(img.PSD, 64, do)
}

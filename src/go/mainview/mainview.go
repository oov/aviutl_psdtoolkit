package mainview

import (
	"image"
	"math"

	"github.com/golang-ui/nuklear/nk"
	"golang.org/x/image/draw"

	"github.com/oov/aviutl_psdtoolkit/src/go/nkhelper"
	"github.com/oov/psd/blend"
)

type MainView struct {
	ResizedImage *image.NRGBA

	VisibleAreaImage *image.NRGBA
	VisibleArea      *nkhelper.Texture

	LatestImageRect     image.Rectangle
	LatestActiveRect    image.Rectangle
	LatestWorkspaceSize image.Point

	BG       *nkhelper.Texture
	BGScroll float32

	ForceUpdate         bool
	ForceScrollToCenter bool
}

func (mv *MainView) Init(bg image.Image) {
	nrgba := image.NewNRGBA(bg.Bounds())
	draw.Draw(nrgba, nrgba.Rect, bg, image.Point{}, draw.Src)
	var err error
	mv.BG, err = nkhelper.NewTexture(nrgba)
	if err != nil {
		panic(err)
	}

	mv.VisibleAreaImage = image.NewNRGBA(image.Rect(0, 0, 1, 1))
	mv.VisibleArea, err = nkhelper.NewTexture(mv.VisibleAreaImage)
	if err != nil {
		panic(err)
	}
}

func (mv *MainView) Clear() {
	mv.ResizedImage = nil
	mv.VisibleAreaImage = image.NewNRGBA(image.Rect(0, 0, 1, 1))
	mv.VisibleArea.Update(mv.VisibleAreaImage)
}

func (mv *MainView) Render(ctx *nk.Context, winRect nk.Rect, zoom float64) {
	winWidth, winHeight := int(winRect.W()), int(winRect.H())

	// calculate the overall size of the main view
	zr, izr := float64(1), float64(1)
	if zoom > 0 {
		zr, izr = math.Pow(2, zoom), math.Pow(2, -zoom)
	}

	imageRect := image.Rect(
		winWidth,
		winHeight,
		winWidth,
		winHeight,
	)
	if mv.ResizedImage != nil {
		imageRect.Max.X += int(float64(mv.ResizedImage.Rect.Dx()) * zr)
		imageRect.Max.Y += int(float64(mv.ResizedImage.Rect.Dy()) * zr)
	}
	workspaceSize := image.Pt(
		winWidth*2+imageRect.Dx(),
		winHeight*2+imageRect.Dy(),
	)

	if !workspaceSize.Eq(mv.LatestWorkspaceSize) || (mv.ResizedImage != nil && mv.ForceScrollToCenter) {
		if wnd := nk.NkWindowFind(ctx, "MainView"); wnd != nil {
			latestWinWidth := (mv.LatestWorkspaceSize.X - mv.LatestImageRect.Dx()) / 2
			latestWinHeight := (mv.LatestWorkspaceSize.Y - mv.LatestImageRect.Dy()) / 2
			var x, y float64
			if mv.ResizedImage != nil && mv.ForceScrollToCenter {
				x = 0.5
				y = 0.5
				mv.ForceScrollToCenter = false
			} else {
				r := nkhelper.GetWindowScrollBarOffset(wnd)
				x = (float64(r.X()) - float64(latestWinWidth)*0.5) / float64(mv.LatestImageRect.Dx())
				y = (float64(r.Y()) - float64(latestWinHeight)*0.5) / float64(mv.LatestImageRect.Dy())
			}
			nkhelper.SetWindowScrollBarOffset(wnd, nk.NkVec2(
				float32(math.Floor(0.5+float64(imageRect.Dx())*x+float64(winWidth)*0.5)),
				float32(math.Floor(0.5+float64(imageRect.Dy())*y+float64(winHeight)*0.5)),
			))
		}
	}

	if nk.NkBegin(ctx, "MainView", winRect, 0) != 0 {
		nk.NkLayoutSpaceBegin(ctx, nk.Static, float32(workspaceSize.Y), 3)
		nk.NkLayoutSpacePush(ctx, nk.NkRect(0, 0, float32(workspaceSize.X), float32(workspaceSize.Y)))
		nk.NkLabel(ctx, "", 0)

		ofs := nkhelper.GetPanelOffset(nk.NkWindowGetPanel(ctx))
		rgn := nk.NkWindowGetContentRegion(ctx)
		activeRect := image.Rect(int(ofs.X()), int(ofs.Y()), int(ofs.X()+rgn.W()), int(ofs.Y()+rgn.H()))

		if mv.ResizedImage != nil {
			if !activeRect.Eq(mv.LatestActiveRect) || !workspaceSize.Eq(mv.LatestWorkspaceSize) || mv.ForceUpdate {
				if activeRect.Dx() > mv.VisibleAreaImage.Rect.Dx() || activeRect.Dy() > mv.VisibleAreaImage.Rect.Dy() {
					var textureSize int
					if activeRect.Dx() > activeRect.Dy() {
						textureSize = int(math.Pow(2, math.Ceil(math.Log(float64(activeRect.Dx()))/math.Ln2)))
					} else {
						textureSize = int(math.Pow(2, math.Ceil(math.Log(float64(activeRect.Dy()))/math.Ln2)))
					}
					mv.VisibleAreaImage = image.NewNRGBA(image.Rect(0, 0, textureSize, textureSize))
				}
				blend.Clear.Draw(mv.VisibleAreaImage, image.Rect(0, 0, activeRect.Dx(), activeRect.Dy()), image.Transparent, image.Point{})
				ix := int(float64(activeRect.Min.X-(workspaceSize.X-imageRect.Dx())/2) * izr)
				iy := int(float64(activeRect.Min.Y-(workspaceSize.Y-imageRect.Dy())/2) * izr)
				draw.NearestNeighbor.Scale(
					mv.VisibleAreaImage,
					image.Rect(0, 0, activeRect.Dx(), activeRect.Dy()),
					mv.ResizedImage,
					image.Rect(ix, iy, ix+int(float64(activeRect.Dx())*izr), iy+int(float64(activeRect.Dy())*izr)),
					draw.Src,
					nil,
				)
				mv.VisibleArea.Update(mv.VisibleAreaImage)
				mv.ForceUpdate = false
			}

			// background animation
			nk.NkLayoutSpacePush(ctx, nk.NkRect(
				float32(imageRect.Min.X),
				float32(imageRect.Min.Y),
				float32(imageRect.Dx()-1),
				float32(imageRect.Dy()-1),
			))
			cmdbuf := nk.NkWindowGetCanvas(ctx)
			var widgetRect nk.Rect
			layoutState := nk.NkWidget(&widgetRect, ctx)
			if layoutState != 0 {
				var col nk.Color
				col.SetRGBA(255, 255, 255, 255)
				h := float32(256 - 16)
				for y, yEnd := widgetRect.Y(), widgetRect.Y()+widgetRect.H(); y < yEnd; y += h {
					if y+h > yEnd {
						h = yEnd - y
					}
					w := float32(256 - 16)
					for x, xEnd := widgetRect.X(), widgetRect.X()+widgetRect.W(); x < xEnd; x += w {
						if x+w > xEnd {
							w = xEnd - x
						}
						img := mv.BG.SubImage(nk.NkRect(16-mv.BGScroll, 16-mv.BGScroll, w, h))
						nk.NkDrawImage(cmdbuf, nk.NkRect(x, y, w, h), &img, col)
					}
				}
			}
			mv.BGScroll += 0.5
			if mv.BGScroll > 8 {
				mv.BGScroll -= 8
			}

			img := mv.VisibleArea.SubImage(nk.NkRect(0, 0, rgn.W(), rgn.H()))
			nk.NkLayoutSpacePush(ctx, nk.NkRect(ofs.X(), ofs.Y(), rgn.W(), rgn.H()))
			nk.NkImage(ctx, img)
		}
		nk.NkLayoutSpaceEnd(ctx)
		mv.LatestImageRect = imageRect
		mv.LatestActiveRect = activeRect
		mv.LatestWorkspaceSize = workspaceSize

	}
	nk.NkEnd(ctx)
}

package mainview

import (
	"image"
	"math"

	"github.com/golang-ui/nuklear/nk"
	"github.com/pkg/errors"
	"golang.org/x/image/draw"

	"github.com/oov/aviutl_psdtoolkit/src/go/nkhelper"
	"github.com/oov/psd/blend"
)

type MainView struct {
	resizedImage *image.NRGBA

	visibleAreaImage *image.NRGBA
	visibleArea      *nkhelper.Texture

	latestImageRect     image.Rectangle
	latestActiveRect    image.Rectangle
	latestWorkspaceSize image.Point

	bg       *nkhelper.Texture
	bgScroll float32

	forceUpdate         bool
	forceScrollToCenter bool
}

func New(bg image.Image) (*MainView, error) {
	mv := &MainView{}
	nrgba := image.NewNRGBA(bg.Bounds())
	draw.Draw(nrgba, nrgba.Rect, bg, image.Point{}, draw.Src)
	var err error
	mv.bg, err = nkhelper.NewTexture(nrgba)
	if err != nil {
		return nil, errors.Wrap(err, "cannot create a new texture")
	}

	mv.visibleAreaImage = image.NewNRGBA(image.Rect(0, 0, 1, 1))
	mv.visibleArea, err = nkhelper.NewTexture(mv.visibleAreaImage)
	if err != nil {
		return nil, errors.Wrap(err, "cannot create a new texture")
	}
	return mv, nil
}

func (mv *MainView) LatestActiveRect() image.Rectangle {
	return mv.latestActiveRect
}

func (mv *MainView) ScrollToCenter() {
	mv.forceScrollToCenter = true
}

func (mv *MainView) UpdateImage(img *image.NRGBA) {
	mv.resizedImage = img
	mv.forceUpdate = true
}

func (mv *MainView) Clear() {
	mv.resizedImage = nil
	mv.visibleAreaImage = image.NewNRGBA(image.Rect(0, 0, 1, 1))
	mv.visibleArea.Update(mv.visibleAreaImage)
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
	if mv.resizedImage != nil {
		imageRect.Max.X += int(float64(mv.resizedImage.Rect.Dx()) * zr)
		imageRect.Max.Y += int(float64(mv.resizedImage.Rect.Dy()) * zr)
	}
	workspaceSize := image.Pt(
		winWidth*2+imageRect.Dx(),
		winHeight*2+imageRect.Dy(),
	)

	if !workspaceSize.Eq(mv.latestWorkspaceSize) || (mv.resizedImage != nil && mv.forceScrollToCenter) {
		if wnd := nk.NkWindowFind(ctx, "MainView"); wnd != nil {
			latestWinWidth := (mv.latestWorkspaceSize.X - mv.latestImageRect.Dx()) / 2
			latestWinHeight := (mv.latestWorkspaceSize.Y - mv.latestImageRect.Dy()) / 2
			var x, y float64
			if mv.resizedImage != nil && mv.forceScrollToCenter {
				x = 0.5
				y = 0.5
				mv.forceScrollToCenter = false
			} else {
				r := nkhelper.GetWindowScrollBarOffset(wnd)
				x = (float64(r.X()) - float64(latestWinWidth)*0.5) / float64(mv.latestImageRect.Dx())
				y = (float64(r.Y()) - float64(latestWinHeight)*0.5) / float64(mv.latestImageRect.Dy())
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

		if mv.resizedImage != nil {
			if !activeRect.Eq(mv.latestActiveRect) || !workspaceSize.Eq(mv.latestWorkspaceSize) || mv.forceUpdate {
				if activeRect.Dx() > mv.visibleAreaImage.Rect.Dx() || activeRect.Dy() > mv.visibleAreaImage.Rect.Dy() {
					var textureSize int
					if activeRect.Dx() > activeRect.Dy() {
						textureSize = int(math.Pow(2, math.Ceil(math.Log(float64(activeRect.Dx()))/math.Ln2)))
					} else {
						textureSize = int(math.Pow(2, math.Ceil(math.Log(float64(activeRect.Dy()))/math.Ln2)))
					}
					mv.visibleAreaImage = image.NewNRGBA(image.Rect(0, 0, textureSize, textureSize))
				}
				blend.Clear.Draw(mv.visibleAreaImage, image.Rect(0, 0, activeRect.Dx(), activeRect.Dy()), image.Transparent, image.Point{})
				ix := int(float64(activeRect.Min.X-(workspaceSize.X-imageRect.Dx())/2) * izr)
				iy := int(float64(activeRect.Min.Y-(workspaceSize.Y-imageRect.Dy())/2) * izr)
				draw.NearestNeighbor.Scale(
					mv.visibleAreaImage,
					image.Rect(0, 0, activeRect.Dx(), activeRect.Dy()),
					mv.resizedImage,
					image.Rect(ix, iy, ix+int(float64(activeRect.Dx())*izr), iy+int(float64(activeRect.Dy())*izr)),
					draw.Src,
					nil,
				)
				mv.visibleArea.Update(mv.visibleAreaImage)
				mv.forceUpdate = false
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
						img := mv.bg.SubImage(nk.NkRect(16-mv.bgScroll, 16-mv.bgScroll, w, h))
						nk.NkDrawImage(cmdbuf, nk.NkRect(x, y, w, h), &img, col)
					}
				}
			}
			mv.bgScroll += 0.5
			if mv.bgScroll > 8 {
				mv.bgScroll -= 8
			}

			img := mv.visibleArea.SubImage(nk.NkRect(0, 0, rgn.W(), rgn.H()))
			nk.NkLayoutSpacePush(ctx, nk.NkRect(ofs.X(), ofs.Y(), rgn.W(), rgn.H()))
			nk.NkImage(ctx, img)
		}
		nk.NkLayoutSpaceEnd(ctx)
		mv.latestImageRect = imageRect
		mv.latestActiveRect = activeRect
		mv.latestWorkspaceSize = workspaceSize

	}
	nk.NkEnd(ctx)
}

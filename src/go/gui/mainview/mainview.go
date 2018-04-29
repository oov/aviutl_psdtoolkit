package mainview

import (
	"image"
	"math"

	"github.com/golang-ui/nuklear/nk"
	"github.com/pkg/errors"
	"golang.org/x/image/draw"

	"github.com/oov/aviutl_psdtoolkit/src/go/jobqueue"
	"github.com/oov/aviutl_psdtoolkit/src/go/nkhelper"
	"github.com/oov/psd/blend"
)

type MainView struct {
	renderedImage *image.RGBA
	resizedImage  *image.NRGBA

	visibleAreaImage *image.NRGBA
	visibleArea      *nkhelper.Texture

	latestImageRect     image.Rectangle
	latestActiveRect    image.Rectangle
	latestWorkspaceSize image.Point

	bg       *nkhelper.Texture
	bgScroll float32

	scrollX, scrollY nk.Uint
	forceUpdate      bool
	forceFitToWindow bool

	minZoom  float32
	maxZoom  float32
	stepZoom float32
	zoom     float64
	zooming  bool

	queue chan func()
}

var jq = jobqueue.New(1)

func New(bg image.Image) (*MainView, error) {
	mv := &MainView{
		queue: make(chan func()),
	}
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

func (mv *MainView) do(f func()) {
	done := make(chan struct{})
	mv.queue <- func() {
		f()
		done <- struct{}{}
	}
	<-done
}

func (mv *MainView) SetZoomRange(min, max, step float32) {
	mv.minZoom = min
	mv.maxZoom = max
	mv.stepZoom = step
}

func (mv *MainView) adjustZoom(imageRect image.Rectangle) {
	latestActiveRect := mv.latestActiveRect
	// If activeRect is empty, it can not be processed correctly.
	// In that case we will try again next time.
	if latestActiveRect.Empty() {
		go mv.do(func() {
			mv.adjustZoom(imageRect)
		})
		return
	}
	z := float64(latestActiveRect.Dx()) / float64(imageRect.Dx())
	if z*float64(imageRect.Dy()) > float64(latestActiveRect.Dy()) {
		z = float64(latestActiveRect.Dy()) / float64(imageRect.Dy())
	}
	mv.zoom = math.Log(z) / math.Ln2
	mv.forceFitToWindow = true
}

func (mv *MainView) SetRenderedImage(img *image.RGBA) {
	mv.renderedImage = img
	if mv.resizedImage == nil {
		mv.adjustZoom(img.Rect)
	}
	mv.updateViewImage(vrmFastAfterBeautiful)
}

func (mv *MainView) Clear() {
	mv.resizedImage = nil
	mv.visibleAreaImage = image.NewNRGBA(image.Rect(0, 0, 1, 1))
	mv.visibleArea.Update(mv.visibleAreaImage)
}

func (mv *MainView) Render(ctx *nk.Context) {
eat:
	for {
		select {
		case f := <-mv.queue:
			f()
		default:
			break eat
		}
	}

	if mv.resizedImage == nil {
		return
	}

	rgn := nk.NkWindowGetContentRegion(ctx)
	winHeight := rgn.H()
	const (
		bottomPaneHeight = 48
		padding          = 2
	)

	nk.NkLayoutRowDynamic(ctx, winHeight-bottomPaneHeight-padding, 1)
	if nk.NkGroupScrolledOffsetBegin(ctx, &mv.scrollX, &mv.scrollY, "CanvasPane", 0) != 0 {
		mv.renderCanvas(ctx)
		nk.NkGroupEnd(ctx)
	}
	nk.NkLayoutRowDynamic(ctx, float32(bottomPaneHeight-padding), 1)
	if nk.NkGroupBegin(ctx, "ScalerPane", nk.WindowNoScrollbar) != 0 {
		nk.NkLayoutRowDynamic(ctx, 0, 1)
		if z := float64(nk.NkSlideFloat(ctx, mv.minZoom, float32(mv.zoom), mv.maxZoom, mv.stepZoom)); z != mv.zoom {
			if !mv.zooming {
				mv.zooming = true
			}
			mv.zoom = z
			mv.updateViewImage(vrmFast)
		} else if (ctx.LastWidgetState()&nk.WidgetStateActive) != nk.WidgetStateActive && mv.zooming {
			mv.zooming = false
			mv.updateViewImage(vrmBeautiful)
		}
		nk.NkGroupEnd(ctx)
	}
}

func (mv *MainView) renderCanvas(ctx *nk.Context) {
	rgn := nk.NkWindowGetContentRegion(ctx)
	winWidth, winHeight := int(rgn.W()), int(rgn.H())

	resizedImage := mv.resizedImage
	forceUpdate := mv.forceUpdate
	forceFitToWindow := mv.forceFitToWindow
	mv.forceUpdate = false
	mv.forceFitToWindow = false

	zoom := mv.zoom

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
	imageRect.Max.X += int(float64(resizedImage.Rect.Dx()) * zr)
	imageRect.Max.Y += int(float64(resizedImage.Rect.Dy()) * zr)
	workspaceSize := image.Pt(
		winWidth*2+imageRect.Dx(),
		winHeight*2+imageRect.Dy(),
	)

	if !workspaceSize.Eq(mv.latestWorkspaceSize) || forceFitToWindow {
		latestWinWidth := (mv.latestWorkspaceSize.X - mv.latestImageRect.Dx()) / 2
		latestWinHeight := (mv.latestWorkspaceSize.Y - mv.latestImageRect.Dy()) / 2
		var x, y float64
		if forceFitToWindow {
			x = 0.5
			y = 0.5
		} else {
			x = (float64(mv.scrollX) - float64(latestWinWidth)*0.5) / float64(mv.latestImageRect.Dx())
			y = (float64(mv.scrollY) - float64(latestWinHeight)*0.5) / float64(mv.latestImageRect.Dy())
		}
		mv.scrollX = nk.Uint(math.Floor(0.5 + float64(imageRect.Dx())*x + float64(winWidth)*0.5))
		mv.scrollY = nk.Uint(math.Floor(0.5 + float64(imageRect.Dy())*y + float64(winHeight)*0.5))
	}

	nk.NkLayoutSpaceBegin(ctx, nk.Static, float32(workspaceSize.Y), 3)
	nk.NkLayoutSpacePush(ctx, nk.NkRect(0, 0, float32(workspaceSize.X), float32(workspaceSize.Y)))
	nk.NkLabel(ctx, "", 0)

	activeRect := image.Rect(int(mv.scrollX), int(mv.scrollY), int(mv.scrollX)+int(rgn.W()), int(mv.scrollY)+int(rgn.H()))

	if !activeRect.Eq(mv.latestActiveRect) || !workspaceSize.Eq(mv.latestWorkspaceSize) || forceUpdate {
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
			resizedImage,
			image.Rect(ix, iy, ix+int(float64(activeRect.Dx())*izr), iy+int(float64(activeRect.Dy())*izr)),
			draw.Src,
			nil,
		)
		mv.visibleArea.Update(mv.visibleAreaImage)
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
	if nk.NkWidget(&widgetRect, ctx) != 0 {
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
	nk.NkLayoutSpacePush(ctx, nk.NkRect(float32(mv.scrollX), float32(mv.scrollY), rgn.W(), rgn.H()))
	nk.NkImage(ctx, img)

	nk.NkLayoutSpaceEnd(ctx)
	mv.latestImageRect = imageRect
	mv.latestActiveRect = activeRect
	mv.latestWorkspaceSize = workspaceSize
}

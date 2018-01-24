package tabview

import (
	"github.com/golang-ui/nuklear/nk"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/editing"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
)

type TabView struct {
	ed *editing.Editing
}

func New(ed *editing.Editing) *TabView {
	return &TabView{
		ed: ed,
	}
}

func (v *TabView) drawTab(ctx *nk.Context, img nk.Image, selected bool) (clicked bool) {
	var rect nk.Rect
	state := nk.NkWidget(&rect, ctx)
	if state == 0 {
		return false
	}

	canvas := nk.NkWindowGetCanvas(ctx)
	var bg nk.Color

	if selected {
		bg.SetA(64)
	} else {
		if state != nk.WidgetRom {
			if nk.NkWidgetIsHovered(ctx) != 0 {
				bg.SetA(32)
				clicked = nk.NkInputIsMousePressed(ctx.Input(), nk.ButtonLeft) != 0
			}
		}
	}
	nk.NkFillRect(canvas, rect, 0, bg)

	var white nk.Color
	white.SetRGBA(255, 255, 255, 255)
	nk.NkDrawImage(canvas, nk.NkRect(rect.X(), rect.Y(), 48, 48), &img, white)
	return clicked
}

func (v *TabView) Render(ctx *nk.Context) int {
	rgn := nk.NkWindowGetContentRegion(ctx)
	winHeight := rgn.H()

	const PADDING = 2
	const tabHeight = 48
	r := v.ed.SelectedIndex

	nk.NkLayoutRowDynamic(ctx, winHeight-PADDING, 1)
	if nk.NkGroupBegin(ctx, "TabPane", 0) != 0 {
		imgs, err := v.ed.ThumbnailList()
		if err == nil {
			nk.NkLayoutSpaceBegin(ctx, nk.Static, tabHeight*float32(len(imgs)+1), int32(len(imgs)+1))

			for i, img := range imgs {
				nk.NkLayoutSpacePush(ctx, nk.NkRect(0, float32(i)*tabHeight, tabHeight, tabHeight))
				if v.drawTab(ctx, img, i == v.ed.SelectedIndex) {
					r = i
					ods.ODS("clicked %d", i)
				}
			}
			nk.NkLayoutSpaceEnd(ctx)
		}
		nk.NkGroupEnd(ctx)
	}

	return r
}

package layerview

import (
	"context"
	"image"
	"time"

	"github.com/golang-ui/nuklear/nk"
	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/nkhelper"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
	"github.com/oov/psd/composite"
)

const (
	symbolEyeClose      = "\u0025"
	symbolEyeOpen       = "\u0026"
	symbolEyeOpenLocked = "\u002d"
	symbolFolderClose   = "\u0021"
	symbolFolderOpen    = "\u0022"
	symbolClippingArrow = "\u0027"
	symbolFilter        = "\u0028"
	symbolFile          = "\u0029"
	symbolLeftArrow     = "\u002a"
	symbolRightArrow    = "\u002b"
	symbolClipboard     = "\u002c"
	symbolExport        = "\u002e"
)

type LayerView struct {
	MainFontHandle   *nk.UserFont
	SymbolFontHandle *nk.UserFont
	ThumbnailSize    int

	Thumbnail     *nkhelper.Texture
	ThumbnailChip map[int]*nk.Image

	LayerFavSelectedIndex int32

	CopyFaviewValue    func(sliderName, name, value string)
	ExportFaviewSlider func(sliderName string, names, values []string)
}

func (lv *LayerView) Init() {
	var err error
	lv.Thumbnail, err = nkhelper.NewTexture(image.NewNRGBA(image.Rect(0, 0, 1, 1)))
	if err != nil {
		panic(err)
	}
	lv.ThumbnailChip = map[int]*nk.Image{}
}

func (lv *LayerView) UpdateThumbnails(tree *composite.Tree, size int, doMain func(func())) {
	lv.ThumbnailChip = map[int]*nk.Image{}
	lv.ThumbnailSize = size
	go func() {
		s := time.Now().UnixNano()
		rgba, ptMap, err := tree.ThumbnailSheet(context.Background(), size)
		if err != nil {
			doMain(func() {
				lv.reportError(errors.Wrap(err, "layerview: failed to create thumbnail sheet"))
			})
			return
		}
		nrgba := rgbaToNRGBA(rgba)
		ods.ODS("thumbnail: %dms", (time.Now().UnixNano()-s)/1e6)
		doMain(func() {
			lv.Thumbnail.Update(nrgba)
			for i, rect := range ptMap {
				img := lv.Thumbnail.SubImage(nk.NkRect(
					float32(rect.Min.X),
					float32(rect.Min.Y),
					float32(rect.Dx()),
					float32(rect.Dy()),
				))
				lv.ThumbnailChip[i] = &img
			}
		})
	}()
}

func b2i(b bool) int32 {
	if b {
		return 1
	}
	return 0
}

func (lv *LayerView) Render(ctx *nk.Context, winRect nk.Rect, img *img.Image) bool {
	modified := false
	if nk.NkBegin(ctx, "Layer", winRect, 0) != 0 {
		if img != nil {
			if img.PFV != nil {
				if len(img.PFV.FaviewRoot.Children) > 0 {
					nk.NkLayoutRowDynamic(ctx, 28, 3)
					if nk.NkSelectLabel(ctx, "レイヤー", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(lv.LayerFavSelectedIndex == 0)) != b2i(lv.LayerFavSelectedIndex == 0) {
						lv.LayerFavSelectedIndex = 0
					}
					if nk.NkSelectLabel(ctx, "シンプルV", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(lv.LayerFavSelectedIndex == 1)) != b2i(lv.LayerFavSelectedIndex == 1) {
						lv.LayerFavSelectedIndex = 1
					}
					if nk.NkSelectLabel(ctx, "お気に入り", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(lv.LayerFavSelectedIndex == 2)) != b2i(lv.LayerFavSelectedIndex == 2) {
						lv.LayerFavSelectedIndex = 2
					}
				} else {
					nk.NkLayoutRowDynamic(ctx, 28, 2)
					if nk.NkSelectLabel(ctx, "レイヤー", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(lv.LayerFavSelectedIndex == 0)) != b2i(lv.LayerFavSelectedIndex == 0) {
						lv.LayerFavSelectedIndex = 0
					}
					if nk.NkSelectLabel(ctx, "お気に入り", nk.TextAlignCentered|nk.TextAlignMiddle, b2i(lv.LayerFavSelectedIndex == 2)) != b2i(lv.LayerFavSelectedIndex == 2) {
						lv.LayerFavSelectedIndex = 2
					}
				}
			} else {
				lv.LayerFavSelectedIndex = 0
			}

			switch lv.LayerFavSelectedIndex {
			case 0:
				for i := len(img.PSD.Root.Children) - 1; i >= 0; i-- {
					modified = lv.layoutLayer(ctx, img, 0, &img.PSD.Root.Children[i], true) || modified
				}
			case 1:
				if img.PFV != nil && len(img.PFV.FaviewRoot.Children) > 0 {
					nk.NkLayoutRowDynamic(ctx, 28, 1)
					img.PFV.FaviewRoot.SelectedIndex = int(nk.NkComboString(
						ctx,
						img.PFV.FaviewRoot.ItemNameList,
						int32(img.PFV.FaviewRoot.SelectedIndex),
						int32(len(img.PFV.FaviewRoot.Children)),
						28,
						nk.NkVec2(winRect.W(), winRect.H()),
					))
					children := img.PFV.FaviewRoot.Children[img.PFV.FaviewRoot.SelectedIndex].Children
					for i := range children {
						modified = lv.layoutFaview(ctx, img, 0, &children[i]) || modified
					}
				}
			case 2:
				if img.PFV != nil {
					modified = lv.layoutFavorites(ctx, img, 0, &img.PFV.Root) || modified
				}
			}
		}
	}
	nk.NkEnd(ctx)
	return modified
}

func drawTextMiddle(canvas *nk.CommandBuffer, rect nk.Rect, s string, font *nk.UserFont, col nk.Color) {
	ofs := (rect.H() - nkhelper.FontHeight(font)) * 0.5
	r := nk.NkRect(rect.X(), rect.Y()+ofs, rect.W(), rect.H())
	nk.NkDrawText(canvas, r, s, int32(len(s)), font, nk.Color{}, col)
}

func layerTreeItem(ctx *nk.Context, indent, thumbSize float32, sansFont, symbolsFont *nk.UserFont, thumb *nk.Image, visible, forceVisible bool, l *composite.Layer) (clicked bool, ctrl bool) {
	clicked = false
	ctrl = false
	const (
		visibleSize  = 24
		collapseSize = 24
		clippingSize = 16
		marginSize   = 8
	)
	bounds := nk.NkLayoutSpaceBounds(ctx)

	if l.Folder {
		nk.NkStylePushFont(ctx, symbolsFont)
		nk.NkStylePushStyleItem(ctx, nkhelper.GetStyleButtonNormalPtr(ctx), nk.NkStyleItemColor(nk.Color{}))
		nk.NkStylePushFloat(ctx, nkhelper.GetStyleButtonBorderPtr(ctx), 0)
		nk.NkStylePushVec2(ctx, nkhelper.GetStyleButtonPaddingPtr(ctx), nk.NkVec2(0, 0))

		nk.NkLayoutSpacePush(ctx, nk.NkRect(indent, 0, collapseSize, bounds.H()))
		symbol := symbolFolderClose
		if l.FolderOpen {
			symbol = symbolFolderOpen
		}
		if nk.NkButtonLabel(ctx, symbol) != 0 {
			l.FolderOpen = !l.FolderOpen
		}
		nk.NkStylePopVec2(ctx)
		nk.NkStylePopFloat(ctx)
		nk.NkStylePopStyleItem(ctx)
		nk.NkStylePopFont(ctx)
	}
	indent += collapseSize + 1

	w := float32(nkhelper.TextWidth(sansFont, l.Name) + marginSize*2 + visibleSize + thumbSize)
	if l.Clipping {
		w += visibleSize
	}
	nk.NkLayoutSpacePush(ctx, nk.NkRect(indent, 0, w, bounds.H()))

	var rect nk.Rect
	state := nk.NkWidget(&rect, ctx)
	if state == 0 {
		return false, false
	}

	canvas := nk.NkWindowGetCanvas(ctx)
	var bg nk.Color

	if state != nk.WidgetRom {
		if !forceVisible && nk.NkWidgetIsHovered(ctx) != 0 {
			bg.SetA(32)
			clicked = nk.NkInputIsMousePressed(ctx.Input(), nk.ButtonLeft) != 0
			ctrl = nk.NkInputIsKeyDown(ctx.Input(), nk.KeyCtrl) != 0
		}
	}
	nk.NkFillRect(canvas, rect, 4, bg)

	var visibleSymbol string
	if forceVisible {
		visibleSymbol = symbolEyeOpenLocked
	} else {
		if l.Visible {
			visibleSymbol = symbolEyeOpen
		} else {
			visibleSymbol = symbolEyeClose
		}
	}
	var fg nk.Color
	c := nkhelper.GetStyleTextColorPtr(ctx)
	fg.SetRGBA(c.R(), c.G(), c.B(), c.A())
	if !visible {
		fg.SetR(fg.R() / 2)
		fg.SetG(fg.G() / 2)
		fg.SetB(fg.B() / 2)
	}
	rect = nk.NkRect(rect.X()+marginSize, rect.Y(), rect.W()-marginSize, rect.H())
	drawTextMiddle(canvas, rect, visibleSymbol, symbolsFont, fg)
	rect = nk.NkRect(rect.X()+visibleSize, rect.Y(), rect.W()-visibleSize, rect.H())

	if thumb != nil {
		var white nk.Color
		white.SetRGBA(255, 255, 255, 255)
		nk.NkDrawImage(canvas, nk.NkRect(rect.X(), rect.Y()+rect.H()/2-thumbSize/2, thumbSize, thumbSize), thumb, white)
		rect = nk.NkRect(rect.X()+thumbSize, rect.Y(), rect.W()-thumbSize, rect.H())
	}

	if l.Clipping {
		drawTextMiddle(canvas, rect, symbolClippingArrow, symbolsFont, fg)
		rect = nk.NkRect(rect.X()+visibleSize, rect.Y(), rect.W()-visibleSize, rect.H())
	}
	drawTextMiddle(canvas, rect, l.Name, sansFont, fg)
	return clicked, ctrl
}

func (lv *LayerView) layoutLayer(ctx *nk.Context, img *img.Image, indent float32, l *composite.Layer, visible bool) bool {
	modified := false
	const (
		indentSize = 16
	)

	visible = visible && l.Visible
	if l.Clipping {
		visible = visible && l.ClippedBy.Visible
	}
	_, forceVisible := img.Layers.ForceVisible[l.SeqID]
	thumb, _ := lv.ThumbnailChip[l.SeqID]
	nk.NkLayoutSpaceBegin(ctx, nk.Static, 28, 3)
	if clicked, ctrl := layerTreeItem(ctx, indent, float32(lv.ThumbnailSize), lv.MainFontHandle, lv.SymbolFontHandle, thumb, visible, forceVisible, l); clicked {
		if ctrl {
			modified = img.Layers.SetVisibleExclusive(l.SeqID, !l.Visible, img.Flip) || modified
		} else {
			modified = img.Layers.SetVisible(l.SeqID, !l.Visible, img.Flip) || modified
		}
		for r := l.Parent; r.SeqID != composite.SeqIDRoot; r = r.Parent {
			modified = img.Layers.SetVisible(r.SeqID, true, img.Flip) || modified
		}
	}
	nk.NkLayoutSpaceEnd(ctx)

	if l.FolderOpen {
		for i := len(l.Children) - 1; i >= 0; i-- {
			modified = lv.layoutLayer(ctx, img, indent+indentSize, &l.Children[i], visible) || modified
		}
	}
	return modified
}

func favoriteItem(ctx *nk.Context, indent float32, sansFont, symbolsFont *nk.UserFont, n *img.Node) bool {
	modified := false
	const (
		visibleSize  = 24
		collapseSize = 24
		clippingSize = 16
		marginSize   = 8
	)
	bounds := nk.NkLayoutSpaceBounds(ctx)

	if n.Folder() || n.Filter() {
		nk.NkStylePushFont(ctx, symbolsFont)
		nk.NkStylePushStyleItem(ctx, nkhelper.GetStyleButtonNormalPtr(ctx), nk.NkStyleItemColor(nk.Color{}))
		nk.NkStylePushFloat(ctx, nkhelper.GetStyleButtonBorderPtr(ctx), 0)
		nk.NkStylePushVec2(ctx, nkhelper.GetStyleButtonPaddingPtr(ctx), nk.NkVec2(0, 0))

		nk.NkLayoutSpacePush(ctx, nk.NkRect(indent, 0, collapseSize, bounds.H()))
		symbol := symbolFolderClose
		if n.Open {
			symbol = symbolFolderOpen
		}
		if nk.NkButtonLabel(ctx, symbol) != 0 {
			n.Open = !n.Open
		}
		nk.NkStylePopVec2(ctx)
		nk.NkStylePopFloat(ctx)
		nk.NkStylePopStyleItem(ctx)
		nk.NkStylePopFont(ctx)
	}
	indent += collapseSize + 1

	w := float32(nkhelper.TextWidth(sansFont, n.Name) + marginSize*2)
	if n.Filter() || n.Item() {
		w += visibleSize
	}
	nk.NkLayoutSpacePush(ctx, nk.NkRect(indent, 0, w, bounds.H()))

	var rect nk.Rect
	state := nk.NkWidget(&rect, ctx)
	if state == 0 {
		return false
	}

	canvas := nk.NkWindowGetCanvas(ctx)
	var bg nk.Color

	if state != nk.WidgetRom {
		if n.Item() && nk.NkWidgetIsHovered(ctx) != 0 {
			bg.SetA(32)
			modified = nk.NkInputIsMousePressed(ctx.Input(), nk.ButtonLeft) != 0
		}
	}
	nk.NkFillRect(canvas, rect, 4, bg)

	var fg nk.Color
	c := nkhelper.GetStyleTextColorPtr(ctx)
	fg.SetRGBA(c.R(), c.G(), c.B(), c.A())
	rect = nk.NkRect(rect.X()+marginSize, rect.Y(), rect.W()-marginSize, rect.H())
	if n.Filter() {
		drawTextMiddle(canvas, rect, symbolFilter, symbolsFont, fg)
		rect = nk.NkRect(rect.X()+visibleSize, rect.Y(), rect.W()-visibleSize, rect.H())
	} else if n.Item() {
		drawTextMiddle(canvas, rect, symbolFile, symbolsFont, fg)
		rect = nk.NkRect(rect.X()+visibleSize, rect.Y(), rect.W()-visibleSize, rect.H())
	}
	drawTextMiddle(canvas, rect, n.Name, sansFont, fg)
	return modified
}

func (lv *LayerView) layoutFavorites(ctx *nk.Context, img *img.Image, indent float32, n *img.Node) bool {
	modified := false
	const (
		indentSize = 16
	)
	nk.NkLayoutSpaceBegin(ctx, nk.Static, 28, 4)
	if favoriteItem(ctx, indent, lv.MainFontHandle, lv.SymbolFontHandle, n) {
		modified = lv.selectFavoriteNode(img, n) || modified
	}
	nk.NkLayoutSpaceEnd(ctx)

	if (n.Folder() || n.Filter()) && n.Open {
		for i := range n.Children {
			modified = lv.layoutFavorites(ctx, img, indent+indentSize, &n.Children[i]) || modified
		}
	}
	return modified
}

func (lv *LayerView) layoutFaview(ctx *nk.Context, img *img.Image, indent float32, n *img.FaviewNode) bool {
	modified := false
	const (
		buttonSize = 32
		marginSize = 8
		indentSize = 16
	)
	nk.NkLayoutSpaceBegin(ctx, nk.Static, 28, 1)
	bounds := nk.NkLayoutSpaceBounds(ctx)

	left := indent
	nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, bounds.W()-left, bounds.H()))
	nk.NkLabel(ctx, n.NameNode.Name, nk.TextLeft)

	nk.NkLayoutSpaceEnd(ctx)

	if len(n.Items) > 0 {
		nk.NkLayoutSpaceBegin(ctx, nk.Static, 28, 5)
		bounds := nk.NkLayoutSpaceBounds(ctx)

		left = indent + marginSize

		nk.NkStylePushFont(ctx, lv.SymbolFontHandle)
		nk.NkStylePushStyleItem(ctx, nkhelper.GetStyleButtonNormalPtr(ctx), nk.NkStyleItemColor(nk.Color{}))
		nk.NkStylePushFloat(ctx, nkhelper.GetStyleButtonBorderPtr(ctx), 0)
		nk.NkStylePushVec2(ctx, nkhelper.GetStyleButtonPaddingPtr(ctx), nk.NkVec2(0, 0))

		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, buttonSize, bounds.H()))
		if nk.NkButtonLabel(ctx, symbolLeftArrow) != 0 {
			modified = lv.selectFaviewNode(img, n, (len(n.Items)+n.SelectedIndex-1)%len(n.Items)) || modified
		}
		left += buttonSize

		nk.NkStylePopVec2(ctx)
		nk.NkStylePopFloat(ctx)
		nk.NkStylePopStyleItem(ctx)
		nk.NkStylePopFont(ctx)

		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, bounds.W()-left-buttonSize*3-marginSize, bounds.H()))
		if idx := int(nk.NkComboString(ctx, n.ItemNameList, int32(n.SelectedIndex), int32(len(n.Items)), 28, nk.NkVec2(bounds.W(), 400))); idx != n.SelectedIndex {
			modified = lv.selectFaviewNode(img, n, idx) || modified
		}
		left += bounds.W() - left - buttonSize*3 - marginSize

		nk.NkStylePushFont(ctx, lv.SymbolFontHandle)
		nk.NkStylePushStyleItem(ctx, nkhelper.GetStyleButtonNormalPtr(ctx), nk.NkStyleItemColor(nk.Color{}))
		nk.NkStylePushFloat(ctx, nkhelper.GetStyleButtonBorderPtr(ctx), 0)
		nk.NkStylePushVec2(ctx, nkhelper.GetStyleButtonPaddingPtr(ctx), nk.NkVec2(0, 0))

		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, buttonSize, bounds.H()))
		if nk.NkButtonLabel(ctx, symbolRightArrow) != 0 {
			modified = lv.selectFaviewNode(img, n, (n.SelectedIndex+1)%len(n.Items)) || modified
		}
		left += buttonSize

		nk.NkStylePopVec2(ctx)
		nk.NkStylePopFloat(ctx)
		nk.NkStylePopStyleItem(ctx)

		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, buttonSize, bounds.H()))
		if nk.NkButtonLabel(ctx, symbolClipboard) != 0 {
			lv.copyFaviewNode(n)
		}
		left += buttonSize

		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, buttonSize, bounds.H()))
		if nk.NkButtonLabel(ctx, symbolExport) != 0 {
			lv.exportFaviewNode(n)
		}
		left += buttonSize

		nk.NkStylePopFont(ctx)

		left = indent + marginSize
		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, bounds.W()-left, bounds.H()))
		if idx := int(nk.NkSlideInt(ctx, 0, int32(n.SelectedIndex), int32(len(n.Items)-1), 1)); idx != n.SelectedIndex {
			modified = lv.selectFaviewNode(img, n, idx) || modified
		}
		nk.NkLayoutSpaceEnd(ctx)
	}

	if len(n.Children) > 0 {
		for i := range n.Children {
			modified = lv.layoutFaview(ctx, img, indent+indentSize, &n.Children[i]) || modified
		}
	}
	return modified
}

func rgbaToNRGBA(rgba *image.RGBA) *image.NRGBA {
	nrgba := &image.NRGBA{
		Stride: rgba.Stride,
		Rect:   rgba.Rect,
		Pix:    rgba.Pix,
	}
	w, lines, pix, stride := rgba.Rect.Dx()<<2, rgba.Rect.Dy(), rgba.Pix, rgba.Stride
	for y := 0; y < lines; y++ {
		p := pix[y*stride : y*stride+stride]
		for x := 0; x < w; x += 4 {
			a := uint32(p[x+3])
			if a > 0 {
				p[x+0] = uint8(uint32(p[x+0]) * 0xff / a)
				p[x+1] = uint8(uint32(p[x+1]) * 0xff / a)
				p[x+2] = uint8(uint32(p[x+2]) * 0xff / a)
			}
		}
	}
	return nrgba
}

func (lv *LayerView) selectFavoriteNode(img *img.Image, n *img.Node) bool {
	state, err := n.State()
	if err != nil {
		lv.reportError(errors.Wrap(err, "layerview: cannot serialize"))
		return false
	}
	m, err := img.Deserialize(state)
	if err != nil {
		lv.reportError(errors.Wrap(err, "layerview: cannot deserialize"))
		return false
	}
	return m
}

func (lv *LayerView) selectFaviewNode(img *img.Image, n *img.FaviewNode, newSelectedIndex int) bool {
	state, err := n.Items[newSelectedIndex].State()
	if err != nil {
		lv.reportError(errors.Wrap(err, "layerview: cannot serialize"))
		return false
	}
	m, err := img.Deserialize(state)
	if err != nil {
		lv.reportError(errors.Wrap(err, "layerview: cannot deserialize"))
		return false
	}
	n.SelectedIndex = newSelectedIndex
	n.LastModified = time.Now()
	return m
}

func (lv *LayerView) copyFaviewNode(n *img.FaviewNode) {
	state, err := n.SelectedState()
	if err != nil {
		lv.reportError(errors.Wrap(err, "layerview: cannot copy"))
		return
	}
	lv.CopyFaviewValue(n.FullName(), n.SelectedName(), state)
}

func (lv *LayerView) exportFaviewNode(n *img.FaviewNode) {
	state, err := n.AllState()
	if err != nil {
		lv.reportError(errors.Wrap(err, "layerview: cannot export"))
		return
	}
	lv.ExportFaviewSlider(n.FullName(), n.AllName(), state)
}

func (lv *LayerView) reportError(err error) {
	//TODO: improve error handling
	ods.ODS("error: %v", err)
}

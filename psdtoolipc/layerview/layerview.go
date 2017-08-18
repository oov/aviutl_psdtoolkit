package layerview

import (
	"context"
	"image"
	"time"
	"unsafe"

	"github.com/go-gl/gl/v3.2-core/gl"
	"github.com/golang-ui/nuklear/nk"

	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/img"
	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/nkhelper"
	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/ods"
	"github.com/oov/psd/layertree"
)

const (
	symbolEyeClose      = "\u0025"
	symbolEyeOpen       = "\u0026"
	symbolFolderClose   = "\u0021"
	symbolFolderOpen    = "\u0022"
	symbolClippingArrow = "\u0027"
	symbolFilter        = "\u0028"
	symbolFile          = "\u0029"
	symbolLeftArrow     = "\u002a"
	symbolRightArrow    = "\u002b"
	symbolClipboard     = "\u002c"
)

type LayerView struct {
	SymbolFontHandle *nk.UserFont

	Thumbnail     nk.Image
	ThumbnailTex  uint32
	ThumbnailChip map[int]nk.Image

	LayerFavSelectedIndex int32

	CopyToClipboard func(s string)
}

func (lv *LayerView) Init() {
	lv.ThumbnailTex, lv.Thumbnail = createTexture(image.NewNRGBA(image.Rect(0, 0, 1, 1)))
	lv.ThumbnailChip = map[int]nk.Image{}
}

func (lv *LayerView) UpdateThumbnails(root *layertree.Root, size int, doMain func(func())) {
	lv.ThumbnailChip = map[int]nk.Image{}
	go func() {
		s := time.Now().UnixNano()
		rgba, ptMap, err := root.ThumbnailSheet(context.Background(), size)
		if err != nil {
			ods.ODS("thumbnail: %v", err)
			return
		}
		nrgba := rgbaToNRGBA(rgba)
		ods.ODS("thumbnail: %dms", (time.Now().UnixNano()-s)/1e6)
		doMain(func() {
			lv.ThumbnailTex, lv.Thumbnail = updateTexture(lv.ThumbnailTex, nrgba)
			for i, rect := range ptMap {
				lv.ThumbnailChip[i] = nk.NkSubimageId(
					int32(lv.ThumbnailTex),
					uint16(nrgba.Rect.Dx()),
					uint16(nrgba.Rect.Dy()),
					nk.NkRect(
						float32(rect.Min.X),
						float32(rect.Min.Y),
						float32(rect.Dx()),
						float32(rect.Dy()),
					),
				)
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
				for i := len(img.PSD.Children) - 1; i >= 0; i-- {
					modified = lv.layoutLayer(ctx, img, 0, &img.PSD.Children[i], true) || modified
				}
			case 1:
				if img.PFV != nil && len(img.PFV.FaviewRoot.Children) > 0 {
					nk.NkLayoutRowDynamic(ctx, 28, 1)
					img.PFV.FaviewRoot.SelectedIndex = nk.NkComboString(ctx, img.PFV.FaviewRoot.ItemNameList, img.PFV.FaviewRoot.SelectedIndex, int32(len(img.PFV.FaviewRoot.Children)), 28, nk.NkVec2(winRect.W(), winRect.H()))
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

func (lv *LayerView) layoutLayer(ctx *nk.Context, img *img.Image, indent float32, l *layertree.Layer, visible bool) bool {
	modified := false
	const (
		visibleSize  = 24
		collapseSize = 24
		clippingSize = 16
		marginSize   = 8
		indentSize   = 16
	)
	nk.NkLayoutSpaceBegin(ctx, nk.Static, 28, 4)
	bounds := nk.NkLayoutSpaceBounds(ctx)

	visible = visible && l.Visible
	if l.Clipping {
		visible = visible && l.ClippedBy.Visible
	}
	if !visible {
		var nc nk.Color
		c := nkhelper.GetStyleTextColorPtr(ctx)
		r, g, b, a := c.RGBA()
		nc.SetRGBA(r, g, b, a/4)
		nk.NkStylePushColor(ctx, c, nc)

		c = nkhelper.GetStyleButtonTextNormalColorPtr(ctx)
		r, g, b, a = c.RGBA()
		nc.SetRGBA(r, g, b, a/4)
		nk.NkStylePushColor(ctx, c, nc)
	}

	nk.NkStylePushFont(ctx, lv.SymbolFontHandle)
	nk.NkStylePushStyleItem(ctx, nkhelper.GetStyleButtonNormalPtr(ctx), nk.NkStyleItemColor(nk.Color{}))
	nk.NkStylePushFloat(ctx, nkhelper.GetStyleButtonBorderPtr(ctx), 0)
	nk.NkStylePushVec2(ctx, nkhelper.GetStyleButtonPaddingPtr(ctx), nk.NkVec2(0, 0))

	left := float32(0)
	if _, ok := img.Layers.ForceVisible[l.SeqID]; !ok {
		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, visibleSize, bounds.H()))
		symbol := symbolEyeClose
		if l.Visible {
			symbol = symbolEyeOpen
		}
		if nk.NkButtonLabel(ctx, symbol) != 0 {
			modified = img.Layers.SetVisible(l.SeqID, !l.Visible, img.Flip) || modified
		}
	}
	left += visibleSize + marginSize + indent

	if l.Folder {
		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, collapseSize, bounds.H()))
		symbol := symbolFolderClose
		if l.FolderOpen {
			symbol = symbolFolderOpen
		}
		if nk.NkButtonLabel(ctx, symbol) != 0 {
			l.FolderOpen = !l.FolderOpen
		}
		left += collapseSize + marginSize
	} else {
		if l.Clipping {
			left += marginSize / 2
			nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, clippingSize, bounds.H()))
			nk.NkLabel(ctx, symbolClippingArrow, nk.TextLeft)
			left += clippingSize
		}

		if img, ok := lv.ThumbnailChip[l.SeqID]; ok {
			nk.NkLayoutSpacePush(ctx, nk.NkRect(left, (bounds.H()-collapseSize)/2, collapseSize, collapseSize))
			nk.NkImage(ctx, img)
		}
		left += collapseSize + marginSize
	}

	nk.NkStylePopVec2(ctx)
	nk.NkStylePopFloat(ctx)
	nk.NkStylePopStyleItem(ctx)
	nk.NkStylePopFont(ctx)

	nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, bounds.W()-left, bounds.H()))
	nk.NkLabel(ctx, l.Name, nk.TextLeft)

	if !visible {
		nk.NkStylePopColor(ctx)
		nk.NkStylePopColor(ctx)
	}

	nk.NkLayoutSpaceEnd(ctx)

	if l.FolderOpen {
		for i := len(l.Children) - 1; i >= 0; i-- {
			modified = lv.layoutLayer(ctx, img, indent+indentSize, &l.Children[i], visible) || modified
		}
	}
	return modified
}

func (lv *LayerView) layoutFavorites(ctx *nk.Context, img *img.Image, indent float32, n *img.Node) bool {
	modified := false
	const (
		visibleSize  = 24
		collapseSize = 24
		clippingSize = 16
		marginSize   = 8
		indentSize   = 16
	)
	nk.NkLayoutSpaceBegin(ctx, nk.Static, 28, 4)
	bounds := nk.NkLayoutSpaceBounds(ctx)

	nk.NkStylePushFont(ctx, lv.SymbolFontHandle)
	nk.NkStylePushStyleItem(ctx, nkhelper.GetStyleButtonNormalPtr(ctx), nk.NkStyleItemColor(nk.Color{}))
	nk.NkStylePushFloat(ctx, nkhelper.GetStyleButtonBorderPtr(ctx), 0)
	nk.NkStylePushVec2(ctx, nkhelper.GetStyleButtonPaddingPtr(ctx), nk.NkVec2(0, 0))

	left := float32(indent)
	if n.Folder() || n.Filter() {
		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, collapseSize, bounds.H()))
		symbol := symbolFolderClose
		if n.Open {
			symbol = symbolFolderOpen
		}
		if nk.NkButtonLabel(ctx, symbol) != 0 {
			n.Open = !n.Open
		}
		left += collapseSize + marginSize
	}
	if n.Filter() {
		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, collapseSize, bounds.H()))
		nk.NkLabel(ctx, symbolFilter, nk.TextLeft)
		left += collapseSize
	} else if n.Item() {
		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, collapseSize, bounds.H()))
		nk.NkLabel(ctx, symbolFile, nk.TextLeft)
		left += collapseSize
	}

	nk.NkStylePopVec2(ctx)
	nk.NkStylePopFloat(ctx)
	nk.NkStylePopStyleItem(ctx)
	nk.NkStylePopFont(ctx)

	nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, bounds.W()-left, bounds.H()))
	nk.NkLabel(ctx, n.Name, nk.TextLeft)

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
		nk.NkLayoutSpaceBegin(ctx, nk.Static, 28, 4)
		bounds := nk.NkLayoutSpaceBounds(ctx)

		left = indent + marginSize

		nk.NkStylePushFont(ctx, lv.SymbolFontHandle)
		nk.NkStylePushStyleItem(ctx, nkhelper.GetStyleButtonNormalPtr(ctx), nk.NkStyleItemColor(nk.Color{}))
		nk.NkStylePushFloat(ctx, nkhelper.GetStyleButtonBorderPtr(ctx), 0)
		nk.NkStylePushVec2(ctx, nkhelper.GetStyleButtonPaddingPtr(ctx), nk.NkVec2(0, 0))

		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, buttonSize, bounds.H()))
		if nk.NkButtonLabel(ctx, symbolLeftArrow) != 0 {
			n.SelectedIndex = int32((len(n.Items) + int(n.SelectedIndex) - 1) % len(n.Items))
			n.LastModified = time.Now()
			m, flip, err := img.Layers.DeserializeVisibility(n.State(), img.Flip)
			if err != nil {
				ods.ODS("cannot apply serialized data: %v", err)
			} else {
				modified = m || modified
				img.Flip = flip
			}
		}
		left += buttonSize

		nk.NkStylePopVec2(ctx)
		nk.NkStylePopFloat(ctx)
		nk.NkStylePopStyleItem(ctx)
		nk.NkStylePopFont(ctx)

		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, bounds.W()-left-buttonSize*2-marginSize, bounds.H()))
		if idx := nk.NkComboString(ctx, n.ItemNameList, n.SelectedIndex, int32(len(n.Items)), 28, nk.NkVec2(bounds.W(), 400)); idx != n.SelectedIndex {
			n.SelectedIndex = idx
			n.LastModified = time.Now()
			m, flip, err := img.Layers.DeserializeVisibility(n.State(), img.Flip)
			if err != nil {
				ods.ODS("cannot apply serialized data: %v", err)
			} else {
				modified = m || modified
				img.Flip = flip
			}
		}
		left += bounds.W() - left - buttonSize*2 - marginSize

		nk.NkStylePushFont(ctx, lv.SymbolFontHandle)
		nk.NkStylePushStyleItem(ctx, nkhelper.GetStyleButtonNormalPtr(ctx), nk.NkStyleItemColor(nk.Color{}))
		nk.NkStylePushFloat(ctx, nkhelper.GetStyleButtonBorderPtr(ctx), 0)
		nk.NkStylePushVec2(ctx, nkhelper.GetStyleButtonPaddingPtr(ctx), nk.NkVec2(0, 0))

		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, buttonSize, bounds.H()))
		if nk.NkButtonLabel(ctx, symbolRightArrow) != 0 {
			n.SelectedIndex = int32((int(n.SelectedIndex) + 1) % len(n.Items))
			n.LastModified = time.Now()
			m, flip, err := img.Layers.DeserializeVisibility(n.State(), img.Flip)
			if err != nil {
				ods.ODS("cannot apply serialized data: %v", err)
			} else {
				modified = m || modified
				img.Flip = flip
			}
		}
		left += buttonSize

		nk.NkStylePopVec2(ctx)
		nk.NkStylePopFloat(ctx)
		nk.NkStylePopStyleItem(ctx)

		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, buttonSize, bounds.H()))
		if nk.NkButtonLabel(ctx, symbolClipboard) != 0 {
			lv.CopyToClipboard(n.State())
		}
		left += buttonSize

		nk.NkStylePopFont(ctx)

		left = indent + marginSize
		nk.NkLayoutSpacePush(ctx, nk.NkRect(left, 0, bounds.W()-left, bounds.H()))
		if idx := nk.NkSlideInt(ctx, 0, n.SelectedIndex, int32(len(n.Items)-1), 1); idx != n.SelectedIndex {
			n.SelectedIndex = idx
			n.LastModified = time.Now()
			m, flip, err := img.Layers.DeserializeVisibility(n.State(), img.Flip)
			if err != nil {
				ods.ODS("cannot apply serialized data: %v", err)
			} else {
				modified = m || modified
				img.Flip = flip
			}
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

func createTexture(nrgba *image.NRGBA) (uint32, nk.Image) {
	var tex uint32
	gl.GenTextures(1, &tex)
	gl.BindTexture(gl.TEXTURE_2D, tex)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
	gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, int32(nrgba.Rect.Dx()), int32(nrgba.Rect.Dy()),
		0, gl.RGBA, gl.UNSIGNED_BYTE, unsafe.Pointer(&nrgba.Pix[0]))
	return tex, nk.NkImageId(int32(tex))
}

func updateTexture(tex uint32, nrgba *image.NRGBA) (uint32, nk.Image) {
	gl.DeleteTextures(1, &tex)
	gl.GenTextures(1, &tex)
	gl.BindTexture(gl.TEXTURE_2D, tex)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
	gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
	gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, int32(nrgba.Rect.Dx()), int32(nrgba.Rect.Dy()),
		0, gl.RGBA, gl.UNSIGNED_BYTE, unsafe.Pointer(&nrgba.Pix[0]))
	return tex, nk.NkImageId(int32(tex))
}

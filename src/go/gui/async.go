package gui

import (
	"context"
	"image"
	"time"

	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
)

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

func updateRenderedImage(g *GUI, img *img.Image) {
	if g.cancelRender != nil {
		g.cancelRender()
	}

	ctx, cancel := context.WithCancel(context.Background())
	g.cancelRender = cancel
	go func() {
		s := time.Now().UnixNano()
		rgba, err := img.Render(ctx)
		if err != nil {
			ods.ODS("rendering: aborted: %v", err)
			return
		}
		ods.ODS("rendering: %dms", (time.Now().UnixNano()-s)/1e6)
		g.do(func() {
			g.mainView.SetRenderedImage(rgba)
			cancel()
		})
	}()
}

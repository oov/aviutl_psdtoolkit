package mainview

import (
	"context"
	"image"
	"math"
	"time"

	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
	"github.com/oov/downscale"
)

type viewResizeMode int

const (
	vrmFast viewResizeMode = iota
	vrmBeautiful
	vrmFastAfterBeautiful
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

func (mv *MainView) updateViewImage(mode viewResizeMode) {
	jq.CancelAll()
	jq.Enqueue(func(ctx context.Context) error {
		var z float64
		if mv.zoom < 0 {
			z = mv.zoom
		}
		resizedImage := <-resizeImage(ctx, mv.renderedImage, math.Pow(2, z), mode == vrmFast || mode == vrmFastAfterBeautiful)
		if resizedImage == nil {
			return nil
		}
		mv.do(func() {
			mv.resizedImage = resizedImage
			mv.forceUpdate = true
		})
		if mode != vrmFastAfterBeautiful {
			return nil
		}
		resizedImage = <-resizeImage(ctx, mv.renderedImage, math.Pow(2, z), false)
		if resizedImage == nil {
			return nil
		}
		mv.do(func() {
			mv.resizedImage = resizedImage
			mv.forceUpdate = true
		})
		return nil
	})
}

func resizeImage(ctx context.Context, img *image.RGBA, scale float64, fast bool) <-chan *image.NRGBA {
	notify := make(chan *image.NRGBA)
	go func() {
		s := time.Now().UnixNano()
		r := image.Rect(
			img.Rect.Min.X,
			img.Rect.Min.Y,
			img.Rect.Min.X+int(float64(img.Rect.Dx())*scale),
			img.Rect.Min.Y+int(float64(img.Rect.Dy())*scale),
		)
		if r.Dx() == 0 {
			r.Max.X++
		}
		if r.Dy() == 0 {
			r.Max.Y++
		}
		rgba := image.NewRGBA(r)
		var err error
		if fast {
			err = downscale.RGBAFast(ctx, rgba, img)
		} else {
			err = downscale.RGBAGamma(ctx, rgba, img, 2.2)
		}
		if err != nil {
			ods.ODS("resize: aborted")
			notify <- nil
			return
		}
		nrgba := rgbaToNRGBA(rgba)
		ods.ODS("resize: %dms", (time.Now().UnixNano()-s)/1e6)
		notify <- nrgba
	}()
	return notify
}

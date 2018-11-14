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

func resizeImage(ctx context.Context, img *image.NRGBA, scale float64, fast bool) <-chan *image.NRGBA {
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
		nrgba := image.NewNRGBA(r)
		var err error
		if fast {
			err = downscale.NRGBAFast(ctx, nrgba, img)
		} else {
			err = downscale.NRGBAGamma(ctx, nrgba, img, 2.2)
		}
		if err != nil {
			ods.ODS("resize: aborted")
			notify <- nil
			return
		}
		ods.ODS("resize: %dms", (time.Now().UnixNano()-s)/1e6)
		notify <- nrgba
	}()
	return notify
}

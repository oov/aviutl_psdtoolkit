package main

import (
	"context"
	"image"
	"math"
	"time"

	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/img"
	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/ods"
	"github.com/oov/downscale"
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

func updateRenderedImage(g *gui, img *img.Image) {
	if g.cancelRender != nil {
		g.cancelRender()
	}
	cimg := *img
	cimg.PSD = img.PSD.Clone()
	cimg.PSD.Renderer = img.PSD.Renderer
	ctx, cancel := context.WithCancel(context.Background())
	g.cancelRender = cancel
	go func() {
		s := time.Now().UnixNano()
		rgba, err := cimg.Render(ctx)
		if err != nil {
			ods.ODS("rendering: aborted")
			return
		}
		nrgba := rgbaToNRGBA(rgba)
		ods.ODS("rendering: %dms", (time.Now().UnixNano()-s)/1e6)
		do(func() {
			g.renderedImage = nrgba
			if g.cancelRender != nil {
				g.cancelRender()
				g.cancelRender = nil
			}
			updateViewImage(g, nrgba, true)
			updateViewImage(g, nrgba, false)
		})
	}()
}

func updateViewImage(g *gui, img *image.NRGBA, fast bool) {
	if g.viewResizeRunning == vrmBeautiful && fast {
		g.cancelViewResize()
		g.cancelViewResize = nil
		g.viewResizeRunning = vrmNone
		g.viewResizeQueued = vrmNone
	}
	var notify <-chan *image.NRGBA
	if g.viewResizeRunning == vrmNone {
		ctx, cancel := context.WithCancel(context.Background())
		g.cancelViewResize = cancel
		var z float64
		if g.zoom < 0 {
			z = g.zoom
		}
		notify = resizeImage(ctx, g.renderedImage, math.Pow(2, z), fast)
		if fast {
			g.viewResizeRunning = vrmFast
		} else {
			g.viewResizeRunning = vrmBeautiful
		}
	} else {
		if fast {
			g.viewResizeQueued = vrmFast
		} else {
			g.viewResizeQueued = vrmBeautiful
		}
	}
	go func() {
		if notify == nil {
			return
		}
		resizedImage := <-notify
		if resizedImage == nil {
			return
		}
		do(func() {
			g.MainView.ResizedImage = resizedImage
			if g.cancelViewResize != nil {
				g.cancelViewResize()
				g.cancelViewResize = nil
			}
			g.MainView.ForceUpdate = true

			q := g.viewResizeQueued
			g.viewResizeRunning = vrmNone
			g.viewResizeQueued = vrmNone
			if q != vrmNone {
				updateViewImage(g, img, q == vrmFast)
			}
		})
	}()
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
		out := image.NewNRGBA(r)
		var err error
		if fast {
			err = downscale.NRGBAFast(ctx, out, img)
		} else {
			err = downscale.NRGBAGamma(ctx, out, img, 2.2)
		}
		if err != nil {
			ods.ODS("resize: aborted")
			notify <- nil
			return
		}
		ods.ODS("resize: %dms", (time.Now().UnixNano()-s)/1e6)
		notify <- out
	}()
	return notify
}

package img

import (
	"context"
	"image"
	"time"

	"github.com/disintegration/gift"
	"github.com/oov/downscale"
	"github.com/oov/psd/layertree"
	"github.com/pkg/errors"
)

type Image struct {
	FilePath   *string
	FileHash   uint32
	LastAccess time.Time

	PSD    *layertree.Root
	image  *image.RGBA
	Layers *LayerManager

	InitialVisibility *string

	Modified bool

	Scale   float32
	OffsetX int
	OffsetY int
}

func (img *Image) ScaledCanvasRect() image.Rectangle {
	r := img.PSD.CanvasRect
	r.Max.X = r.Min.X + int(float32(r.Dx())*img.Scale+0.5)
	r.Max.Y = r.Min.Y + int(float32(r.Dy())*img.Scale+0.5)
	if r.Dx() < 1 {
		r.Max.X = r.Min.X + 1
	}
	if r.Dy() < 1 {
		r.Max.Y = r.Min.Y + 1
	}
	return r
}

func (img *Image) Render(ctx context.Context) (*image.RGBA, error) {
	var err error
	if img.image == nil {
		img.image = image.NewRGBA(img.PSD.CanvasRect)
		err = img.PSD.Renderer.Render(ctx, img.image)
	} else {
		err = img.PSD.Renderer.RenderDiff(ctx, img.image)
	}
	if err != nil {
		return nil, errors.Wrap(err, "img: render failed")
	}
	img.Modified = false
	rgba := img.image
	if img.Scale < 1 {
		tmp := image.NewRGBA(img.ScaledCanvasRect())
		if err = downscale.RGBAGamma(ctx, tmp, rgba, 2.2); err != nil {
			return nil, errors.Wrap(err, "img: downscale failed")
		}
		rgba = tmp
	}
	f := img.Layers.Flip()
	if f != FlipNone {
		tmp := image.NewRGBA(rgba.Rect)
		g := gift.New()
		if f == FlipX || f == FlipXY {
			g.Add(gift.FlipHorizontal())
		}
		if f == FlipY || f == FlipXY {
			g.Add(gift.FlipVertical())
		}
		g.Draw(tmp, rgba)
		rgba = tmp
	}
	return rgba, nil
}

type ImageState struct {
	FilePath string
	FileHash uint32

	Scale   float32
	OffsetX int
	OffsetY int

	Layer string
}

func (img *Image) Serialize() *ImageState {
	return &ImageState{
		FilePath: *img.FilePath,
		FileHash: img.FileHash,

		Scale:   img.Scale,
		OffsetX: img.OffsetX,
		OffsetY: img.OffsetY,

		Layer: img.Layers.SerializeVisibility(),
	}
}

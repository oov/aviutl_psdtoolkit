// +build gdip

package nkhelper

import (
	"image"

	"github.com/golang-ui/nuklear/nk"
)

type Texture struct {
	img *nk.GdipImage
}

func (t *Texture) Close() error {
	return t.img.Close()
}

func (t *Texture) Image() nk.Image {
	return nk.NkImageHandle(t.img.Handle())
}

func (t *Texture) SubImage(rect nk.Rect) nk.Image {
	return nk.NkSubimageHandle(t.img.Handle(), uint16(t.img.Width), uint16(t.img.Height), rect)
}

func (t *Texture) Update(img image.Image) error {
	gi, err := nk.NkCreateImage(img)
	if err != nil {
		return err
	}
	old := t.img
	t.img = gi
	return old.Close()
}

func NewTexture(img image.Image) (*Texture, error) {
	gi, err := nk.NkCreateImage(img)
	if err != nil {
		return nil, err
	}
	return &Texture{
		img: gi,
	}, nil
}

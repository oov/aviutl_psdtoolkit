// +build !gdip

package nkhelper

import (
	"errors"
	"image"
	"unsafe"

	"github.com/go-gl/gl/v3.2-core/gl"
	"github.com/golang-ui/nuklear/nk"
)

type Texture struct {
	tex    uint32
	handle nk.Handle
	Width  int
	Height int
}

func (t *Texture) Close() error {
	gl.DeleteTextures(1, &t.tex)
	return nil
}

func (t *Texture) Image() nk.Image {
	return nk.NkImageHandle(t.handle)
}

func (t *Texture) SubImage(rect nk.Rect) nk.Image {
	return nk.NkSubimageHandle(t.handle, uint16(t.Width), uint16(t.Height), rect)
}

func (t *Texture) Update(img image.Image) error {
	gl.DeleteTextures(1, &t.tex)
	gl.GenTextures(1, &t.tex)
	switch i := img.(type) {
	case *image.NRGBA:
		rect := i.Rect
		gl.GenTextures(1, &t.tex)
		gl.BindTexture(gl.TEXTURE_2D, t.tex)
		gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
		gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
		gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
		gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
		gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, int32(rect.Dx()), int32(rect.Dy()),
			0, gl.RGBA, gl.UNSIGNED_BYTE, unsafe.Pointer(&i.Pix[0]))
		t.handle = nk.NkHandleId(int32(t.tex))
		t.Width = rect.Dx()
		t.Height = rect.Dy()
		return nil
	}
	return errors.New("unsupported image type")
}

func NewTexture(img image.Image) (*Texture, error) {
	switch i := img.(type) {
	case *image.NRGBA:
		var tex uint32
		rect := i.Rect
		gl.GenTextures(1, &tex)
		gl.BindTexture(gl.TEXTURE_2D, tex)
		gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
		gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
		gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
		gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
		gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, int32(rect.Dx()), int32(rect.Dy()),
			0, gl.RGBA, gl.UNSIGNED_BYTE, unsafe.Pointer(&i.Pix[0]))
		return &Texture{tex, nk.NkHandleId(int32(tex)), rect.Dx(), rect.Dy()}, nil
	}
	return nil, errors.New("unsupported image type")
}

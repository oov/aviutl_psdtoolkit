package editing

import (
	"bytes"
	"context"
	"encoding/json"
	"image"
	"math"
	"path/filepath"
	"time"

	"github.com/golang-ui/nuklear/nk"
	"github.com/pkg/errors"
	"golang.org/x/image/draw"

	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/source"
	"github.com/oov/aviutl_psdtoolkit/src/go/nkhelper"
	"github.com/oov/downscale"
)

const (
	thumbnailSize = 48
	textureSize   = 256

	limit = (textureSize * textureSize) / (thumbnailSize * thumbnailSize)
)

type Thumbnailer struct {
	ed   *Editing
	item *item
	t    *time.Timer
}

func makeThumbnail(src image.Image) (*image.NRGBA, error) {
	rect := src.Bounds()
	dx, dy := float64(rect.Dx()), float64(rect.Dy())
	f := thumbnailSize / math.Max(dx, dy)
	rect = image.Rect(0, 0, int(dx*f), int(dy*f))
	switch src0 := src.(type) {
	case *image.RGBA:
		tmp := image.NewRGBA(rect)
		if err := downscale.RGBAGamma(context.Background(), tmp, src0, 2.2); err != nil {
			return nil, err
		}
		src = tmp
	case *image.NRGBA:
		tmp := image.NewNRGBA(rect)
		if err := downscale.NRGBAGamma(context.Background(), tmp, src0, 2.2); err != nil {
			return nil, err
		}
		src = tmp
	default:
		// TODO: implement fallback
		return nil, errors.Errorf("unsupported image type %t", src)
	}

	r := image.NewNRGBA(rect)
	draw.Draw(r, rect, src, image.Pt(0, 0), draw.Over)
	return r, nil
}

func (t *Thumbnailer) Update(img image.Image, doer func(func())) {
	if t.t != nil {
		t.t.Stop()
	}
	t.t = time.AfterFunc(500*time.Millisecond, func() {
		thumb, err := makeThumbnail(img)
		if err != nil {
			// TODO: report error
			return
		}
		doer(func() {
			t.item.Thumbnail = thumb
			t.ed.thumbnails = nil
		})
	})
}

type item struct {
	DisplayName string
	Image       *img.Image

	Thumbnail *image.NRGBA
}

type Editing struct {
	Srcs          *source.Sources
	SelectedIndex int

	images []item

	thumbnailSheet        *image.NRGBA
	thumbnailSheetTexture *nkhelper.Texture
	thumbnails            []nk.Image
}

func (ed *Editing) Add(filePath string) error {
	if len(ed.images) == limit {
		return errors.Errorf("too many images")
	}
	img, err := ed.Srcs.NewImage(filePath)
	if err != nil {
		return errors.Wrapf(err, "editing: failed to load %q", filePath)
	}
	ed.images = append(ed.images, item{
		DisplayName: filepath.Base(*img.FilePath),
		Image:       img,
	})
	ed.thumbnails = nil
	ed.SelectedIndex = len(ed.images) - 1
	return nil
}

func (ed *Editing) Delete(index int) {
	copy(ed.images[index:], ed.images[index+1:])
	ed.images[len(ed.images)-1] = item{}
	ed.images = ed.images[:len(ed.images)-1]

	ed.thumbnails = nil
	if len(ed.images) == 0 {
		ed.SelectedIndex = 0
	} else {
		if index != 0 && index <= ed.SelectedIndex {
			ed.SelectedIndex--
		}
	}
}

func (ed *Editing) Clear() {
	ed.images = nil
	ed.thumbnails = nil
	ed.SelectedIndex = 0
}

func (ed *Editing) ThumbnailList() ([]nk.Image, error) {
	if ed.thumbnails == nil {
		ed.thumbnailSheet = image.NewNRGBA(image.Rect(0, 0, textureSize, textureSize))
		rects := make([]nk.Rect, len(ed.images))
		var tx, ty int
		for i, img := range ed.images {
			if img.Thumbnail == nil {
				continue
			}
			draw.Draw(
				ed.thumbnailSheet,
				image.Rect(tx, ty, tx+thumbnailSize, ty+thumbnailSize),
				img.Thumbnail,
				image.Pt(-(thumbnailSize-img.Thumbnail.Rect.Dx())/2, -(thumbnailSize-img.Thumbnail.Rect.Dy())/2),
				draw.Over,
			)
			rects[i] = nk.NkRect(float32(tx), float32(ty), thumbnailSize, thumbnailSize)
			tx += thumbnailSize
			if tx+thumbnailSize >= textureSize {
				tx = 0
				ty += thumbnailSize
			}
		}

		var err error
		if ed.thumbnailSheetTexture == nil {
			ed.thumbnailSheetTexture, err = nkhelper.NewTexture(ed.thumbnailSheet)
		} else {
			err = ed.thumbnailSheetTexture.Update(ed.thumbnailSheet)
		}
		if err != nil {
			return nil, errors.Wrapf(err, "editing: cannot create texture")
		}
		ed.thumbnails = make([]nk.Image, len(ed.images))
		for i, rect := range rects {
			ed.thumbnails[i] = ed.thumbnailSheetTexture.SubImage(rect)
		}
	}
	return ed.thumbnails, nil
}

func (ed *Editing) Len() int {
	r := len(ed.images)
	if r == 0 {
		return 1
	}
	return r
}

func (ed *Editing) Empty() bool {
	return len(ed.images) == 0
}

func (ed *Editing) SelectedImage() *img.Image {
	if ed.SelectedIndex < 0 || ed.SelectedIndex >= len(ed.images) {
		return nil
	}
	return ed.images[ed.SelectedIndex].Image
}

func (ed *Editing) SelectedImageDisplayName() string {
	if ed.SelectedIndex < 0 || ed.SelectedIndex >= len(ed.images) {
		return ""
	}
	return ed.images[ed.SelectedIndex].DisplayName
}

func (ed *Editing) SelectedImageThumbnailer() *Thumbnailer {
	if ed.SelectedIndex < 0 || ed.SelectedIndex >= len(ed.images) {
		return nil
	}
	return &Thumbnailer{ed: ed, item: &ed.images[ed.SelectedIndex]}
}

type serializeData struct {
	Images []*img.ProjectState
}

func (ed *Editing) Serialize() (string, error) {
	var srz serializeData
	for _, item := range ed.images {
		ps, err := item.Image.SerializeProject()
		if err != nil {
			return "", errors.Wrapf(err, "editing: cannot serialize %q", item.DisplayName)
		}
		srz.Images = append(srz.Images, ps)
	}
	b := bytes.NewBufferString("")
	if err := json.NewEncoder(b).Encode(srz); err != nil {
		return "", err
	}
	return b.String(), nil
}

func (ed *Editing) Deserialize(state string) error {
	if state == "" {
		ed.Clear()
		return nil
	}
	var srz serializeData
	if err := json.NewDecoder(bytes.NewReader([]byte(state))).Decode(&srz); err != nil {
		return err
	}
	ed.Clear()
	for _, ps := range srz.Images {
		// TODO: update thumbnail
		if err := ed.Add(ps.FilePath); err != nil {
			return errors.Wrapf(err, "editing: cannot load %q", ps.FilePath)
		}
		if err := ed.SelectedImage().DeserializeProject(ps); err != nil {
			return errors.Wrapf(err, "editing: failed to deserialize on %q", ps.FilePath)
		}
	}
	return nil
}

func (ed *Editing) Touch() {
	for _, item := range ed.images {
		item.Image.Touch()
	}
}

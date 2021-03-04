package editing

import (
	"bytes"
	"context"
	"encoding/json"
	"image"
	"image/png"
	"math"
	"path/filepath"
	"time"

	"github.com/golang-ui/nuklear/nk"
	"github.com/oov/downscale"
	"github.com/pkg/errors"
	"golang.org/x/image/draw"

	"psdtoolkit/img"
	"psdtoolkit/imgmgr/source"
	"psdtoolkit/nkhelper"
	"psdtoolkit/warn"
)

const (
	thumbnailSize = 48
	textureSize   = 512

	limit = int(textureSize/thumbnailSize) * int(textureSize/thumbnailSize)
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

func (t *Thumbnailer) Update(img image.Image, doer func(func() error) error) {
	if t.t != nil {
		t.t.Stop()
	}
	t.t = time.AfterFunc(500*time.Millisecond, func() {
		thumb, err := makeThumbnail(img)
		if err != nil {
			// TODO: report error
			return
		}
		if err = doer(func() error {
			t.item.Thumbnail = thumb
			t.ed.thumbnails = nil
			return nil
		}); err != nil {
			// TODO: report error
		}
	})
}

type item struct {
	DisplayName string
	Image       *img.Image
	Tag         int
	LatestState string

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

func (ed *Editing) UpdateLatestState(filePath string, tag int, state string) bool {
	for idx, item := range ed.images {
		if item.Tag == tag {
			ed.images[idx].LatestState = state
			return true
		}
	}
	return false
}

func (ed *Editing) Add(filePath string, tag int) (int, error) {
	if tag != 0 {
		for idx, item := range ed.images {
			if item.Tag == tag {
				return idx, nil
			}
		}
	}
	if len(ed.images) == limit {
		return -1, errors.Errorf("too many images")
	}
	img, err := ed.Srcs.NewImage(filePath)
	if err != nil {
		return -1, errors.Wrapf(err, "editing: failed to load %q", filePath)
	}
	ed.images = append(ed.images, item{
		DisplayName: filepath.Base(*img.FilePath),
		Image:       img,
		Tag:         tag,
	})
	ed.thumbnails = nil
	ed.SelectedIndex = len(ed.images) - 1
	return -1, nil
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

func (ed *Editing) SelectedImageLatestState() string {
	if ed.SelectedIndex < 0 || ed.SelectedIndex >= len(ed.images) {
		return ""
	}
	return ed.images[ed.SelectedIndex].LatestState
}

type serializeData struct {
	Image     *img.ProjectState
	Tag       int
	Thumbnail []byte
}

func (ed *Editing) Serialize() (string, error) {
	var srz []serializeData
	for _, item := range ed.images {
		var thumb []byte
		if item.Thumbnail != nil {
			b := bytes.NewBufferString("")
			if err := png.Encode(b, item.Thumbnail); err == nil {
				thumb = b.Bytes()
			}
		}
		srz = append(srz, serializeData{
			Image:     item.Image.SerializeProject(),
			Tag:       item.Tag,
			Thumbnail: thumb,
		})
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
	var srz []serializeData
	if err := json.NewDecoder(bytes.NewReader([]byte(state))).Decode(&srz); err != nil {
		return err
	}
	ed.Clear()
	var wr warn.Warning
	for _, d := range srz {
		if _, err := ed.Add(d.Image.FilePath, d.Tag); err != nil {
			wr = append(wr, errors.Wrapf(err, "editing: cannot load %q", d.Image.FilePath))
			continue
		}
		item := &ed.images[ed.SelectedIndex]
		if len(d.Thumbnail) > 0 {
			if img, err := png.Decode(bytes.NewReader(d.Thumbnail)); err == nil {
				item.Thumbnail = image.NewNRGBA(img.Bounds())
				draw.Draw(item.Thumbnail, item.Thumbnail.Rect, img, image.Point{}, draw.Over)
			}
		}
		if w, err := item.Image.DeserializeProject(d.Image); err != nil {
			wr = append(wr, errors.Wrapf(err, "editing: failed to deserialize on %q", d.Image.FilePath))
		} else if w != nil {
			wr = append(wr, w...)
		}
	}
	if wr != nil {
		ed.Srcs.Logger.Println(wr)
	}
	ed.thumbnails = nil
	return nil
}

func (ed *Editing) Touch() {
	for _, item := range ed.images {
		item.Image.Touch()
	}
}

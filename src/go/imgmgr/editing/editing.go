package editing

import (
	"bytes"
	"context"
	"encoding/json"
	"image"
	"math"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/pkg/errors"
	"golang.org/x/image/draw"

	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/source"
	"github.com/oov/downscale"
)

type Thumbnailer struct {
	ed   *Editing
	item *item
	t    *time.Timer
}

func makeThumbnail(src image.Image) (*image.NRGBA, error) {
	const thumbnailSize = 32

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

	r := image.NewNRGBA(image.Rect(0, 0, thumbnailSize, thumbnailSize))
	draw.Draw(r, r.Rect, src, image.Pt(0, 0), draw.Over)
	return r, nil
}

func (t *Thumbnailer) Update(img image.Image) {
	t.ed.thumbnailM.Lock()
	if t.t != nil {
		t.t.Stop()
	}
	t.t = time.AfterFunc(3*time.Second, func() {
		thumb, err := makeThumbnail(img)
		if err != nil {
			return
		}
		t.ed.thumbnailM.Lock()
		t.item.Thumbnail = thumb
		t.ed.thumbnailM.Unlock()
	})
	t.ed.thumbnailM.Unlock()
}

type item struct {
	DisplayName string
	Image       *img.Image

	Thumbnail *image.NRGBA
}

type Editing struct {
	Srcs          *source.Sources
	SelectedIndex int

	images     []item
	stringList string

	thumbnailM sync.Mutex
}

func (ed *Editing) Add(filePath string) error {
	img, err := ed.Srcs.NewImage(filePath)
	if err != nil {
		return errors.Wrapf(err, "editing: failed to load %q", filePath)
	}
	ed.images = append(ed.images, item{
		DisplayName: filepath.Base(*img.FilePath),
		Image:       img,
	})
	ed.stringList = ""
	ed.SelectedIndex = len(ed.images) - 1
	return nil
}

func (ed *Editing) Delete(index int) {
	copy(ed.images[index:], ed.images[index+1:])
	ed.images[len(ed.images)-1] = item{}
	ed.images = ed.images[:len(ed.images)-1]

	ed.stringList = ""
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
	ed.stringList = ""
	ed.SelectedIndex = 0
}

func (ed *Editing) StringList() string {
	if ed.stringList == "" {
		if ed.Empty() {
			ed.stringList = "<NO ITEMS>\x00"
		} else {
			var s []string
			for _, img := range ed.images {
				s = append(s, img.DisplayName, "\x00")
			}
			ed.stringList = strings.Join(s, "")
		}
	}
	return ed.stringList
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

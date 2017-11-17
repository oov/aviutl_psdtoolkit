package editing

import (
	"bytes"
	"encoding/json"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/source"
)

type item struct {
	DisplayName string
	Image       *img.Image
}

type Editing struct {
	Srcs          *source.Sources
	SelectedIndex int

	images     []item
	stringList string
}

func (ed *Editing) Add(filePath string) (*img.Image, error) {
	img, err := ed.Srcs.NewImage(filePath)
	if err != nil {
		return nil, errors.Wrapf(err, "editing: failed to load %q", filePath)
	}
	ed.images = append(ed.images, item{
		DisplayName: filepath.Base(*img.FilePath),
		Image:       img,
	})
	ed.stringList = ""
	ed.SelectedIndex = len(ed.images) - 1
	return img, nil
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
		img, err := ed.Add(ps.FilePath)
		if err != nil {
			return errors.Wrapf(err, "editing: cannot load %q", ps.FilePath)
		}
		if err = img.DeserializeProject(ps); err != nil {
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

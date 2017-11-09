package imagelist

import (
	"strings"

	"github.com/oov/aviutl_psdtoolkit/src/go/ipc"
)

type ImageList struct {
	keys          ipc.StateKeys
	s             string
	SelectedIndex int
}

func (skl *ImageList) UpdateKeys(keys ipc.StateKeys) {
	var oldKey ipc.StateKey
	if !skl.Empty() {
		oldKey = skl.keys[skl.SelectedIndex]
	}

	skl.keys = keys
	if skl.Empty() {
		skl.s = "<NO ITEMS>\x00"
		skl.SelectedIndex = 0
		return
	}

	var s []string
	for _, k := range skl.keys {
		s = append(s, k.String())
	}
	skl.s = strings.Join(s, "\x00")
	skl.SelectedIndex = len(keys) - 1
	for index, k := range skl.keys {
		if oldKey == k {
			skl.SelectedIndex = index
			break
		}
	}
}

func (skl *ImageList) List() string {
	return skl.s
}

func (skl *ImageList) Len() int {
	r := len(skl.keys)
	if r == 0 {
		return 1
	}
	return r
}

func (skl *ImageList) Empty() bool {
	return len(skl.keys) == 0
}

func (skl *ImageList) SelectedKey() ipc.StateKey {
	if skl.Empty() {
		return ipc.StateKey{}
	}
	return skl.keys[skl.SelectedIndex]
}

package img

import (
	"context"
	"os"
	"testing"

	"github.com/oov/psd/composite"
)

func LoadTestFile() (*LayerManager, error) {
	file, err := os.Open("testdata/test.psd")
	if err != nil {
		return nil, err
	}
	tree, err := composite.New(context.Background(), file, &composite.Options{})
	if err != nil {
		return nil, err
	}
	return NewLayerManager(tree), nil
}

type VisibleTest struct {
	Name string
	Old  bool
	New  bool
}

func TestFlipX(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{
			Name: "root",
			Old:  true,
			New:  true,
		},
		{
			Name: "test:flipx",
			Old:  false,
			New:  true,
		},
		{
			Name: "test",
			Old:  true,
			New:  false,
		},
		{
			Name: "!folder",
			Old:  true,
			New:  false,
		},
		{
			Name: "!folder:flipx",
			Old:  false,
			New:  true,
		},
		{
			Name: "!folder/*chk1",
			Old:  true,
			New:  false,
		},
		{
			Name: "!folder:flipx/*chk1",
			Old:  false,
			New:  true,
		},
		{
			Name: "!folder/*chk2",
			Old:  false,
			New:  false,
		},
		{
			Name: "!folder:flipx/*chk2",
			Old:  false,
			New:  false,
		},
		{
			Name: "!folder/c1",
			Old:  true,
			New:  false,
		},
		{
			Name: "!folder:flipx/c1",
			Old:  false,
			New:  true,
		},
		{
			Name: "!folder/c2",
			Old:  false,
			New:  false,
		},
		{
			Name: "!folder:flipx/c2",
			Old:  false,
			New:  false,
		},
	}
	for _, v := range testData {
		l := lm.FindLayerByFullPathLayerName(v.Name)
		if l == nil {
			t.Fatalf("layer %q not found.", v.Name)
		}
		if l.Visible != v.Old {
			t.Errorf("layer %q initial state is not expected.", v.Name)
		}
	}
	_, _, err = lm.Deserialize("L.1", FlipNone, nil)
	if err != nil {
		t.Error("failed to deserialize.")
	}
	for _, v := range testData {
		l := lm.FindLayerByFullPathLayerName(v.Name)
		if l == nil {
			t.Fatalf("layer %q not found.", v.Name)
		}
		if l.Visible != v.New {
			t.Errorf("layer %q expected %v got %v.", v.Name, v.New, l.Visible)
		}
	}
}

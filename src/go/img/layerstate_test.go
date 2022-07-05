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

func verifyOld(t *testing.T, lm *LayerManager, tests []VisibleTest) {
	for _, v := range tests {
		l := lm.FindLayerByFullPath(v.Name)
		if l == nil {
			t.Fatalf("layer %q not found.", v.Name)
		}
		if l.Layer.Visible != v.Old {
			t.Errorf("layer %q initial state is not expected.", v.Name)
		}
	}
}

func verifyNew(t *testing.T, lm *LayerManager, tests []VisibleTest) {
	for _, v := range tests {
		l := lm.FindLayerByFullPath(v.Name)
		if l == nil {
			t.Fatalf("layer %q not found.", v.Name)
		}
		if l.Layer.Visible != v.New {
			t.Errorf("layer %q expected %v got %v.", v.Name, v.New, l.Layer.Visible)
		}
	}
}

func TestInitial(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, false},
		{"test:flipx", false, false},
		{"test", true, true},
		{"!folder", true, true},
		{"!folder:flipx", false, false},
		{"!folder/*chk1", true, true},
		{"!folder:flipx/*chk1", true, true},
		{"!folder/*chk2", false, false},
		{"!folder:flipx/*chk2", false, false},
		{"!folder/c1", true, true},
		{"!folder:flipx/c1", true, true},
		{"!folder/c2", false, false},
		{"!folder:flipx/c2", false, false},
	}
	verifyOld(t, lm, testData)
}

func TestFlipX(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, true},
		{"test:flipx", false, true},
		{"test", true, false},
		{"!folder", true, false},
		{"!folder:flipx", false, true},
		{"!folder/*chk1", true, true},
		{"!folder:flipx/*chk1", true, true},
		{"!folder/*chk2", false, false},
		{"!folder:flipx/*chk2", false, false},
		{"!folder/c1", true, true},
		{"!folder:flipx/c1", true, true},
		{"!folder/c2", false, false},
		{"!folder:flipx/c2", false, false},
	}
	verifyOld(t, lm, testData)
	r, err := lm.Deserialize("L.1", nil)
	if err != nil {
		t.Error("failed to set flip.")
	}
	if !r {
		t.Errorf("expected %v got %v", true, r)
	}
	verifyNew(t, lm, testData)
}

func TestFlipY(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, true},
		{"test:flipx", false, false},
		{"test", true, true},
		{"!folder", true, true},
		{"!folder:flipx", false, false},
		{"!folder/*chk1", true, true},
		{"!folder:flipx/*chk1", true, true},
		{"!folder/*chk2", false, false},
		{"!folder:flipx/*chk2", false, false},
		{"!folder/c1", true, true},
		{"!folder:flipx/c1", true, true},
		{"!folder/c2", false, false},
		{"!folder:flipx/c2", false, false},
	}
	verifyOld(t, lm, testData)
	r, err := lm.Deserialize("L.2", nil)
	if err != nil {
		t.Error("failed to set flip.")
	}
	if !r {
		t.Errorf("expected %v got %v", true, r)
	}
	verifyNew(t, lm, testData)
}

func TestSetVisibleFalse(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, false},
		{"test:flipx", false, false},
		{"test", true, true},
		{"!folder", true, true},
		{"!folder:flipx", false, false},
		{"!folder/*chk1", true, true},
		{"!folder:flipx/*chk1", true, true},
		{"!folder/*chk2", false, false},
		{"!folder:flipx/*chk2", false, false},
		{"!folder/c1", true, true},
		{"!folder:flipx/c1", true, true},
		{"!folder/c2", false, false},
		{"!folder:flipx/c2", false, false},
	}
	verifyOld(t, lm, testData)
	r := lm.SetVisible(SeqID(lm.FindLayerByFullPath("root").Layer.SeqID), false)
	if !r {
		t.Errorf("expected %v got %v", true, r)
	}
	verifyNew(t, lm, testData)
}

func TestSetVisibleTrue(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, true},
		{"test:flipx", false, false},
		{"test", true, true},
		{"!folder", true, true},
		{"!folder:flipx", false, false},
		{"!folder/*chk1", true, true},
		{"!folder:flipx/*chk1", true, true},
		{"!folder/*chk2", false, false},
		{"!folder:flipx/*chk2", false, false},
		{"!folder/c1", true, true},
		{"!folder:flipx/c1", true, true},
		{"!folder/c2", false, true},
		{"!folder:flipx/c2", false, true},
	}
	verifyOld(t, lm, testData)
	r := lm.SetVisible(SeqID(lm.FindLayerByFullPath("!folder/c2").Layer.SeqID), true)
	if !r {
		t.Errorf("expected %v got %v", true, r)
	}
	verifyNew(t, lm, testData)
}

func TestSetVisibleTrueSynced(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, true},
		{"test:flipx", false, false},
		{"test", true, true},
		{"!folder", true, true},
		{"!folder:flipx", false, false},
		{"!folder/*chk1", true, true},
		{"!folder:flipx/*chk1", true, true},
		{"!folder/*chk2", false, false},
		{"!folder:flipx/*chk2", false, false},
		{"!folder/c1", true, true},
		{"!folder:flipx/c1", true, true},
		{"!folder/c2", false, true},
		{"!folder:flipx/c2", false, true},
	}
	verifyOld(t, lm, testData)
	r := lm.SetVisible(SeqID(lm.FindLayerByFullPath("!folder:flipx/c2").Layer.SeqID), true)
	if !r {
		t.Errorf("expected %v got %v", true, r)
	}
	verifyNew(t, lm, testData)
}

func TestSetVisibleGroupTrue(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, true},
		{"test:flipx", false, false},
		{"test", true, true},
		{"!folder", true, true},
		{"!folder:flipx", false, false},
		{"!folder/*chk1", true, false},
		{"!folder:flipx/*chk1", true, false},
		{"!folder/*chk2", false, true},
		{"!folder:flipx/*chk2", false, true},
		{"!folder/c1", true, true},
		{"!folder:flipx/c1", true, true},
		{"!folder/c2", false, false},
		{"!folder:flipx/c2", false, false},
	}
	verifyOld(t, lm, testData)
	r := lm.SetVisible(SeqID(lm.FindLayerByFullPath("!folder/*chk2").Layer.SeqID), true)
	if !r {
		t.Errorf("expected %v got %v", true, r)
	}
	verifyNew(t, lm, testData)
}

func TestSetVisibleGroupTrueSynced(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, true},
		{"test:flipx", false, false},
		{"test", true, true},
		{"!folder", true, true},
		{"!folder:flipx", false, false},
		{"!folder/*chk1", true, false},
		{"!folder:flipx/*chk1", true, false},
		{"!folder/*chk2", false, true},
		{"!folder:flipx/*chk2", false, true},
		{"!folder/c1", true, true},
		{"!folder:flipx/c1", true, true},
		{"!folder/c2", false, false},
		{"!folder:flipx/c2", false, false},
	}
	verifyOld(t, lm, testData)
	r := lm.SetVisible(SeqID(lm.FindLayerByFullPath("!folder:flipx/*chk2").Layer.SeqID), true)
	if !r {
		t.Errorf("expected %v got %v", true, r)
	}
	verifyNew(t, lm, testData)
}

func TestSetVisibleExclusiveTrue(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, true},
		{"test:flipx", false, false},
		{"test", true, true},
		{"!folder", true, true},
		{"!folder:flipx", false, false},
		{"!folder/*chk1", true, true},
		{"!folder:flipx/*chk1", true, true},
		{"!folder/*chk2", false, false},
		{"!folder:flipx/*chk2", false, false},
		{"!folder/c1", true, false},
		{"!folder:flipx/c1", true, false},
		{"!folder/c2", false, true},
		{"!folder:flipx/c2", false, true},
	}
	verifyOld(t, lm, testData)
	r := lm.SetVisibleExclusive(SeqID(lm.FindLayerByFullPath("!folder/c2").Layer.SeqID), true)
	if !r {
		t.Errorf("expected %v got %v", true, r)
	}
	verifyNew(t, lm, testData)
}

func TestSetVisibleExclusiveGroupTrue(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, true},
		{"test:flipx", false, false},
		{"test", true, true},
		{"!folder", true, true},
		{"!folder:flipx", false, false},
		{"!folder/*chk1", true, false},
		{"!folder:flipx/*chk1", true, false},
		{"!folder/*chk2", false, true},
		{"!folder:flipx/*chk2", false, true},
		{"!folder/c1", true, true},
		{"!folder:flipx/c1", true, true},
		{"!folder/c2", false, false},
		{"!folder:flipx/c2", false, false},
	}
	verifyOld(t, lm, testData)
	r := lm.SetVisibleExclusive(SeqID(lm.FindLayerByFullPath("!folder:flipx/*chk2").Layer.SeqID), true)
	if !r {
		t.Errorf("expected %v got %v", true, r)
	}
	verifyNew(t, lm, testData)
}

func TestSetVisibleExclusiveGroupTrue2(t *testing.T) {
	lm, err := LoadTestFile()
	if err != nil {
		t.Error("failed to load test file.")
	}
	testData := []VisibleTest{
		{"root", true, true},
		{"test:flipx", false, false},
		{"test", true, true},
		{"!folder", true, true},
		{"!folder:flipx", false, false},
		{"!folder/*chk1", true, false},
		{"!folder:flipx/*chk1", true, false},
		{"!folder/*chk2", false, true},
		{"!folder:flipx/*chk2", false, true},
		{"!folder/c1", true, false},
		{"!folder:flipx/c1", true, false},
		{"!folder/c2", false, true},
		{"!folder:flipx/c2", false, true},
	}
	verifyOld(t, lm, testData)
	lm.SetVisible(SeqID(lm.FindLayerByFullPath("!folder:flipx/*chk2").Layer.SeqID), true)
	r := lm.SetVisibleExclusive(SeqID(lm.FindLayerByFullPath("!folder/c2").Layer.SeqID), true)
	if !r {
		t.Errorf("expected %v got %v", true, r)
	}
	verifyNew(t, lm, testData)
}

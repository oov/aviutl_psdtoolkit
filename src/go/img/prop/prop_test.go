package prop

import "testing"

var testdata = [][2]string{
	{"表現不可能なソース文字列", ".%xg現不可%xfな%x1ース文字列"},
	{"表現不可能", ".%xg現不可%xf"},
	{"ソース顔", ".%x1ース顔"},
	{"👍グッドな文字", ".%u68J-RjQグッドな文字"},
	{"グッド👍な文字", ".グッド%u68J-RjQな文字"},
	{"グッドな文字👍", ".グッドな文字%u68J-RjQ"},
}

func TestEncode(t *testing.T) {
	for idx, data := range testdata {
		if got := Encode(data[0]); data[1] != got {
			t.Errorf("[%d] want %q, got %q", idx, data[1], got)
		}
	}
}

func TestDecode(t *testing.T) {
	for idx, data := range testdata {
		got, err := Decode(data[1])
		if err != nil {
			t.Fatal(err)
		}
		if data[0] != got {
			t.Errorf("[%d] want %q, got %q", idx, data[0], got)
		}
	}
}

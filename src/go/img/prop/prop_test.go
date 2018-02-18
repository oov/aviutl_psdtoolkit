package prop

import "testing"

var testdata = [][2]string{
	{"表現不可能なソース文字列", ".%xQ現不可%xPな%xBース文字列"},
	{"表現不可能", ".%xQ現不可%xP"},
	{"ソース顔", ".%xBース顔"},
	{"👍グッドな文字", ".%vTfQBグッドな文字"},
	{"グッド👍な文字", ".グッド%vTfQBな文字"},
	{"グッドな文字👍", ".グッドな文字%vTfQB"},
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

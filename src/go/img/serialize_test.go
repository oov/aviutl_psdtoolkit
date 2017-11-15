package img

import (
	"testing"
)

type serializeBitsTest struct {
	Visibility []bool
	Want       string
}

var serializeBitsTestData = []serializeBitsTest{
	{[]bool{}, "AgA"},
	{[]bool{false}, "_wECAA"},
	{[]bool{true}, "_QEAAQ"},
	{[]bool{false, false}, "_wICAA"},
	{[]bool{true, false}, "_QIAAg"},
	{[]bool{false, true}, "_QIAAQ"},
	{[]bool{true, true}, "_QIAAw"},
}

func TestSerializeBits(t *testing.T) {
	for _, testData := range serializeBitsTestData {
		got, err := serializeBits(testData.Visibility)
		if err != nil {
			t.Errorf("serialize(%v): error %v", err)
		}
		if got != testData.Want {
			t.Errorf("serialize(%v) == %q, want %q", testData.Visibility, got, testData.Want)
		}
	}
}

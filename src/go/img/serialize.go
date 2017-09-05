package img

import (
	"encoding/base64"
	"encoding/binary"
)

func serialize(filter, visibility []bool) string {
	if filter == nil {
		buf := make([]byte, 2+(len(visibility)+7)/8)
		binary.LittleEndian.PutUint16(buf, uint16(len(visibility)))
		var v byte
		d := 2
		for i, visible := range visibility {
			v <<= 1
			if visible {
				v++
			}
			if i&0x7 == 0x7 {
				buf[d] = v
				v = 0
				d++
			}
		}
		if d < len(buf) {
			buf[d] = v
		}
		return "V." + base64.RawURLEncoding.EncodeToString(encodePackBits(buf))
	}

	buf := make([]byte, 2+(len(visibility)*2+7)/8)
	binary.LittleEndian.PutUint16(buf, uint16(len(visibility)))
	var v byte
	d := 2
	for i, pass := range filter {
		v <<= 2
		if pass {
			if visibility[i] {
				v += 3
			} else {
				v += 2
			}
		}
		if i&0x3 == 0x3 {
			buf[d] = v
			v = 0
			d++
		}
	}
	if d < len(buf) {
		buf[d] = v
	}
	return "F." + base64.RawURLEncoding.EncodeToString(encodePackBits(buf))
}

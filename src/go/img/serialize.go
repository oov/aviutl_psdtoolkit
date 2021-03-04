package img

import (
	"encoding/base64"
	"encoding/binary"

	"github.com/pkg/errors"

	"psdtoolkit/img/internal/packbits"
)

// serializeBits maps the bool value of the array to each bit and compresses the result with PackBits.
func serializeBits(bits []bool) (string, error) {
	if len(bits) > 0xffff {
		return "", errors.Errorf("img: too many bits(%d)", len(bits))
	}
	buf := make([]byte, 2+(len(bits)+7)/8)
	binary.LittleEndian.PutUint16(buf, uint16(len(bits)))
	var v byte
	d := 2
	for i, b := range bits {
		v <<= 1
		if b {
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
	return base64.RawURLEncoding.EncodeToString(packbits.Encode(buf)), nil
}

func deserializeBits(s string) ([]byte, error) {
	buf, err := base64.RawURLEncoding.DecodeString(s)
	if err != nil {
		return nil, err
	}
	return packbits.Decode(buf)
}

func deserializeBitsAsBool(s string) ([]bool, error) {
	buf, err := deserializeBits(s)
	if err != nil {
		return nil, err
	}
	b := make([]bool, binary.LittleEndian.Uint16(buf))
	i := 0
	for _, v := range buf[2:] {
		if i+7 < len(b) {
			b[i+0] = v&0x80 != 0
			b[i+1] = v&0x40 != 0
			b[i+2] = v&0x20 != 0
			b[i+3] = v&0x10 != 0
			b[i+4] = v&0x08 != 0
			b[i+5] = v&0x04 != 0
			b[i+6] = v&0x02 != 0
			b[i+7] = v&0x01 != 0
			i += 8
			continue
		}
		v <<= 8 - uint(len(b)-i)
		for i < len(b) {
			b[i] = v&0x80 != 0
			v <<= 1
			i++
		}
	}
	return b, nil
}

package img

import "errors"

func decodePackBits(b []byte) ([]byte, error) {
	lenb := len(b)
	r := make([]byte, 0, lenb)
	i := 0
	var c byte
	for i < lenb {
		c = b[i]
		i++
		if c&0x80 == 0 {
			if i >= lenb {
				return nil, errors.New("img: unexpected EOF")
			}
			n := int(c)
			c = b[i]
			for ; n > 0; n-- {
				r = append(r, c)
			}
			i++
		} else {
			n := int(^c + 1)
			if i+n > lenb {
				return nil, errors.New("img: unexpected EOF")
			}
			r = append(r, b[i:i+n]...)
			i += n
		}
	}
	return r, nil
}

func encodePackBits(b []byte) []byte {
	switch len(b) {
	case 0:
		return []byte{}
	case 1:
		return []byte{0xff, b[0]}
	case 2:
		if b[0] == b[1] {
			return []byte{0x02, b[0]}
		}
		return []byte{0xfe, b[0], b[1]}
	}

	r := make([]byte, 0, len(b))
	pos := 0
	i := 1
	var repeated bool
	var c byte
	for i < len(b) {
		repeated = b[i-1] == b[i]
		if repeated {
			c = b[i]
			for i < len(b) {
				if c == b[i] {
					i++
					continue
				}
				for i-pos > 127 {
					r = append(r, 0x7f, c)
					pos += 127
				}
				if i-pos > 0 {
					r = append(r, byte(i-pos), c)
				}
				pos = i
				i++
				break
			}
		} else {
			for i < len(b) {
				if b[i-1] != b[i] {
					i++
					continue
				}
				i--
				for i-pos > 128 {
					r = append(r, 0x80)
					r = append(r, b[pos:pos+128]...)
					pos += 128
				}
				if i-pos > 0 {
					r = append(r, byte(-int8(i-pos)))
					r = append(r, b[pos:i]...)
				}
				pos = i
				i++
				break
			}
		}
	}
	if pos != i {
		repeated = b[i-2] == b[i-1]
		if repeated {
			for i-pos > 127 {
				r = append(r, 0x7f, c)
				pos += 127
			}
			if i-pos > 0 {
				r = append(r, byte(i-pos), c)
			}
		} else {
			for i-pos > 128 {
				r = append(r, 0x80)
				r = append(r, b[pos:pos+128]...)
				pos += 128
			}
			if i-pos > 0 {
				r = append(r, byte(-int8(i-pos)))
				r = append(r, b[pos:i]...)
			}
		}
	}
	return r
}

package img

import "net/url"

func decodeName(s string) (string, error) {
	return url.PathUnescape(s)
}

func toHexChar(v byte) byte {
	if v < 10 {
		return '0' + v
	}
	return 'a' + v - 10
}

func encodeName(s string) string {
	b := make([]byte, 0, len(s))
	for _, c := range []byte(s) {
		switch c {
		case 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
			0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
			0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
			0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
			0x22, 0x25, 0x27, 0x2f, 0x5c, 0x7e, 0x7f:
			b = append(b, '%', toHexChar(c>>4), toHexChar(c&15))
			continue
		}
		b = append(b, c)
	}
	return string(b)
}

func itoa(x int) string {
	if x < 10 {
		return string([]byte{byte(x + '0')})
	} else if x < 100 {
		return string([]byte{byte(x/10 + '0'), byte(x%10 + '0')})
	}

	var b [32]byte
	i := len(b) - 1
	for x > 9 {
		b[i] = byte(x%10 + '0')
		x /= 10
		i--
	}
	b[i] = byte(x + '0')
	return string(b[i:])
}

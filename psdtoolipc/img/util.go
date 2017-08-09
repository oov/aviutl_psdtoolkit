package img

func toHexChar(v byte) byte {
	if v < 10 {
		return '0' + v
	}
	return 'a' + v - 10
}

func encodeName(s string) string {
	b := make([]byte, 0, len(s))
	for _, c := range []byte(s) {
		if c < 0x20 {
			b = append(b, '%', toHexChar(c>>4), toHexChar(c&15))
			continue
		}
		switch c {
		case 0x22, 0x25, 0x27, 0x2f, 0x5c, 0x7e, 0x7f:
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

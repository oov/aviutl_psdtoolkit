package prop

import (
	"encoding/base64"

	"github.com/pkg/errors"
	"golang.org/x/text/encoding/japanese"
)

var encoder = japanese.ShiftJIS.NewEncoder()

var damemoji = map[rune]int{
	'―': 0, 'ソ': 1, 'Ы': 2, 'Ⅸ': 3, '噂': 4, '浬': 5, '欺': 6, '圭': 7,
	'構': 8, '蚕': 9, '十': 10, '申': 11, '曾': 12, '箪': 13, '貼': 14, '能': 15,
	'表': 16, '暴': 17, '予': 18, '禄': 19, '兔': 20, '喀': 21, '媾': 22, '彌': 23,
	'拿': 24, '杤': 25, '歃': 26, '濬': 27, '畚': 28, '秉': 29, '綵': 30, '臀': 31,
	'藹': 32, '觸': 33, '軆': 34, '鐔': 35, '饅': 36, '鷭': 37, '纊': 38, '犾': 39,
	'偆': 40, '砡': 41,
}
var damemojiRev = [42]string{
	"―", "ソ", "Ы", "Ⅸ", "噂", "浬", "欺", "圭",
	"構", "蚕", "十", "申", "曾", "箪", "貼", "能",
	"表", "暴", "予", "禄", "兔", "喀", "媾", "彌",
	"拿", "杤", "歃", "濬", "畚", "秉", "綵", "臀",
	"藹", "觸", "軆", "鐔", "饅", "鷭", "纊", "犾",
	"偆", "砡",
}

var rune64 = [64]rune{
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
	'-', '_',
}

func Encode(s string) string {
	if len(s) == 0 {
		return ""
	}
	runes := []rune(s)
	escaped := make([]rune, 0, len(runes)+1)
	escaped = append(escaped, '.')
	for _, r := range runes {
		if idx, found := damemoji[r]; found {
			// escape damemoji to %xN
			escaped = append(escaped, '%', 'x', rune64[idx])
			continue
		}
		ch := string(r)
		if _, err := encoder.String(ch); err != nil {
			// this char seems is not available in Shift_JIS.
			b64 := base64.RawURLEncoding.EncodeToString([]byte(ch))
			escaped = append(escaped, '%', 'u', rune64[len(b64)])
			escaped = append(escaped, []rune(b64)...)
			continue
		}
		escaped = append(escaped, r)
	}
	return string(escaped)
}

func rune64ToInt(r byte) int {
	if '0' <= r && r <= '9' {
		return int(r - '0')
	}
	if 'a' <= r && r <= 'z' {
		return int(10 + r - 'a')
	}
	if 'A' <= r && r <= 'Z' {
		return int(36 + r - 'A')
	}
	if r == '-' {
		return 62
	}
	if r == '_' {
		return 63
	}
	return -1
}

func unescape(s string) string {
	l := len(s)
	r := make([]byte, 0, l)
	for i := 0; i < l; i++ {
		if s[i] != '%' {
			r = append(r, s[i])
			continue
		}
		if i+2 < l {
			switch s[i+1] {
			case 'x':
				if idx := rune64ToInt(s[i+2]); 0 <= idx && idx < len(damemojiRev) {
					r = append(r, damemojiRev[idx]...)
					i += 2
					continue
				}
			case 'u':
				if ln := rune64ToInt(s[i+2]); ln != -1 && i+2+ln < l {
					if b, err := base64.RawURLEncoding.DecodeString(s[i+3 : i+3+ln]); err == nil {
						r = append(r, b...)
						i += 2 + ln
						continue
					}
				}
			}
		}
		r = append(r, s[i])
	}
	return string(r)
}

func Decode(s string) (string, error) {
	if len(s) <= 1 {
		return "", nil
	}
	switch s[0] {
	case '_':
		b, err := base64.RawURLEncoding.DecodeString(s[1:])
		if err != nil {
			return "", err
		}
		return string(b), nil
	case '.':
		return unescape(s[1:]), nil
	}
	return "", errors.Errorf("unsupported encoding type: %q", s[0:1])
}

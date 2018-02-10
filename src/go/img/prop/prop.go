package prop

import (
	"encoding/base64"

	"github.com/pkg/errors"
	"golang.org/x/text/encoding/japanese"
)

var encoder = japanese.ShiftJIS.NewEncoder()

func Encode(s string) string {
	if len(s) == 0 {
		return ""
	}
	for _, r := range s {
		switch r {
		case '―', 'ソ', 'Ы', 'Ⅸ', '噂', '浬', '欺', '圭',
			'構', '蚕', '十', '申', '曾', '箪', '貼', '能',
			'表', '暴', '予', '禄', '兔', '喀', '媾', '彌',
			'拿', '杤', '歃', '濬', '畚', '秉', '綵', '臀',
			'藹', '觸', '軆', '鐔', '饅', '鷭', '纊', '犾',
			'偆', '砡':
			return "_" + base64.RawURLEncoding.EncodeToString([]byte(s))
		}
	}
	if _, err := encoder.String(s); err != nil {
		return "_" + base64.RawURLEncoding.EncodeToString([]byte(s))
	}

	return "." + s
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
		return s[1:], nil
	}
	return "", errors.Errorf("unsupported encoding type: %q", s[0:1])
}

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

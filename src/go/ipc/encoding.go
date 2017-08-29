// This code is based on the_platinum_searcher.
// https://github.com/monochromegane/the_platinum_searcher
// The MIT License (MIT) Copyright (c) [2014] [the_platinum_searcher]

package ipc

import (
	"golang.org/x/text/encoding"
	"golang.org/x/text/encoding/japanese"
	"golang.org/x/text/encoding/unicode"
)

func autoDetect(bs []byte) encoding.Encoding {
	var (
		suspiciousBytes = 0
		likelyUtf8      = 0
		likelyEucjp     = 0
		likelyShiftjis  = 0
		likelyIso2022jp = 0
	)

	var length = len(bs)
	if length == 0 {
		return encoding.Nop // ASCII
	}

	if length >= 2 {
		switch {
		case bs[0] == 0xFF && bs[1] == 0xFE:
			return unicode.UTF16(unicode.LittleEndian, unicode.UseBOM)
		case bs[0] == 0xFE && bs[1] == 0xFF:
			return unicode.UTF16(unicode.BigEndian, unicode.UseBOM)
		}
	}

	if length >= 3 && bs[0] == 0xEF && bs[1] == 0xBB && bs[2] == 0xBF {
		// UTF-8 BOM. This isn't binary.
		return encoding.Nop // UTF-8
	}

	if length >= 5 && bs[0] == 0x25 && bs[1] == 0x50 && bs[2] == 0x44 && bs[3] == 0x46 && bs[4] == 0x2D {
		/*  %PDF-. This is binary. */
		return encoding.Nop // Binary
	}

	for i := 0; i < length; i++ {
		if bs[i] == 0x00 {
			/* NULL char. It's binary */
			return encoding.Nop // Binary
		} else if (bs[i] < 7 || bs[i] > 14) && (bs[i] < 32 || bs[i] > 127) {
			/* UTF-8 detection */
			if bs[i] > 193 && bs[i] < 224 && i+1 < length {
				i++
				if bs[i] > 127 && bs[i] < 192 {
					likelyUtf8++
					continue
				}

			} else if bs[i] > 223 && bs[i] < 240 && i+2 < length {
				i++
				if bs[i] > 127 && bs[i] < 192 && bs[i+1] > 127 && bs[i+1] < 192 {
					i++
					likelyUtf8++
					continue
				}
			}

			/* EUC-JP detection */
			if bs[i] == 142 && i+1 < length {
				i++
				if bs[i] > 160 && bs[i] < 224 {
					likelyEucjp++
					continue
				}
			} else if bs[i] > 160 && bs[i] < 255 && i+1 < length {
				i++
				if bs[i] > 160 && bs[i] < 255 {
					likelyEucjp++
					continue
				}
			}

			/* Shift-JIS detection */
			if bs[i] > 160 && bs[i] < 224 {
				likelyShiftjis++
				continue
			} else if ((bs[i] > 128 && bs[i] < 160) || (bs[i] > 223 && bs[i] < 240)) && i+1 < length {
				i++
				if (bs[i] > 63 && bs[i] < 127) || (bs[i] > 127 && bs[i] < 253) {
					likelyShiftjis++
					continue
				}
			}

			/* ISO-2022-JP detection */
			if bs[i] == 27 && i+2 < length {
				i++
				switch bs[i] {
				case 36:
					i++
					if bs[i] == 64 || bs[i] == 66 || bs[i] == 68 {
						likelyIso2022jp++
						continue
					}
				case 40:
					i++
					if bs[i] == 66 || bs[i] == 73 || bs[i] == 74 {
						likelyIso2022jp++
						continue
					}
				}
			}

			suspiciousBytes++
			if i >= 32 && (suspiciousBytes*100)/length > 10 {
				return encoding.Nop // Binary
			}

		}
	}

	if (suspiciousBytes*100)/length > 10 {
		return encoding.Nop // Binary
	}

	// fmt.Printf("Detected points[utf8/eucjp/shiftjis] is %d/%d/%d.\n", likelyUtf8, likelyEucjp, likelyShiftjis)

	if likelyUtf8 == 0 && likelyEucjp == 0 && likelyShiftjis == 0 && likelyIso2022jp == 0 {
		return encoding.Nop // ASCII
	} else if likelyUtf8 >= likelyEucjp && likelyUtf8 >= likelyShiftjis && likelyUtf8 >= likelyIso2022jp {
		return encoding.Nop // UTF-8
	} else if likelyEucjp >= likelyUtf8 && likelyEucjp >= likelyShiftjis && likelyEucjp >= likelyIso2022jp {
		return japanese.EUCJP
	} else if likelyShiftjis >= likelyUtf8 && likelyShiftjis >= likelyEucjp && likelyShiftjis >= likelyIso2022jp {
		return japanese.ShiftJIS
	} else if likelyIso2022jp >= likelyUtf8 && likelyIso2022jp >= likelyEucjp && likelyIso2022jp >= likelyShiftjis {
		return japanese.ISO2022JP
	}

	return encoding.Nop
}

package ipc

import (
	"encoding/binary"
	"errors"
	"io"
	"math"
	"os"

	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
)

func rgbaToNBGRA(p []byte) {
	for i := 0; i < len(p); i += 4 {
		a := uint32(p[i+3])
		if a > 0 {
			p[i+2], p[i+1], p[i+0] = uint8(uint32(p[i+0])*0xff/a), uint8(uint32(p[i+1])*0xff/a), uint8(uint32(p[i+2])*0xff/a)
		}
	}
}

func readIDAndFilePath() (int, string, error) {
	id, err := readInt32()
	if err != nil {
		return 0, "", err
	}
	filePath, err := readString()
	if err != nil {
		return 0, "", err
	}
	ods.ODS("  ID: %d / FilePath: %s", id, filePath)
	return id, filePath, nil
}

func readUInt64() (uint64, error) {
	var l uint64
	if err := binary.Read(os.Stdin, binary.LittleEndian, &l); err != nil {
		return 0, err
	}
	return l, nil
}

func readInt32() (int, error) {
	var l uint32
	if err := binary.Read(os.Stdin, binary.LittleEndian, &l); err != nil {
		return 0, err
	}
	return int(int32(l)), nil
}

func readFloat32() (float32, error) {
	var l uint32
	if err := binary.Read(os.Stdin, binary.LittleEndian, &l); err != nil {
		return 0, err
	}
	return math.Float32frombits(l), nil
}

func readBool() (bool, error) {
	i, err := readInt32()
	if err != nil {
		return false, err
	}
	return i != 0, nil
}

func readString() (string, error) {
	l, err := readInt32()
	if err != nil {
		return "", err
	}

	buf := make([]byte, l)
	read, err := io.ReadFull(os.Stdin, buf)
	if err != nil {
		return "", err
	}
	if read != l {
		return "", errors.New("unexcepted read size")
	}
	return string(buf), nil
}

func writeUint64(i uint64) error {
	return binary.Write(os.Stdout, binary.LittleEndian, i)
}

func writeInt32(i int) error {
	return binary.Write(os.Stdout, binary.LittleEndian, int32(i))
}

func writeFloat32(v float32) error {
	return binary.Write(os.Stdout, binary.LittleEndian, math.Float32bits(v))
}

func writeBool(v bool) error {
	if v {
		return writeInt32(1)
	}
	return writeInt32(0)
}

func writeString(s string) error {
	if err := writeInt32(len(s)); err != nil {
		return err
	}
	if _, err := os.Stdout.WriteString(s); err != nil {
		return err
	}
	ods.ODS("  -> String(Len: %d)", len(s))
	return nil
}

func writeReply(err error) error {
	if err == nil {
		return writeInt32(0x80000000)
	}
	s := err.Error()
	if err := writeInt32(len(s) | 0x80000000); err != nil {
		return err
	}
	if _, err := os.Stdout.WriteString(s); err != nil {
		return err
	}
	ods.ODS("  -> Error: %v", err)
	return nil
}

func writeBinary(b []byte) error {
	if err := writeInt32(len(b)); err != nil {
		return err
	}
	if _, err := os.Stdout.Write(b); err != nil {
		return err
	}
	ods.ODS("  -> Binary(Len: %d)", len(b))
	return nil
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

package clipboard

import (
	"bytes"
	"encoding/binary"
	"image"
	"image/draw"
	"image/png"
	"io"
	"syscall"
	"unsafe"

	"golang.org/x/sys/windows"
)

type winBITMAPFILEHEADER struct {
	Type       uint16
	Size       uint32
	Reserved1  uint16
	Reserved2  uint16
	OffsetBits uint32
}
type winBITMAPV5HEADER struct {
	Size          uint32
	Width         int32
	Height        int32
	Planes        uint16
	BitCount      uint16
	Compression   uint32
	SizeImage     uint32
	XPelsPerMeter int32
	YPelsPerMeter int32
	ClrUsed       uint32
	ClrImportant  uint32
	RedMask       uint32
	GreenMask     uint32
	BlueMask      uint32
	AlphaMask     uint32
	CSType        uint32
	Endpoints     struct{ Red, Green, Blue struct{ X, Y, Z int32 } }
	GammaRed      uint32
	GammaGreen    uint32
	GammaBlue     uint32
	Intent        uint32
	ProfileData   uint32
	ProfileSize   uint32
	Reserved      uint32
}

var (
	modkernel32 = windows.NewLazySystemDLL("kernel32")
	moduser32   = windows.NewLazySystemDLL("user32")

	procGlobalAlloc  = modkernel32.NewProc("GlobalAlloc")
	procGlobalFree   = modkernel32.NewProc("GlobalFree")
	procGlobalLock   = modkernel32.NewProc("GlobalLock")
	procGlobalUnlock = modkernel32.NewProc("GlobalUnlock")

	procRegisterClipboardFormat = moduser32.NewProc("RegisterClipboardFormatW")
	procOpenClipboard           = moduser32.NewProc("OpenClipboard")
	procCloseClipboard          = moduser32.NewProc("CloseClipboard")
	procEmptyClipboard          = moduser32.NewProc("EmptyClipboard")
	procSetClipboardData        = moduser32.NewProc("SetClipboardData")
)

func registerClipboardFormat(format string) (uintptr, error) {
	p, err := windows.UTF16PtrFromString(format)
	if err != nil {
		return 0, err
	}
	r1, _, err := procRegisterClipboardFormat.Call(uintptr(unsafe.Pointer(p)))
	if r1 == 0 {
		return 0, err
	}
	return r1, nil
}

func globalAlloc(uFlags uintptr, dwBytes uint32) (uintptr, error) {
	r1, _, err := procGlobalAlloc.Call(uFlags, uintptr(dwBytes))
	if r1 == 0 {
		return 0, err
	}
	return r1, nil
}

func globalFree(hMem uintptr) error {
	r1, _, err := procGlobalFree.Call(hMem)
	if r1 != 0 {
		return err
	}
	return nil
}

func globalLock(hMem uintptr) (uintptr, error) {
	r1, _, err := procGlobalLock.Call(hMem)
	if r1 == 0 {
		return 0, err
	}
	return r1, nil
}

func globalUnlock(hMem uintptr) (uintptr, error) {
	r1, _, err := procGlobalUnlock.Call(hMem)
	if r1 == 0 {
		if e1, ok := err.(syscall.Errno); ok && e1 == 0 {
			return 0, nil
		}
		return 0, err
	}
	return r1, nil
}

func openClipboard(hWndNewOwner uintptr) error {
	r1, _, err := procOpenClipboard.Call(hWndNewOwner)
	if r1 == 0 {
		return err
	}
	return nil
}

func closeClipboard() error {
	r1, _, err := procCloseClipboard.Call()
	if r1 == 0 {
		return err
	}
	return nil
}

func emptyClipboard() error {
	r1, _, err := procEmptyClipboard.Call()
	if r1 == 0 {
		return err
	}
	return nil
}

func setClipboardData(uFormat uintptr, hMem uintptr) (uintptr, error) {
	r1, _, err := procSetClipboardData.Call(uFormat, hMem)
	if r1 == 0 {
		return 0, err
	}
	return r1, nil
}

func setFormatData(uFormat uintptr, data []byte) error {
	hMem, err := globalAlloc(0x0002|0x2000, uint32(len(data))) // GMEM_MOVEABLE | GMEM_SHARE
	if err != nil {
		return err
	}
	defer func() {
		if hMem != 0 {
			globalFree(hMem)
		}
	}()
	ptr, err := globalLock(hMem)
	if err != nil {
		return err
	}
	const maxInt = int(^uint32(0) >> 1)
	copy((*(*[maxInt]byte)(unsafe.Pointer(ptr)))[:len(data)], data)
	_, err = globalUnlock(hMem)
	if err != nil {
		return err
	}
	if _, err = setClipboardData(uFormat, hMem); err != nil {
		return err
	}
	hMem = 0
	return nil
}

func nrgbaToNBGRA(p []byte) {
	for i := 0; i < len(p); i += 4 {
		if p[i+3] > 0 {
			p[i+2], p[i+0] = p[i+0], p[i+2]
		}
	}
}

func writeBin(w io.Writer, data interface{}) {
	err := binary.Write(w, binary.LittleEndian, data)
	if err != nil {
		panic(err)
	}
}

func makeDIB(img image.Image, bitfields bool) []byte {
	nrgba := image.NewNRGBA(img.Bounds())
	draw.Src.Draw(nrgba, nrgba.Rect, img, image.Point{})
	const fhSize = 14
	const hSize = 124
	h := winBITMAPV5HEADER{
		Size:      hSize,
		Width:     int32(nrgba.Rect.Max.X - nrgba.Rect.Min.X),
		Height:    int32(nrgba.Rect.Max.Y - nrgba.Rect.Min.Y),
		Planes:    1,
		BitCount:  32,
		SizeImage: uint32((nrgba.Rect.Max.X - nrgba.Rect.Min.X) * 4 * (nrgba.Rect.Max.Y - nrgba.Rect.Min.Y)),
		RedMask:   0x00ff0000,
		GreenMask: 0x0000ff00,
		BlueMask:  0x000000ff,
		AlphaMask: 0xff000000,
	}
	if bitfields {
		h.Compression = 3 // BI_BITFIELDS
	}
	fh := winBITMAPFILEHEADER{
		Type:       0x4d42,
		Size:       uint32(fhSize + hSize + h.SizeImage),
		Reserved1:  0,
		Reserved2:  0,
		OffsetBits: uint32(fhSize + hSize),
	}
	buf := bytes.NewBuffer([]byte{})
	writeBin(buf, fh.Type)
	writeBin(buf, fh.Size)
	writeBin(buf, fh.Reserved1)
	writeBin(buf, fh.Reserved2)
	writeBin(buf, fh.OffsetBits)
	writeBin(buf, h.Size)
	writeBin(buf, h.Width)
	writeBin(buf, h.Height)
	writeBin(buf, h.Planes)
	writeBin(buf, h.BitCount)
	writeBin(buf, h.Compression)
	writeBin(buf, h.SizeImage)
	writeBin(buf, h.XPelsPerMeter)
	writeBin(buf, h.YPelsPerMeter)
	writeBin(buf, h.ClrUsed)
	writeBin(buf, h.ClrImportant)
	writeBin(buf, h.RedMask)
	writeBin(buf, h.GreenMask)
	writeBin(buf, h.BlueMask)
	writeBin(buf, h.AlphaMask)
	writeBin(buf, h.CSType)
	writeBin(buf, h.Endpoints.Red.X)
	writeBin(buf, h.Endpoints.Red.Y)
	writeBin(buf, h.Endpoints.Red.Z)
	writeBin(buf, h.Endpoints.Green.X)
	writeBin(buf, h.Endpoints.Green.Y)
	writeBin(buf, h.Endpoints.Green.Z)
	writeBin(buf, h.Endpoints.Blue.X)
	writeBin(buf, h.Endpoints.Blue.Y)
	writeBin(buf, h.Endpoints.Blue.Z)
	writeBin(buf, h.GammaRed)
	writeBin(buf, h.GammaGreen)
	writeBin(buf, h.GammaBlue)
	writeBin(buf, h.Intent)
	writeBin(buf, h.ProfileData)
	writeBin(buf, h.ProfileSize)
	writeBin(buf, h.Reserved)
	nrgbaToNBGRA(nrgba.Pix)
	for y := int32(0); y < h.Height; y++ {
		l := h.Width * 4 * (h.Height - y - 1)
		writeBin(buf, nrgba.Pix[l:l+h.Width*4])
	}
	return buf.Bytes()
}

func SetImage(img image.Image) error {
	bmp := makeDIB(img, true)
	buf := bytes.NewBuffer([]byte{})
	if err := png.Encode(buf, img); err != nil {
		return err
	}
	if err := openClipboard(0); err != nil {
		return err
	}
	defer closeClipboard()
	if err := emptyClipboard(); err != nil {
		return err
	}
	pngFmt, err := registerClipboardFormat("PNG")
	if err != nil {
		return err
	}
	if err = setFormatData(pngFmt, buf.Bytes()); err != nil {
		return err
	}
	pngMimeFmt, err := registerClipboardFormat("image/png")
	if err != nil {
		return err
	}
	if err = setFormatData(pngMimeFmt, buf.Bytes()); err != nil {
		return err
	}
	if err := setFormatData(8, bmp[14:]); err != nil { // CF_DIB
		return err
	}
	return nil
}

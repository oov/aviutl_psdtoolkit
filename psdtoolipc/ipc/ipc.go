package ipc

import (
	"context"
	"encoding/binary"
	"fmt"
	"hash/fnv"
	"image"
	"io"
	"os"
	"path/filepath"
	"runtime/debug"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/img"
	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/ods"
	"github.com/oov/psd/blend"
	"github.com/oov/psd/layertree"
)

type sourceImage struct {
	FilePath   *string
	FileHash   uint32
	LastAccess time.Time

	PSD *layertree.Root

	InitialVisibility *string
	ForceChecked      map[int]struct{} // map[SeqID]
	Group             map[int][]int    // map[SeqID][]SeqID
}

type IPC struct {
	ShowGUI func() (uintptr, error)

	sourceImages map[string]*sourceImage

	hotImages  map[StateKey]*img.Image
	usingInGUI StateKey

	queue     chan func()
	reply     chan error
	replyDone chan struct{}

	// these are accessed from another goroutine
	hotImageList []string
	hotImageKeys StateKeys
	m            sync.RWMutex
}

type StateKey struct {
	ID       int
	FilePath string
}

type StateKeys []StateKey

func (a StateKeys) Len() int      { return len(a) }
func (a StateKeys) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a StateKeys) Less(i, j int) bool {
	if a[i].FilePath == a[j].FilePath {
		return a[i].ID < a[j].ID
	}
	return filepath.Base(a[i].FilePath) < filepath.Base(a[j].FilePath)
}

func (ipc *IPC) do(f func()) {
	done := make(chan struct{})
	ipc.queue <- func() {
		f()
		done <- struct{}{}
	}
	<-done
}

func (ipc *IPC) ImageList() ([]string, StateKeys) {
	ipc.m.RLock()
	il := ipc.hotImageList
	k := ipc.hotImageKeys
	ipc.m.RUnlock()
	return il, k
}

func (ipc *IPC) Image(id int, filePath string) (*img.Image, error) {
	var r img.Image
	var err error
	ipc.do(func() {
		var ro *img.Image
		ro, err = ipc.load(id, filePath)
		if err == nil {
			r = *ro
			r.PSD = ro.PSD.Clone()
			r.Layers = img.NewLayerManager(r.PSD)
			r.Scale = 1
			ipc.usingInGUI = StateKey{id, filePath}
		}
	})
	if err != nil {
		return nil, err
	}
	return &r, nil
}

func (ipc *IPC) updateImageList() {
	var keys StateKeys
	for k := range ipc.hotImages {
		keys = append(keys, k)
	}
	sort.Sort(keys)
	s := make([]string, len(keys))
	for i, key := range keys {
		s[i] = "ID: " + itoa(key.ID) + " / " + filepath.Base(key.FilePath)
	}
	ipc.m.Lock()
	ipc.hotImageList = s
	ipc.hotImageKeys = keys
	ipc.m.Unlock()
}

func (ipc *IPC) getSourceImage(filePath string) (*sourceImage, error) {
	if r, ok := ipc.sourceImages[filePath]; ok {
		r.LastAccess = time.Now()
		return r, nil
	}

	ods.ODS("psd loading: %s", filePath)
	s := time.Now().UnixNano()
	f, err := os.Open(filePath)
	if err != nil {
		return nil, errors.Wrap(err, "ipc: cannot open the file")
	}
	defer f.Close()

	hash := fnv.New32a()
	if _, err = io.Copy(hash, f); err != nil {
		return nil, errors.Wrap(err, "ipc: hash calculation failed")
	}
	if _, err = f.Seek(0, os.SEEK_SET); err != nil {
		return nil, errors.Wrap(err, "ipc: cannot seek")
	}

	psd, err := layertree.New(context.Background(), f, &layertree.Options{
		LayerNameEncodingDetector: autoDetect,
	})
	if err != nil {
		return nil, errors.Wrap(err, "ipc: could not build the layer tree.")
	}
	ods.ODS("psd loading: %dms", (time.Now().UnixNano()-s)/1e6)

	lm := img.NewLayerManager(psd)
	lm.Normalize()
	state := lm.SerializeVisibility()
	srcImage := &sourceImage{
		FilePath:   &filePath,
		FileHash:   hash.Sum32(),
		LastAccess: time.Now(),

		PSD: psd,

		InitialVisibility: &state,
	}
	ipc.sourceImages[filePath] = srcImage
	debug.FreeOSMemory()
	return srcImage, nil
}

func (ipc *IPC) load(id int, filePath string) (*img.Image, error) {
	if himg, ok := ipc.hotImages[StateKey{id, filePath}]; ok {
		n := time.Now()
		ipc.sourceImages[filePath].LastAccess = n
		himg.LastAccess = n
		return himg, nil
	}

	r, err := ipc.getSourceImage(filePath)
	if err != nil {
		return nil, err
	}
	psd := r.PSD.Clone()
	himg := &img.Image{
		FilePath:   r.FilePath,
		FileHash:   r.FileHash,
		LastAccess: time.Now(),

		PSD:    psd,
		Layers: img.NewLayerManager(psd),

		InitialVisibility: r.InitialVisibility,

		Scale: 1,
	}
	ipc.hotImages[StateKey{id, filePath}] = himg
	ipc.updateImageList()
	return himg, nil
}

func (ipc *IPC) draw(id int, filePath string, width, height int) ([]byte, error) {
	himg, err := ipc.load(id, filePath)
	if err != nil {
		return nil, errors.Wrap(err, "ipc: could not load")
	}
	rgba, err := himg.Render(context.Background())
	if err != nil {
		return nil, errors.Wrap(err, "ipc: could not load")
	}
	ret := image.NewRGBA(image.Rect(0, 0, width, height))
	blend.Copy.Draw(ret, ret.Rect, rgba, image.Pt(-himg.OffsetX, -himg.OffsetY))
	rgbaToNBGRA(ret.Pix)
	debug.FreeOSMemory()
	return ret.Pix, nil
}

func (ipc *IPC) getLayerNames(id int, filePath string) (string, error) {
	img, err := ipc.load(id, filePath)
	if err != nil {
		return "", errors.Wrap(err, "ipc: could not load")
	}
	s := make([]string, len(img.Layers.Flat))
	for path, index := range img.Layers.FullPath {
		s[index] = path
	}
	return strings.Join(s, "\n"), nil
}

func (ipc *IPC) setProps(id int, filePath string, layers *string, scale *float32, offsetX, offsetY *int) (bool, int, int, error) {
	himg, err := ipc.load(id, filePath)
	if err != nil {
		return false, 0, 0, errors.Wrap(err, "ipc: could not load")
	}
	modified := himg.Modified
	if layers != nil {
		if *layers == "" {
			layers = himg.InitialVisibility
		}
		b, err := himg.Layers.DeserializeVisibility(*layers)
		if err != nil {
			return false, 0, 0, errors.Wrap(err, "ipc: deserialize failed")
		}
		if b {
			modified = true
		}
	}
	if scale != nil {
		if *scale > 1 {
			*scale = 1
		} else if *scale < 0.00001 {
			*scale = 0.00001
		}
		if *scale != himg.Scale {
			himg.Scale = *scale
			modified = true
		}
	}
	if offsetX != nil {
		if *offsetX != himg.OffsetX {
			himg.OffsetX = *offsetX
			modified = true
		}
	}
	if offsetY != nil {
		if *offsetY != himg.OffsetY {
			himg.OffsetY = *offsetY
			modified = true
		}
	}
	r := himg.ScaledCanvasRect()
	himg.Modified = modified
	return modified, r.Dx(), r.Dy(), nil
}

func (ipc *IPC) showGUI() (uintptr, error) {
	h, err := ipc.ShowGUI()
	if err != nil {
		return 0, errors.Wrap(err, "ipc: cannot show the gui")
	}
	return h, nil
}

func (ipc *IPC) SendEditingImageState(s *img.ImageState) error {
	var err error
	ipc.do(func() {
		fmt.Print("EDIS")
		ods.ODS("  FilePath: %s / FileHash: %08x / Scale: %f / OffsetX: %d / OffsetY: %d / State: %s", s.FilePath, s.FileHash, s.Scale, s.OffsetX, s.OffsetY, s.Layer)
		if err = writeString(s.FilePath); err != nil {
			return
		}
		if err = writeInt32(int(int32(s.FileHash))); err != nil {
			return
		}
		if err = writeFloat32(s.Scale); err != nil {
			return
		}
		if err = writeInt32(s.OffsetX); err != nil {
			return
		}
		if err = writeInt32(s.OffsetY); err != nil {
			return
		}
		if err = writeString(s.Layer); err != nil {
			return
		}
	})
	if err != nil {
		return err
	}
	ods.ODS("wait EDIS reply...")
	err = <-ipc.reply
	ods.ODS("wait EDIS reply ok")
	ipc.replyDone <- struct{}{}
	return err
}

func (ipc *IPC) dispatch(cmd string) error {
	switch cmd {
	case "HELO":
		fmt.Print("HELO")
		return nil

	case "DRAW":
		id, filePath, err := readIDAndFilePath()
		if err != nil {
			return err
		}
		width, err := readInt32()
		if err != nil {
			return err
		}
		height, err := readInt32()
		if err != nil {
			return err
		}
		ods.ODS("  Width: %d / Height: %d", width, height)
		b, err := ipc.draw(id, filePath, width, height)
		if err != nil {
			return err
		}
		if err = writeInt32(0x80000000); err != nil {
			return err
		}
		return writeBinary(b)

	case "LNAM":
		id, filePath, err := readIDAndFilePath()
		if err != nil {
			return err
		}
		s, err := ipc.getLayerNames(id, filePath)
		if err != nil {
			return err
		}
		if err = writeInt32(0x80000000); err != nil {
			return err
		}
		return writeString(s)

	case "PROP":
		id, filePath, err := readIDAndFilePath()
		if err != nil {
			return err
		}
		const (
			propEnd = iota
			propLayers
			propScale
			propOffsetX
			propOffsetY
		)
		var layers *string
		var scale *float32
		var offsetX, offsetY *int
	readProps:
		for {
			pid, err := readInt32()
			if err != nil {
				return err
			}
			switch pid {
			case propEnd:
				break readProps
			case propLayers:
				s, err := readString()
				if err != nil {
					return err
				}
				layers = &s
				ods.ODS("  Layers: %s", s)
			case propScale:
				f, err := readFloat32()
				if err != nil {
					return err
				}
				scale = &f
				ods.ODS("  Scale: %f", f)
			case propOffsetX:
				i, err := readInt32()
				if err != nil {
					return err
				}
				offsetX = &i
				ods.ODS("  OffsetX: %d", i)
			case propOffsetY:
				i, err := readInt32()
				if err != nil {
					return err
				}
				offsetY = &i
				ods.ODS("  OffsetY: %d", i)
			}
		}
		modified, width, height, err := ipc.setProps(id, filePath, layers, scale, offsetX, offsetY)
		if err != nil {
			return err
		}
		ods.ODS("  Modified: %v / Width: %d / Height: %d", modified, width, height)
		if err = writeInt32(0x80000000); err != nil {
			return err
		}
		if err = writeBool(modified); err != nil {
			return err
		}
		if err = writeInt32(width); err != nil {
			return err
		}
		return writeInt32(height)

	case "SGUI":
		h, err := ipc.showGUI()
		if err != nil {
			return err
		}
		if err = writeInt32(0x80000000); err != nil {
			return err
		}
		return writeUint64(uint64(h))
	}
	return errors.New("unknown command")
}

func (ipc *IPC) gc() {
	const deadline = 10 * time.Minute
	now := time.Now()

	if using, ok := ipc.hotImages[ipc.usingInGUI]; ok {
		using.LastAccess = now
	}

	used := make(map[string]struct{})
	for k, v := range ipc.hotImages {
		if now.Sub(v.LastAccess) > deadline {
			delete(ipc.hotImages, k)
		} else {
			used[k.FilePath] = struct{}{}
		}
	}
	for k, v := range ipc.sourceImages {
		if _, ok := used[k]; ok {
			continue
		}
		if now.Sub(v.LastAccess) > deadline {
			delete(ipc.sourceImages, k)
		}
	}
	ipc.updateImageList()
}

func (ipc *IPC) Init() {
	ipc.sourceImages = map[string]*sourceImage{}
	ipc.hotImages = map[StateKey]*img.Image{}
	ipc.queue = make(chan func())
	ipc.reply = make(chan error)
	ipc.replyDone = make(chan struct{})
}

func (ipc *IPC) readCommand(r chan string) {
	cmd := make([]byte, 4)
	for {
		ods.ODS("wait next command...")
		if read, err := io.ReadFull(os.Stdin, cmd); err != nil || read != 4 {
			r <- fmt.Sprintf("error: %v", err)
			return
		}
		l := binary.LittleEndian.Uint32(cmd)
		if l&0x80000000 == 0 {
			break
		}
		l &= 0x7fffffff
		if l == 0 {
			ods.ODS("readCommand: reply no error")
			ipc.reply <- nil
		} else {
			buf := make([]byte, l)
			read, err := io.ReadFull(os.Stdin, buf)
			if err != nil {
				r <- fmt.Sprintf("error: %v", err)
				return
			}
			if read != int(l) {
				r <- fmt.Sprintf("error: %v", errors.New("unexcepted read size"))
				return
			}
			ods.ODS("readCommand: reply %s", string(buf))
			ipc.reply <- errors.New(string(buf))
		}
		<-ipc.replyDone
	}
	ods.ODS("readCommand: cmd %s", string(cmd))
	r <- string(cmd)
}

func (ipc *IPC) Main(exitCh chan<- struct{}) {
	gcTicker := time.NewTicker(1 * time.Minute)
	defer func() {
		if err := recover(); err != nil {
			ods.Recover(err)
		}
		gcTicker.Stop()
		close(exitCh)
	}()

	cmdCh := make(chan string)
	go ipc.readCommand(cmdCh)
	for {
		select {
		case <-gcTicker.C:
			ipc.gc()
		case f := <-ipc.queue:
			f()
		case cmd := <-cmdCh:
			if len(cmd) != 4 {
				ods.ODS("%s", cmd) // error report
				return
			}
			ods.ODS("%s", cmd)
			if err := ipc.dispatch(cmd); err != nil {
				ods.ODS("error: %v", err)
				if err = writeReply(err); err != nil {
					return
				}
			}
			ods.ODS("%s END", cmd)
			go ipc.readCommand(cmdCh)
		}
	}
}

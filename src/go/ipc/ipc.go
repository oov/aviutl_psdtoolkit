package ipc

import (
	"context"
	"encoding/binary"
	"fmt"
	"hash/fnv"
	"image"
	"io"
	"math"
	"os"
	"strings"
	"time"

	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/source"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/temporary"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
	"github.com/oov/psd/blend"
)

type cacheKey struct {
	Width   int
	Height  int
	OffsetX int
	OffsetY int
	Scale   float32
	Path    string
	State   string
}

func (k *cacheKey) Hash() uint32 {
	h := fnv.New32a()
	if _, err := io.WriteString(h, k.Path); err != nil {
		panic(err)
	}
	if err := binary.Write(h, binary.LittleEndian, uint32(k.Width)); err != nil {
		panic(err)
	}
	if err := binary.Write(h, binary.LittleEndian, uint32(k.Height)); err != nil {
		panic(err)
	}
	if err := binary.Write(h, binary.LittleEndian, int32(k.OffsetX)); err != nil {
		panic(err)
	}
	if err := binary.Write(h, binary.LittleEndian, int32(k.OffsetY)); err != nil {
		panic(err)
	}
	if err := binary.Write(h, binary.LittleEndian, math.Float32bits(k.Scale)); err != nil {
		panic(err)
	}
	if _, err := io.WriteString(h, k.State); err != nil {
		panic(err)
	}
	return h.Sum32()
}

type cacheValue struct {
	LastAccess time.Time
	Data       []byte
}

type IPC struct {
	AddFile            func(file string, tag int) error
	AddFileIfNotExists func(file string, tag int, state string) error
	ClearFiles         func() error
	ShowGUI            func() (uintptr, error)
	Serialize          func() (string, error)
	Deserialize        func(state string) error
	GCing              func()

	tmpImg temporary.Temporary
	cache  map[cacheKey]cacheValue

	queue     chan func()
	reply     chan error
	replyDone chan struct{}
}

func (ipc *IPC) do(f func()) {
	done := make(chan struct{})
	ipc.queue <- func() {
		f()
		done <- struct{}{}
	}
	<-done
}

func (ipc *IPC) load(id int, filePath string) (*img.Image, error) {
	return ipc.tmpImg.Load(id, filePath)
}

func (ipc *IPC) draw(id int, filePath string, width, height int) ([]byte, error) {
	img, err := ipc.tmpImg.Load(id, filePath)
	if err != nil {
		return nil, errors.Wrap(err, "ipc: could not load")
	}
	state, err := img.Serialize()
	if err != nil {
		return nil, errors.Wrap(err, "ipc: could not serialize state")
	}

	ckey := cacheKey{
		Width:   width,
		Height:  height,
		OffsetX: img.OffsetX,
		OffsetY: img.OffsetY,
		Scale:   img.Scale,
		Path:    filePath,
		State:   state,
	}
	if cv, ok := ipc.cache[ckey]; ok {
		cv.LastAccess = time.Now()
		ipc.cache[ckey] = cv
		ipc.tmpImg.Srcs.Logger.Println("cached")
		return cv.Data, nil
	}

	startAt := time.Now().UnixNano()
	nrgba, err := img.Render(context.Background())
	if err != nil {
		return nil, errors.Wrap(err, "ipc: could not render")
	}
	ret := image.NewNRGBA(image.Rect(0, 0, width, height))
	blend.Copy.Draw(ret, ret.Rect, nrgba, image.Pt(int(float32(-img.OffsetX)*img.Scale), int(float32(-img.OffsetY)*img.Scale)))
	nrgbaToNBGRA(ret.Pix)
	ipc.cache[ckey] = cacheValue{
		LastAccess: time.Now(),
		Data:       ret.Pix,
	}
	ipc.tmpImg.Srcs.Logger.Println(fmt.Sprintf("render: %dms", (time.Now().UnixNano()-startAt)/1e6))
	return ret.Pix, nil
}

func (ipc *IPC) getLayerNames(id int, filePath string) (string, error) {
	img, err := ipc.tmpImg.Load(id, filePath)
	if err != nil {
		return "", errors.Wrap(err, "ipc: could not load")
	}
	s := make([]string, len(img.Layers.Flat))
	for path, index := range img.Layers.FullPath {
		s[index] = path
	}
	return strings.Join(s, "\n"), nil
}

func (ipc *IPC) setProps(id int, filePath string, tag *int, layer *string, scale *float32, offsetX, offsetY *int) (bool, uint32, int, int, error) {
	img, err := ipc.tmpImg.Load(id, filePath)
	if err != nil {
		return false, 0, 0, 0, errors.Wrap(err, "ipc: could not load")
	}
	modified := img.Modified
	if layer != nil {
		if *layer != "" {
			l := *img.InitialLayerState + " " + *layer
			layer = &l
		} else {
			layer = img.InitialLayerState
		}
		b, err := img.Deserialize(*layer)
		if err != nil {
			return false, 0, 0, 0, errors.Wrap(err, "ipc: deserialize failed")
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
		if *scale != img.Scale {
			img.Scale = *scale
			modified = true
		}
	}
	if offsetX != nil {
		if *offsetX != img.OffsetX {
			img.OffsetX = *offsetX
			modified = true
		}
	}
	if offsetY != nil {
		if *offsetY != img.OffsetY {
			img.OffsetY = *offsetY
			modified = true
		}
	}
	r := img.ScaledCanvasRect()
	img.Modified = modified

	state, err := img.Serialize()
	if err != nil {
		return false, 0, 0, 0, errors.Wrap(err, "ipc: could not serialize state")
	}

	if tag != nil && *tag != 0 {
		go func() {
			ipc.AddFileIfNotExists(filePath, *tag, state)
		}()
	}

	ckey := (&cacheKey{
		Width:   r.Dx(),
		Height:  r.Dy(),
		OffsetX: img.OffsetX,
		OffsetY: img.OffsetY,
		Scale:   img.Scale,
		Path:    filePath,
		State:   state,
	}).Hash()

	return modified, ckey, r.Dx(), r.Dy(), nil
}

func (ipc *IPC) SendEditingImageState(filePath, state string) error {
	var err error
	ipc.do(func() {
		fmt.Print("EDIS")
		ods.ODS("  FilePath: %s", filePath)
		if err = writeString(filePath); err != nil {
			return
		}
		ods.ODS("  State: %s", state)
		if err = writeString(state); err != nil {
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

func (ipc *IPC) ExportFaviewSlider(filePath, sliderName string, names, values []string, selectedIndex int) error {
	var err error
	ipc.do(func() {
		fmt.Print("EXFS")
		ods.ODS("  FilePath: %s", filePath)
		if err = writeString(filePath); err != nil {
			return
		}
		ods.ODS("  SliderName: %s / Names: %v / Values: %v", sliderName, names, values)
		if err = writeString(sliderName); err != nil {
			return
		}
		if err = writeString(strings.Join(names, "\x00")); err != nil {
			return
		}
		if err = writeString(strings.Join(values, "\x00")); err != nil {
			return
		}
		if err = writeInt32(int32(selectedIndex)); err != nil {
			return
		}
	})
	if err != nil {
		return err
	}
	ods.ODS("wait EXFS reply...")
	err = <-ipc.reply
	ods.ODS("wait EXFS reply ok")
	ipc.replyDone <- struct{}{}
	return err
}

func (ipc *IPC) ExportLayerNames(filePath string, names, values []string, selectedIndex int) error {
	var err error
	ipc.do(func() {
		fmt.Print("EXLN")
		ods.ODS("  FilePath: %s", filePath)
		if err = writeString(filePath); err != nil {
			return
		}
		if err = writeString(strings.Join(names, "\x00")); err != nil {
			return
		}
		if err = writeString(strings.Join(values, "\x00")); err != nil {
			return
		}
		if err = writeInt32(int32(selectedIndex)); err != nil {
			return
		}
	})
	if err != nil {
		return err
	}
	ods.ODS("wait EXLN reply...")
	err = <-ipc.reply
	ods.ODS("wait EXLN reply ok")
	ipc.replyDone <- struct{}{}
	return err
}

func (ipc *IPC) Abort(err error) {
	ipc.queue <- nil
	writeReply(err)
}

func (ipc *IPC) dispatch(cmd string) error {
	switch cmd {
	case "HELO":
		return writeUint32(0x80000000)

	case "ADDF":
		file, err := readString()
		if err != nil {
			return err
		}
		tag, err := readUInt32()
		if err != nil {
			return err
		}
		if err = ipc.AddFile(file, tag); err != nil {
			return err
		}
		return writeUint32(0x80000000)

	case "CLRF":
		if err := ipc.ClearFiles(); err != nil {
			return err
		}
		return writeUint32(0x80000000)

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
		if err = writeUint32(0x80000000); err != nil {
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
		if err = writeUint32(0x80000000); err != nil {
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
			propLayer
			propScale
			propOffsetX
			propOffsetY
			propTag
		)
		var tag *int
		var layer *string
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
			case propTag:
				ui, err := readUInt32()
				if err != nil {
					return err
				}
				tag = &ui
				ods.ODS("  Tag: %d", ui)
			case propLayer:
				s, err := readString()
				if err != nil {
					return err
				}
				layer = &s
				ods.ODS("  Layer: %s", s)
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
		modified, ckey, width, height, err := ipc.setProps(id, filePath, tag, layer, scale, offsetX, offsetY)
		if err != nil {
			return err
		}
		ods.ODS("  Modified: %v / CacheKey: %v / Width: %d / Height: %d", modified, ckey, width, height)
		if err = writeUint32(0x80000000); err != nil {
			return err
		}
		if err = writeBool(modified); err != nil {
			return err
		}
		if err = writeUint32(ckey); err != nil {
			return err
		}
		if err = writeUint32(uint32(width)); err != nil {
			return err
		}
		return writeUint32(uint32(height))

	case "SGUI":
		h, err := ipc.ShowGUI()
		if err != nil {
			return errors.Wrap(err, "ipc: cannot show the gui")
		}
		if err = writeUint32(0x80000000); err != nil {
			return err
		}
		return writeUint64(uint64(h))

	case "SRLZ":
		s, err := ipc.Serialize()
		if err != nil {
			return errors.Wrap(err, "ipc: cannot serialize")
		}
		if err = writeUint32(0x80000000); err != nil {
			return err
		}
		return writeString(s)

	case "DSLZ":
		s, err := readString()
		if err != nil {
			return err
		}
		err = ipc.Deserialize(s)
		if err != nil {
			return errors.Wrap(err, "ipc: cannot deserialize")
		}
		if err = writeUint32(0x80000000); err != nil {
			return err
		}
		return writeBool(true)
	}
	return errors.New("unknown command")
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

func (ipc *IPC) gc() {
	const deadline = 1 * time.Minute
	now := time.Now()

	for k, v := range ipc.cache {
		if now.Sub(v.LastAccess) > deadline {
			delete(ipc.cache, k)
		}
	}
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
			ipc.GCing()
			ipc.tmpImg.GC()
			ipc.tmpImg.Srcs.GC()
			ipc.gc()
		case f := <-ipc.queue:
			if f == nil {
				return
			}
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

func New(srcs *source.Sources) *IPC {
	r := &IPC{
		tmpImg: temporary.Temporary{Srcs: srcs},
		cache:  map[cacheKey]cacheValue{},

		queue:     make(chan func()),
		reply:     make(chan error),
		replyDone: make(chan struct{}),
	}
	return r
}

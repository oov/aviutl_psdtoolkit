package ipc

import (
	"bytes"
	"context"
	"encoding/binary"
	"encoding/json"
	"fmt"
	"hash/fnv"
	"image"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
	"github.com/oov/psd/blend"
	"github.com/oov/psd/composite"
)

type sourceImage struct {
	FilePath   *string
	FileHash   uint32
	LastAccess time.Time

	PSD *composite.Tree
	PFV *img.PFV

	InitialLayerState *string
	ForceChecked      map[int]struct{} // map[SeqID]
	Group             map[int][]int    // map[SeqID][]SeqID
}

type IPC struct {
	ShowGUI           func() (uintptr, error)
	OpenImagesChanged func()

	sourceImages map[string]*sourceImage
	fileSeqID    int

	hotImages map[StateKey]*img.Image

	queue     chan func()
	reply     chan error
	replyDone chan struct{}
}

type StateKey struct {
	ID2         int
	AddedByUser bool
	FilePath    string
}

func (k *StateKey) String() string {
	return fmt.Sprintf("ID: %06d / %s", k.ID2, filepath.Base(k.FilePath))
}

type StateKeys []StateKey

func (a StateKeys) Len() int      { return len(a) }
func (a StateKeys) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a StateKeys) Less(i, j int) bool {
	if a[i].ID2 == a[j].ID2 {
		return filepath.Base(a[i].FilePath) < filepath.Base(a[j].FilePath)
	}
	return a[i].ID2 < a[j].ID2
}

func (ipc *IPC) do(f func()) {
	done := make(chan struct{})
	ipc.queue <- func() {
		f()
		done <- struct{}{}
	}
	<-done
}

func (ipc *IPC) Image(id int, filePath string) (*img.Image, error) {
	var r *img.Image
	var err error
	ipc.do(func() {
		if id == -1 {
			r, err = ipc.load(ipc.fileSeqID, true, filePath)
			if err == nil {
				ipc.fileSeqID++
			}
		} else {
			r, err = ipc.load(id, true, filePath)
		}
	})
	if err != nil {
		return nil, err
	}
	return r, nil
}

func (ipc *IPC) BuildImageList() StateKeys {
	var keys StateKeys
	for k := range ipc.hotImages {
		if k.AddedByUser {
			keys = append(keys, k)
		}
	}
	sort.Sort(keys)
	return keys
}

func (ipc *IPC) getSourceImage(filePath string) (*sourceImage, error) {
	if r, ok := ipc.sourceImages[filePath]; ok {
		r.LastAccess = time.Now()
		return r, nil
	}

	files := strings.SplitN(filePath, "|", 2)
	ods.ODS("psd loading: %s", files[0])
	s := time.Now().UnixNano()
	f, err := os.Open(files[0])
	if err != nil {
		return nil, errors.Wrap(err, "ipc: cannot open the image file")
	}
	defer f.Close()

	hash := fnv.New32a()
	if _, err = io.Copy(hash, f); err != nil {
		return nil, errors.Wrap(err, "ipc: hash calculation failed")
	}
	if _, err = f.Seek(0, os.SEEK_SET); err != nil {
		return nil, errors.Wrap(err, "ipc: cannot seek")
	}

	root, err := composite.New(context.Background(), f, &composite.Options{
		LayerNameEncodingDetector: autoDetect,
	})
	if err != nil {
		return nil, errors.Wrap(err, "ipc: could not build the layer tree.")
	}
	ods.ODS("psd loading: %dms", (time.Now().UnixNano()-s)/1e6)

	lm := img.NewLayerManager(root)

	var pf *img.PFV
	if len(files) > 1 {
		f2, err := os.Open(filepath.Join(filepath.Dir(files[0]), files[1]))
		if err != nil {
			return nil, errors.Wrap(err, "ipc: cannot open the pfv file")
		}
		defer f2.Close()
		pf, err = img.NewPFV(f2, lm)
		if err != nil {
			return nil, errors.Wrap(err, "ipc: cannot parse the pfv file")
		}
	}

	lm.Normalize(img.FlipNone)
	state := lm.Serialize()
	srcImage := &sourceImage{
		FilePath:   &filePath,
		FileHash:   hash.Sum32(),
		LastAccess: time.Now(),

		PSD: root,
		PFV: pf,

		InitialLayerState: &state,
	}
	ipc.sourceImages[filePath] = srcImage
	return srcImage, nil
}

func (ipc *IPC) load(id int, addedByUser bool, filePath string) (*img.Image, error) {
	if himg, ok := ipc.hotImages[StateKey{id, addedByUser, filePath}]; ok {
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

		InitialLayerState: r.InitialLayerState,

		Scale: 1,

		PFV: r.PFV,
	}
	ipc.hotImages[StateKey{id, addedByUser, filePath}] = himg
	return himg, nil
}

func (ipc *IPC) draw(id int, addedByUser bool, filePath string, width, height int) ([]byte, error) {
	himg, err := ipc.load(id, addedByUser, filePath)
	if err != nil {
		return nil, errors.Wrap(err, "ipc: could not load")
	}
	s := time.Now().UnixNano()
	rgba, err := himg.Render(context.Background())
	if err != nil {
		return nil, errors.Wrap(err, "ipc: could not render")
	}
	ret := image.NewRGBA(image.Rect(0, 0, width, height))
	blend.Copy.Draw(ret, ret.Rect, rgba, image.Pt(int(float32(-himg.OffsetX)*himg.Scale), int(float32(-himg.OffsetY)*himg.Scale)))
	rgbaToNBGRA(ret.Pix)
	ods.ODS("render: %dms", (time.Now().UnixNano()-s)/1e6)
	return ret.Pix, nil
}

func (ipc *IPC) getLayerNames(id int, addedByUser bool, filePath string) (string, error) {
	img, err := ipc.load(id, addedByUser, filePath)
	if err != nil {
		return "", errors.Wrap(err, "ipc: could not load")
	}
	s := make([]string, len(img.Layers.Flat))
	for path, index := range img.Layers.FullPath {
		s[index] = path
	}
	return strings.Join(s, "\n"), nil
}

func (ipc *IPC) setProps(id int, addedByUser bool, filePath string, layer *string, scale *float32, offsetX, offsetY *int) (bool, int, int, error) {
	himg, err := ipc.load(id, addedByUser, filePath)
	if err != nil {
		return false, 0, 0, errors.Wrap(err, "ipc: could not load")
	}
	modified := himg.Modified
	if layer != nil {
		if *layer != "" {
			l := *himg.InitialLayerState + " " + *layer
			layer = &l
		} else {
			layer = himg.InitialLayerState
		}
		b, err := himg.Deserialize(*layer)
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

type serializedImage struct {
	FilePath string
	State    string
}

type serializeData struct {
	Images []serializedImage
}

func (ipc *IPC) serialize() (string, error) {
	var keys StateKeys
	for k := range ipc.hotImages {
		if k.AddedByUser {
			keys = append(keys, k)
		}
	}
	sort.Sort(keys)
	// TODO: layer open/close state, fav open/close state, faview selected item state
	var srz serializeData
	for _, k := range keys {
		srz.Images = append(srz.Images, serializedImage{
			FilePath: k.FilePath,
			State:    ipc.hotImages[k].Serialize(),
		})
	}
	b := bytes.NewBufferString("")
	if err := json.NewEncoder(b).Encode(srz); err != nil {
		return "", err
	}
	return b.String(), nil
}

func (ipc *IPC) deserialize(s string) error {
	// TODO: close editing images
	if s == "" {
		return nil
	}
	var srz serializeData
	if err := json.NewDecoder(bytes.NewReader([]byte(s))).Decode(&srz); err != nil {
		return err
	}
	for index, simg := range srz.Images {
		img, err := ipc.load(index, true, simg.FilePath)
		if err != nil {
			return err
		}
		if _, err = img.Deserialize(simg.State); err != nil {
			return err
		}
	}
	ipc.fileSeqID = len(srz.Images)
	ipc.OpenImagesChanged()
	return nil
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

func (ipc *IPC) CopyFaviewValue(filePath, sliderName, name, value string) error {
	var err error
	ipc.do(func() {
		fmt.Print("CPFV")
		ods.ODS("  FilePath: %s", filePath)
		if err = writeString(filePath); err != nil {
			return
		}
		ods.ODS("  SliderName: %s / Name: %s / Value: %s", sliderName, name, value)
		if err = writeString(sliderName); err != nil {
			return
		}
		if err = writeString(name); err != nil {
			return
		}
		if err = writeString(value); err != nil {
			return
		}
	})
	if err != nil {
		return err
	}
	ods.ODS("wait CPFV reply...")
	err = <-ipc.reply
	ods.ODS("wait CPFV reply ok")
	ipc.replyDone <- struct{}{}
	return err
}

func (ipc *IPC) ExportFaviewSlider(filePath, sliderName string, names, values []string) error {
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

func (ipc *IPC) dispatch(cmd string) error {
	switch cmd {
	case "HELO":
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
		b, err := ipc.draw(id, false, filePath, width, height)
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
		s, err := ipc.getLayerNames(id, false, filePath)
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
		)
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
		modified, width, height, err := ipc.setProps(id, false, filePath, layer, scale, offsetX, offsetY)
		if err != nil {
			return err
		}
		ods.ODS("  Modified: %v / Width: %d / Height: %d", modified, width, height)
		if err = writeUint32(0x80000000); err != nil {
			return err
		}
		if err = writeBool(modified); err != nil {
			return err
		}
		if err = writeUint32(uint32(width)); err != nil {
			return err
		}
		return writeUint32(uint32(height))

	case "SGUI":
		h, err := ipc.showGUI()
		if err != nil {
			return err
		}
		if err = writeUint32(0x80000000); err != nil {
			return err
		}
		return writeUint64(uint64(h))

	case "SRLZ":
		s, err := ipc.serialize()
		if err != nil {
			return err
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
		err = ipc.deserialize(s)
		if err != nil {
			return err
		}
		if err = writeUint32(0x80000000); err != nil {
			return err
		}
		return writeBool(true)
	}
	return errors.New("unknown command")
}

func (ipc *IPC) gc() {
	const deadline = 10 * time.Minute
	now := time.Now()

	used := make(map[string]struct{})
	for k, v := range ipc.hotImages {
		if k.AddedByUser {
			continue
		}
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

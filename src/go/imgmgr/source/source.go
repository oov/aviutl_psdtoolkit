package source

import (
	"context"
	"fmt"
	"hash/fnv"
	"io"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/warn"
	"github.com/oov/psd/composite"
)

// Logger is the subset of log.Logger.
type Logger interface {
	Println(a ...interface{})
}

// Source keeps initial state of the image.
//
// When editing an image, do it on the Image instance that duplicated Source.
type Source struct {
	m          sync.Mutex
	lastAccess time.Time

	FilePath string
	FileHash uint32

	PSD *composite.Tree
	PFV *img.PFV

	InitialLayerState string
}

func (src *Source) Touch() {
	src.m.Lock()
	src.lastAccess = time.Now()
	src.m.Unlock()
}

func (src *Source) LastAccess() time.Time {
	src.m.Lock()
	lastAccess := src.lastAccess
	src.m.Unlock()
	return lastAccess
}

// Sources has a map from filePath to Source.
type Sources struct {
	m      sync.Mutex
	srcs   map[string]*Source
	Logger Logger
}

func (s *Sources) load(filePath string) (*Source, error) {
	files := strings.SplitN(filePath, "|", 2)
	startAt := time.Now().UnixNano()
	f, err := os.Open(files[0])
	if err != nil {
		return nil, errors.Wrap(err, "source: cannot open the image file")
	}
	defer f.Close()

	hash := fnv.New32a()
	if _, err = io.Copy(hash, f); err != nil {
		return nil, errors.Wrap(err, "source: hash calculation failed")
	}
	if _, err = f.Seek(0, os.SEEK_SET); err != nil {
		return nil, errors.Wrap(err, "source: cannot seek")
	}

	root, err := composite.New(context.Background(), f, &composite.Options{
		LayerNameEncodingDetector: autoDetect,
	})
	if err != nil {
		return nil, errors.Wrap(err, "source: could not build the layer tree.")
	}
	if s.Logger != nil {
		s.Logger.Println(fmt.Sprintf("psd loading: %dms %q", (time.Now().UnixNano()-startAt)/1e6, files[0]))
	}

	lm := img.NewLayerManager(root)

	var pf *img.PFV
	var wr warn.Warning
	if len(files) > 1 {
		f2, err := os.Open(filepath.Join(filepath.Dir(files[0]), files[1]))
		if err != nil {
			return nil, errors.Wrap(err, "source: cannot open the pfv file")
		}
		defer f2.Close()
		pf, wr, err = img.NewPFV(f2, lm)
		if err != nil {
			return nil, errors.Wrap(err, "source: cannot parse the pfv file")
		} else if wr != nil {
			s.Logger.Println(wr)
		}
	}

	lm.Normalize(img.FlipNone)
	state, err := lm.Serialize()
	if err != nil {
		return nil, errors.Wrap(err, "source: cannot serialize")
	}

	return &Source{
		lastAccess: time.Now(),

		FilePath: filePath,
		FileHash: hash.Sum32(),

		PSD: root,
		PFV: pf,

		InitialLayerState: state,
	}, nil
}

// get returns Source corresponding to a filePath. If the data has not yet been loaded, it will load.
func (s *Sources) get(filePath string) (*Source, error) {
	if s.srcs != nil {
		if src, ok := s.srcs[filePath]; ok {
			src.Touch()
			return src, nil
		}
	}
	src, err := s.load(filePath)
	if err != nil {
		return nil, errors.Wrapf(err, "source: failed to load %q", filePath)
	}
	if s.srcs == nil {
		s.srcs = make(map[string]*Source)
	}
	s.srcs[filePath] = src
	return src, nil
}

func (s *Sources) NewImage(filePath string) (*img.Image, error) {
	s.m.Lock()
	defer s.m.Unlock()

	src, err := s.get(filePath)
	if err != nil {
		return nil, err
	}
	psd := src.PSD.Clone()
	pfv, err := src.PFV.Clone()
	if err != nil {
		return nil, err
	}
	return &img.Image{
		Toucher: src,

		FilePath: &src.FilePath,
		FileHash: src.FileHash,

		PSD:    psd,
		PFV:    pfv,
		Layers: img.NewLayerManager(psd),

		InitialLayerState: &src.InitialLayerState,

		Scale: 1,
	}, nil
}

func (s *Sources) GC() {
	s.m.Lock()
	defer s.m.Unlock()

	const deadline = 10 * time.Minute
	now := time.Now()

	for k, v := range s.srcs {
		if now.Sub(v.LastAccess()) > deadline {
			delete(s.srcs, k)
		}
	}
}

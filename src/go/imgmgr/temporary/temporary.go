package temporary

import (
	"time"

	"github.com/pkg/errors"

	"psdtoolkit/img"
	"psdtoolkit/imgmgr/source"
)

type Key struct {
	ID       int
	FilePath string
}

type Temporary struct {
	Srcs   *source.Sources
	images map[Key]*img.Image
}

func (tp *Temporary) Load(id int, filePath string) (*img.Image, error) {
	if img, ok := tp.images[Key{id, filePath}]; ok {
		img.Touch()
		return img, nil
	}
	nimg, err := tp.Srcs.NewImage(filePath)
	if err != nil {
		return nil, errors.Wrapf(err, "temporary: failed to load %q", filePath)
	}
	if tp.images == nil {
		tp.images = make(map[Key]*img.Image)
	}
	tp.images[Key{id, filePath}] = nimg
	return nimg, nil
}

func (tp *Temporary) GC() {
	const deadline = 10 * time.Minute
	now := time.Now()

	for k, v := range tp.images {
		if now.Sub(v.LastAccess()) > deadline {
			delete(tp.images, k)
		}
	}
}

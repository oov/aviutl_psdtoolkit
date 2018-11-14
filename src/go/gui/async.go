package gui

import (
	"context"
	"time"

	"github.com/oov/aviutl_psdtoolkit/src/go/img"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
)

func updateRenderedImage(g *GUI, img *img.Image) {
	if g.cancelRender != nil {
		g.cancelRender()
	}

	ctx, cancel := context.WithCancel(context.Background())
	g.cancelRender = cancel
	go func() {
		s := time.Now().UnixNano()
		nrgba, err := img.Render(ctx)
		if err != nil {
			ods.ODS("rendering: aborted: %v", err)
			return
		}
		ods.ODS("rendering: %dms", (time.Now().UnixNano()-s)/1e6)
		if err = g.do(func() error {
			g.mainView.SetRenderedImage(nrgba)
			g.thumbnailer.Update(nrgba, g.do)
			cancel()
			return nil
		}); err != nil {
			ods.ODS("gui: failed to update rendered image: %v", err)
		}
	}()
}

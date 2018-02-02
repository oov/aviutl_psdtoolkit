package main

import (
	"flag"
	"fmt"
	"runtime"

	// _ "net/http/pprof"

	"github.com/oov/aviutl_psdtoolkit/src/go/assets"
	"github.com/oov/aviutl_psdtoolkit/src/go/gc"
	"github.com/oov/aviutl_psdtoolkit/src/go/gui"
	"github.com/oov/aviutl_psdtoolkit/src/go/imgmgr/source"
	"github.com/oov/aviutl_psdtoolkit/src/go/ipc"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
)

func init() {
	runtime.LockOSThread()
}

type odsLogger struct{}

func (odsLogger) Println(v ...interface{}) {
	ods.ODS(fmt.Sprintln(v...))
}

func main() {
	// go http.ListenAndServe(":6060", nil)
	// psd.Debug = log.New(os.Stdout, "psd: ", log.Lshortfile)

	defer func() {
		if err := recover(); err != nil {
			ods.Recover(err)
		}
	}()

	flag.Parse()

	srcs := &source.Sources{Logger: odsLogger{}}
	ipcm := ipc.New(srcs)
	g := gui.New(srcs)

	ipcm.AddFile = g.AddFileSync
	ipcm.ShowGUI = g.ShowWindow
	ipcm.Serialize = g.Serialize
	ipcm.Deserialize = g.Deserialize
	ipcm.GCing = g.Touch
	g.SendEditingImageState = ipcm.SendEditingImageState
	g.CopyFaviewValue = ipcm.CopyFaviewValue
	g.ExportFaviewSlider = ipcm.ExportFaviewSlider
	g.DropFiles = func(filenames []string) {
		if err := g.AddFile(extractPSDAndPFV(filenames)); err != nil {
			g.ReportError(err)
		}
	}

	exitCh := make(chan struct{})
	go ipcm.Main(exitCh)
	gcDone := gc.Start(exitCh)

	if err := g.Init(
		"PSDToolKit "+version,
		assets.MustAsset("bg.png"),
		assets.MustAsset("Ohruri-Regular.ttf"),
		assets.MustAsset("symbols.ttf"),
	); err != nil {
		ipcm.Abort(err)
		<-gcDone
		return
	}

	g.Main(exitCh)
	<-gcDone
}

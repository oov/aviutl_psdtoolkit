package ods

import (
	"fmt"
	"os"
	"runtime"
	"unsafe"

	"golang.org/x/sys/windows"
)

var outputDebugStringW = windows.NewLazySystemDLL("kernel32").NewProc("OutputDebugStringW")
var debugging = os.Getenv("PSDTOOLKITDEBUG") != ""

func ODS(format string, a ...interface{}) {
	if !debugging {
		return
	}
	s := fmt.Sprintf("psdtoolkit srv: "+format, a...)
	p, err := windows.UTF16PtrFromString(s)
	if err != nil {
		return
	}
	outputDebugStringW.Call(uintptr(unsafe.Pointer(p)))
}

func Recover(err interface{}) {
	ODS("Panic: %v\n", err)
	for i := 2; ; i++ {
		pc, src, line, ok := runtime.Caller(i)
		if !ok {
			break
		}
		ODS("  [%d] %s: %s(%d)\n", i, runtime.FuncForPC(pc).Name(), src, line)
	}
}

package gc

import (
	"runtime/debug"
	"sync/atomic"
	"time"
)

var counter int32

func Start(exitCh <-chan struct{}) <-chan struct{} {
	done := make(chan struct{})
	go func() {
		fpsTicker := time.NewTicker(time.Second)
		for {
			select {
			case <-exitCh:
				fpsTicker.Stop()
				done <- struct{}{}
				return
			case <-fpsTicker.C:
				if atomic.LoadInt32(&counter) == 0 {
					debug.FreeOSMemory()
				}
			}
		}
	}()
	return done
}

func EnterCS() { atomic.AddInt32(&counter, 1) }
func LeaveCS() { atomic.AddInt32(&counter, -1) }

package jobqueue

import (
	"context"
	"errors"
	"sync"
	"sync/atomic"
)

var Continue = errors.New("jobqueue: requested continue")

type JobFunc func(context.Context) error

type JobQueue struct {
	queue  chan JobFunc
	done   chan struct{}
	ctx    context.Context
	cancel context.CancelFunc
	m      sync.Mutex
}

func New(length int) *JobQueue {
	jq := &JobQueue{
		done: make(chan struct{}),
	}
	jq.init()
	return jq
}

func (jq *JobQueue) init() {
	jq.queue = make(chan JobFunc, cap(jq.queue))
	jq.ctx, jq.cancel = context.WithCancel(context.Background())
	go jq.work()
}

func (jq *JobQueue) final() {
	close(jq.queue)
	jq.cancel()
	<-jq.done
}

func (jq *JobQueue) Close() {
	jq.m.Lock()
	jq.final()
	close(jq.done)
	jq.done = nil
	jq.m.Unlock()
}

func (jq *JobQueue) CancelAll() {
	jq.m.Lock()
	if jq.done != nil {
		jq.final()
		jq.init()
	}
	jq.m.Unlock()
}

func (jq *JobQueue) Enqueue(job JobFunc) {
	jq.m.Lock()
	if jq.done != nil {
		jq.queue <- job
	}
	jq.m.Unlock()
}

func (jq *JobQueue) work() {
	defer func() {
		if jq.ctx.Err() != context.Canceled {
			jq.cancel()
		}
		jq.done <- struct{}{}
	}()
	for f := range jq.queue {
		if jq.run(f) == context.Canceled {
			break
		}
	}
}

func (jq *JobQueue) run(job JobFunc) error {
	var finished int32
	finish := make(chan error, 1)
	go func() {
		for {
			err := job(jq.ctx)
			if err == Continue && atomic.LoadInt32(&finished) == 0 {
				continue
			}
			finish <- err
			break
		}
	}()
	doneCh := jq.ctx.Done()
	for {
		select {
		case err := <-finish:
			return err
		case <-doneCh:
			atomic.StoreInt32(&finished, 1)
			return jq.ctx.Err()
		}
	}
}

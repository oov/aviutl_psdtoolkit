package jobqueue

import (
	"context"
	"sync"
	"testing"
	"time"
)

func TestNewClose(t *testing.T) {
	jq := New(5)
	jq.Close()
}

func TestQueueing(t *testing.T) {
	jq := New(1)
	defer jq.Close()

	var got []int
	var m sync.Mutex
	jq.Enqueue(func(ctx context.Context) error {
		time.Sleep(50 * time.Millisecond)
		m.Lock()
		got = append(got, 1)
		m.Unlock()
		return nil
	})
	jq.Enqueue(func(ctx context.Context) error {
		time.Sleep(25 * time.Millisecond)
		m.Lock()
		got = append(got, 2)
		m.Unlock()
		return nil
	})
	for {
		time.Sleep(10 * time.Millisecond)
		m.Lock()
		l := len(got)
		m.Unlock()
		if l == 2 {
			break
		}
	}
	if got[0] != 1 || got[1] != 2 {
		t.Errorf("want 1, 2 got %d, %d", got[0], got[1])
	}
}

func TestCancelAll(t *testing.T) {
	jq := New(1)
	defer jq.Close()

	start := time.Now()
	var got []int
	var m sync.Mutex
	jq.Enqueue(func(ctx context.Context) error {
		if start.Add(50 * time.Millisecond).After(time.Now()) {
			time.Sleep(10 * time.Millisecond)
			return Continue
		}
		m.Lock()
		got = append(got, 1)
		m.Unlock()
		return nil
	})
	jq.CancelAll()
	time.Sleep(25 * time.Millisecond)
	m.Lock()
	l := len(got)
	m.Unlock()
	if l != 0 {
		t.Errorf("want 0 got %d", l)
	}
	time.Sleep(50 * time.Millisecond)
	m.Lock()
	l = len(got)
	m.Unlock()
	if l != 0 {
		t.Errorf("want 0 got %d", l)
	}
}

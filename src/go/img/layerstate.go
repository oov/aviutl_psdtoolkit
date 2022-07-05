package img

import (
	"encoding/binary"
	"fmt"
)

type layerState struct {
	Visible  bool
	Priority int
}

type layerStates struct {
	priority int
	flip     Flip
	lm       *LayerManager
	states   []layerState
}

func NewLayerStates(m *LayerManager) *layerStates {
	ls := &layerStates{
		flip:   m.Flip,
		lm:     m,
		states: make([]layerState, len(m.Layers)),
	}
	for fi, l := range m.Layers {
		ls.states[fi] = layerState{
			Visible:  l.Layer.Visible,
			Priority: 0,
		}
	}
	return ls
}

func (ls *layerStates) Increment() {
	ls.priority++
}

func (ls *layerStates) FindBySeqID(seqID SeqID) *layerState {
	fi, ok := ls.lm.Mapped[seqID]
	if !ok {
		return nil
	}
	return &ls.states[fi]
}

func (ls *layerStates) FindByFullPath(path string) *layerState {
	fi, ok := ls.lm.FullPath[path]
	if !ok {
		return nil
	}
	return &ls.states[fi]
}

func copyStateRecursive(ls *layerStates, fi0 flatIndex, fi1 flatIndex) {
	l0, l1 := &ls.lm.Layers[fi0], &ls.lm.Layers[fi1]
	for i := range l0.Layer.Children {
		seqID := SeqID(l0.Layer.Children[i].SeqID)
		cfi0, ok := ls.lm.Mapped[seqID]
		if !ok {
			continue
		}
		path := l1.FullPath + ls.lm.Layers[cfi0].FullPath[len(l0.FullPath):]
		cfi1, ok := ls.lm.FullPath[path]
		if !ok {
			continue
		}
		cs0, cs1 := &ls.states[cfi0], &ls.states[cfi1]
		if cs1.Visible != cs0.Visible {
			cs1.Visible = cs0.Visible
			cs1.Priority = ls.priority
		}
		copyStateRecursive(ls, cfi0, cfi1)
	}
}

func setFlipOne(ls *layerStates, fpMap flipPairMap, mirror bool, processed map[*flipPair]struct{}) error {
	for _, fp := range fpMap {
		if _, ok := processed[fp]; ok {
			continue
		}
		processed[fp] = struct{}{}
		var id0, id1 SeqID
		if mirror {
			id0 = fp.Original
			id1 = fp.Mirror
		} else {
			id0 = fp.Mirror
			id1 = fp.Original
		}
		fi0, ok := ls.lm.Mapped[id0]
		fi1, ok2 := ls.lm.Mapped[id1]
		if !ok || !ok2 {
			continue
		}
		s0, s1 := &ls.states[fi0], &ls.states[fi1]
		if !s0.Visible && !s1.Visible {
			continue
		}
		s0.Visible, s1.Visible = false, true
		s0.Priority, s1.Priority = ls.priority, ls.priority
		copyStateRecursive(ls, fi0, fi1)
	}

	return nil
}

func (ls *layerStates) SetAll(serializedBits string) error {
	buf, err := deserializeBits(serializedBits)
	if err != nil {
		return fmt.Errorf("img: cannot deserialize: %w", err)
	}
	n := int(binary.LittleEndian.Uint16(buf))
	if len(ls.states) != n {
		return fmt.Errorf("img: number of layers mismatch(expected %v got %v)", len(ls.states), n)
	}
	i := 0
	ls.Increment()
	pri := ls.priority
	for _, v := range buf[2:] {
		if i+7 < len(ls.states) {
			ls.states[i+0].Visible = v&0x80 != 0
			ls.states[i+0].Priority = pri
			ls.states[i+1].Visible = v&0x40 != 0
			ls.states[i+1].Priority = pri
			ls.states[i+2].Visible = v&0x20 != 0
			ls.states[i+2].Priority = pri
			ls.states[i+3].Visible = v&0x10 != 0
			ls.states[i+3].Priority = pri
			ls.states[i+4].Visible = v&0x08 != 0
			ls.states[i+4].Priority = pri
			ls.states[i+5].Visible = v&0x04 != 0
			ls.states[i+5].Priority = pri
			ls.states[i+6].Visible = v&0x02 != 0
			ls.states[i+6].Priority = pri
			ls.states[i+7].Visible = v&0x01 != 0
			ls.states[i+7].Priority = pri
			i += 8
			continue
		}
		v <<= 8 - uint(len(ls.states)-i)
		for i < len(ls.states) {
			ls.states[i].Visible = v&0x80 != 0
			ls.states[i].Priority = pri
			v <<= 1
			i++
		}
	}
	return nil
}

func (ls *layerStates) SetVisible(seqID SeqID, visible bool) error {
	fi, ok := ls.lm.Mapped[seqID]
	if !ok {
		return fmt.Errorf("SeqID: %v layer not found", seqID)
	}
	ls.Increment()
	return ls.setVisible(fi, visible)
}

func (ls *layerStates) setVisible(fi flatIndex, visible bool) error {
	s := &ls.states[fi]
	sg, ok := ls.lm.SyncedMap[SeqID(ls.lm.Layers[fi].Layer.SeqID)]
	if !ok {
		s.Visible = visible
		s.Priority = ls.priority
		return nil
	}
	for _, seqID := range *sg {
		ss := ls.FindBySeqID(seqID)
		if ss == nil {
			continue
		}
		ss.Visible = visible
		ss.Priority = ls.priority
	}
	return nil
}

func (ls *layerStates) SetVisibleExclusive(seqID SeqID, visible bool) error {
	fi, ok := ls.lm.Mapped[seqID]
	if !ok {
		return fmt.Errorf("SeqID: %v layer not found", seqID)
	}
	ls.Increment()
	l := &ls.lm.Layers[fi]
	if isGroup(l.Layer.Name) {
		return ls.setVisible(fi, visible)
	}
	for _, sib := range l.Layer.Parent.Children {
		if isGroup(sib.Name) {
			continue
		}
		ls.SetVisible(SeqID(sib.SeqID), seqID == SeqID(sib.SeqID) && visible)
	}
	return nil
}

func (ls *layerStates) SetFlip(flip Flip) error {
	if flip != FlipNone && flip != FlipX && flip != FlipY && flip != FlipXY {
		return fmt.Errorf("unknown flip state: %v", flip)
	}
	ls.Increment()
	processed := map[*flipPair]struct{}{}
	err := setFlipOne(ls, ls.lm.FlipXMap, flip == FlipX, processed)
	if err != nil {
		return err
	}
	err = setFlipOne(ls, ls.lm.FlipYMap, flip == FlipY, processed)
	if err != nil {
		return err
	}
	err = setFlipOne(ls, ls.lm.FlipXYMap, flip == FlipXY, processed)
	if err != nil {
		return err
	}
	ls.flip = flip
	return nil
}

func normalizeGroup(ls *layerStates, seqID SeqID) {
	g, ok := ls.lm.GroupMap[seqID]
	if !ok || len(*g) == 0 {
		return
	}
	maxPriority := -1
	maxPriorityID := SeqID(-1)
	for _, seqID := range *g {
		s := ls.FindBySeqID(seqID)
		if !s.Visible {
			continue
		}
		if s.Priority > maxPriority {
			maxPriority = s.Priority
			maxPriorityID = seqID
		}
		s.Visible = false
		s.Priority = ls.priority
	}
	if maxPriorityID == SeqID(-1) {
		maxPriorityID = (*g)[len(*g)-1]
	}
	s := ls.FindBySeqID(maxPriorityID)
	if s != nil {
		s.Visible = true
		s.Priority = ls.priority
	}
}

func normalize(ls *layerStates) {
	ls.Increment()
	for seqID := range ls.lm.ForceVisible {
		s := ls.FindBySeqID(seqID)
		s.Visible = true
		s.Priority = ls.priority
	}
	ls.Increment()
	processedGroup := map[*group]struct{}{}
	for seqID, g := range ls.lm.GroupMap {
		if _, ok := processedGroup[g]; ok {
			continue
		}
		normalizeGroup(ls, seqID)
		processedGroup[g] = struct{}{}
	}
	ls.Increment()
	ls.SetFlip(ls.flip)
}

func (ls *layerStates) Apply() bool {
	normalize(ls)
	modified := false
	for fi := range ls.lm.Layers {
		dstate := &ls.states[fi]
		l := &ls.lm.Layers[fi]
		if l.Layer.Visible != dstate.Visible {
			l.Layer.Visible = dstate.Visible
			modified = true
			ls.lm.Renderer.SetDirtyByLayer(l.Layer)
		}
	}
	ls.lm.Flip = ls.flip
	return modified
}

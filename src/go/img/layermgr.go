package img

import (
	"encoding/binary"
	"strings"

	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolkit/src/go/img/prop"
	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
	"github.com/oov/aviutl_psdtoolkit/src/go/warn"
	"github.com/oov/psd/composite"
)

type flipPair map[int]*[2]int

func (fp flipPair) FindOriginal(seqID int) int {
	p, ok := fp[seqID]
	if !ok {
		return -1
	}
	return p[0]
}

func (fp flipPair) FindMirror(seqID int) int {
	p, ok := fp[seqID]
	if !ok {
		return -1
	}
	return p[1]
}

type LayerManager struct {
	Renderer *composite.Renderer

	Mapped   map[int]*composite.Layer
	Flat     []int
	FullPath map[string]int // int != SeqID, index in Flat

	ForceVisible map[int]struct{}
	Group        map[int]*[]int
	FlipXPair    flipPair
	FlipYPair    flipPair
	FlipXYPair   flipPair
}

func NewLayerManager(tree *composite.Tree) *LayerManager {
	m := &LayerManager{
		Renderer: tree.Renderer,

		Mapped:   map[int]*composite.Layer{},
		Flat:     []int{},
		FullPath: map[string]int{},

		ForceVisible: map[int]struct{}{},
		Group:        map[int]*[]int{},
		FlipXPair:    flipPair{},
		FlipYPair:    flipPair{},
		FlipXYPair:   flipPair{},
	}
	dup := map[string]int{}
	var g []int
	for i := range tree.Root.Children {
		enumChildren(m, &tree.Root.Children[i], tree.Root.Children, nil, dup)
		if isGroup(tree.Root.Children[i].Name) {
			g = append(g, tree.Root.Children[i].SeqID)
		}
	}
	for _, seqID := range g {
		m.Group[seqID] = &g
	}
	return m
}

func (m *LayerManager) setVisible(seqID int, visible bool) bool {
	l := m.Mapped[seqID]
	if _, ok := m.ForceVisible[seqID]; ok {
		if !l.Visible {
			l.Visible = true
			m.Renderer.SetDirtyByLayer(l)
			return true
		}
		return false
	}
	if g, ok := m.Group[seqID]; ok {
		if visible {
			var modified bool
			for _, seqID0 := range *g {
				if seqID0 == seqID {
					continue
				}
				l0 := m.Mapped[seqID0]
				if l0.Visible {
					l0.Visible = false
					m.Renderer.SetDirtyByLayer(l0)
					modified = true
				}
			}
			if !l.Visible {
				l.Visible = true
				m.Renderer.SetDirtyByLayer(l)
				modified = true
			}
			return modified
		}

		n := 0
		for _, id := range *g {
			if m.Mapped[id].Visible {
				n++
				if n == 2 {
					break
				}
			}
		}
		if n <= 1 {
			return false
		}
		if !l.Visible {
			return false
		}
		l.Visible = false
		m.Renderer.SetDirtyByLayer(l)
		return true
	}
	if l.Visible == visible {
		return false
	}
	l.Visible = visible
	m.Renderer.SetDirtyByLayer(l)
	return true
}

func (m *LayerManager) SetVisible(seqID int, visible bool, flip Flip) bool {
	modified := m.setVisible(seqID, visible)
	modified = m.NormalizeFlipOne(seqID, flip) || modified
	return modified
}

func (m *LayerManager) SetVisibleExclusive(seqID int, visible bool, flip Flip) bool {
	if _, ok := m.Group[seqID]; ok {
		return m.SetVisible(seqID, visible, flip)
	}
	modified := false
	for _, l := range m.Mapped[seqID].Parent.Children {
		if _, ok := m.Group[l.SeqID]; ok {
			continue
		}
		modified = m.SetVisible(l.SeqID, l.SeqID == seqID && visible, flip) || modified
	}
	return modified
}

func (m *LayerManager) normalizeGroupMap(seqID int, layers map[int]*deserializingState) bool {
	g, ok := m.Group[seqID]
	if !ok {
		return false
	}

	found, maxPriority, maxPrioritySeqID := 0, -1, -1
	for i := len(*g) - 1; i >= 0; i-- {
		seqID := (*g)[i]
		if !layers[seqID].Visible {
			continue
		}
		found++
		if layers[seqID].Priority > maxPriority {
			maxPriority = layers[seqID].Priority
			maxPrioritySeqID = seqID
		}
		layers[seqID].Visible = false
	}
	if found > 0 {
		layers[maxPrioritySeqID].Visible = true
	}
	return found > 1
}

func (m *LayerManager) normalizeFlipOne(seqID int, flip Flip) bool {
	idOrg := m.FlipXPair.FindOriginal(seqID)
	if idOrg == -1 {
		idOrg = m.FlipYPair.FindOriginal(seqID)
		if idOrg == -1 {
			idOrg = m.FlipXYPair.FindOriginal(seqID)
			if idOrg == -1 {
				return false
			}
		}
	}
	org := m.Mapped[idOrg]

	idX := m.FlipXPair.FindMirror(idOrg)
	idY := m.FlipYPair.FindMirror(idOrg)
	idXY := m.FlipXYPair.FindMirror(idOrg)
	var mrrX, mrrY, mrrXY *composite.Layer
	if idX != -1 {
		mrrX = m.Mapped[idX]
	}
	if idY != -1 {
		mrrY = m.Mapped[idY]
	}
	if idXY != -1 {
		mrrXY = m.Mapped[idXY]
	}
	if !org.Visible &&
		(mrrX == nil || !mrrX.Visible) &&
		(mrrY == nil || !mrrY.Visible) &&
		(mrrXY == nil || !mrrXY.Visible) {
		return false
	}

	var modified bool
	var active *composite.Layer
	switch flip {
	case FlipX:
		active = mrrX
		if mrrX != nil && !mrrX.Visible {
			mrrX.Visible = true
			modified = true
		}
	case FlipY:
		active = mrrY
		if mrrY != nil && !mrrY.Visible {
			mrrY.Visible = true
			modified = true
		}
	case FlipXY:
		active = mrrXY
		if mrrXY != nil && !mrrXY.Visible {
			mrrXY.Visible = true
			modified = true
		}
	}
	if active == nil {
		active = org
		if !org.Visible {
			org.Visible = true
			modified = true
		}
	}
	if org != active && org.Visible {
		org.Visible = false
		modified = true
	}
	if mrrX != nil && mrrX != active && mrrX.Visible {
		mrrX.Visible = false
		modified = true
	}
	if mrrY != nil && mrrY != active && mrrY.Visible {
		mrrY.Visible = false
		modified = true
	}
	if mrrXY != nil && mrrXY != active && mrrXY.Visible {
		mrrXY.Visible = false
		modified = true
	}
	return modified
}

func (m *LayerManager) NormalizeFlipOne(seqID int, flip Flip) bool {
	return m.normalizeFlipOne(seqID, flip)
}

func (m *LayerManager) normalizeFlipOneMap(seqID int, flip Flip, layers map[int]*deserializingState, processed map[int]struct{}) bool {
	if _, ok := processed[seqID]; ok {
		return false
	}

	orgID := m.FlipXPair.FindOriginal(seqID)
	if orgID == -1 {
		orgID = m.FlipYPair.FindOriginal(seqID)
		if orgID == -1 {
			orgID = m.FlipXYPair.FindOriginal(seqID)
			if orgID == -1 {
				return false
			}
		}
	}

	if _, ok := processed[orgID]; ok {
		return false
	}
	processed[orgID] = struct{}{}

	org := &layers[orgID].Visible

	xID := m.FlipXPair.FindMirror(orgID)
	yID := m.FlipYPair.FindMirror(orgID)
	xyID := m.FlipXYPair.FindMirror(orgID)
	var mrrX, mrrY, mrrXY *bool
	if xID != -1 {
		mrrX = &layers[xID].Visible
		processed[xID] = struct{}{}
	}
	if yID != -1 {
		mrrY = &layers[yID].Visible
		processed[yID] = struct{}{}
	}
	if xyID != -1 {
		mrrXY = &layers[xyID].Visible
		processed[xyID] = struct{}{}
	}
	if !*org &&
		(mrrX == nil || !*mrrX) &&
		(mrrY == nil || !*mrrY) &&
		(mrrXY == nil || !*mrrXY) {
		return false
	}

	var modified bool
	var active *bool
	switch flip {
	case FlipX:
		active = mrrX
		if mrrX != nil && !*mrrX {
			*mrrX = true
			modified = true
		}
	case FlipY:
		active = mrrY
		if mrrY != nil && !*mrrY {
			*mrrY = true
			modified = true
		}
	case FlipXY:
		active = mrrXY
		if mrrXY != nil && !*mrrXY {
			*mrrXY = true
			modified = true
		}
	}
	if active == nil {
		active = org
		if !*org {
			*org = true
			modified = true
		}
	}
	if org != active && *org {
		*org = false
		modified = true
	}
	if mrrX != nil && mrrX != active && *mrrX {
		*mrrX = false
		modified = true
	}
	if mrrY != nil && mrrY != active && *mrrY {
		*mrrY = false
		modified = true
	}
	if mrrXY != nil && mrrXY != active && *mrrXY {
		*mrrXY = false
		modified = true
	}

	return modified
}

func (m *LayerManager) Normalize(flip Flip) bool {
	layers := make(map[int]*deserializingState, len(m.Flat))
	for _, seqID := range m.Flat {
		layers[seqID] = &deserializingState{Visible: m.Mapped[seqID].Visible}
	}
	if !m.NormalizeMap(layers, flip) {
		return false
	}
	for seqID, dstate := range layers {
		l := m.Mapped[seqID]
		if l.Visible != dstate.Visible {
			l.Visible = dstate.Visible
			m.Renderer.SetDirtyByLayer(l)
		}
	}
	return true
}

func (m *LayerManager) NormalizeMap(layers map[int]*deserializingState, flip Flip) bool {
	modified := false
	for seqID := range m.ForceVisible {
		if layers[seqID].Visible == true {
			continue
		}
		layers[seqID].Visible = true
		modified = true
	}

	processedGroup := map[*[]int]struct{}{}
	for seqID, g := range m.Group {
		if _, ok := processedGroup[g]; ok {
			continue
		}
		modified = m.normalizeGroupMap(seqID, layers) || modified
		processedGroup[g] = struct{}{}
	}

	processedFlip := map[int]struct{}{}
	for seqID := range layers {
		modified = m.normalizeFlipOneMap(seqID, flip, layers, processedFlip) || modified
	}
	return modified
}

func isForceVisible(s string) bool {
	return len(s) > 1 && s[0] == '!' && s != "!?"
}

func isGroup(s string) bool {
	return len(s) > 1 && s[0] == '*' && s[1] != '*'
}

func registerFlips(m *LayerManager, l *composite.Layer, sib []composite.Layer) {
	tokens := strings.Split(l.Name, ":")
	var orgName string
	for i := len(tokens) - 1; i >= 0; i-- {
		switch tokens[i] {
		case "flipx", "flipy", "flipxy":
			continue
		default:
			if i == len(tokens)-1 {
				return // not found
			}
			break
		}
		orgName = strings.Join(tokens[:i+1], ":")
		break
	}

	var org *composite.Layer
	for i, l2 := range sib {
		if l2.Name == orgName {
			org = &sib[i]
			break
		}
	}
	if org == nil {
		return
	}
	f := &[2]int{org.SeqID, l.SeqID}
	for i := len(tokens) - 1; i >= 0; i-- {
		switch tokens[i] {
		case "flipx":
			m.FlipXPair[org.SeqID] = f
			m.FlipXPair[l.SeqID] = f
		case "flipy":
			m.FlipYPair[org.SeqID] = f
			m.FlipYPair[l.SeqID] = f
		case "flipxy":
			m.FlipXYPair[org.SeqID] = f
			m.FlipXYPair[l.SeqID] = f
		default:
			return
		}
	}
}

func enumChildren(m *LayerManager, l *composite.Layer, sib []composite.Layer, dir []byte, dup map[string]int) {
	if dir != nil {
		dir = append(dir, '/')
	}
	dir = append(dir, encodeName(l.Name)...)
	n, ok := dup[l.Name]
	if ok {
		n++
		dir = append(dir, '\\')
		dir = append(dir, itoa(n)...)
	}
	dup[l.Name] = n

	m.Mapped[l.SeqID] = l
	m.FullPath[string(dir)] = len(m.Flat)
	m.Flat = append(m.Flat, l.SeqID)

	if isForceVisible(l.Name) {
		m.ForceVisible[l.SeqID] = struct{}{}
	}
	registerFlips(m, l, sib)

	dup = map[string]int{}
	var g []int
	for i := range l.Children {
		enumChildren(m, &l.Children[i], l.Children, dir, dup)
		if isGroup(l.Children[i].Name) {
			g = append(g, l.Children[i].SeqID)
		}
	}
	for _, seqID := range g {
		m.Group[seqID] = &g
	}
}

type deserializingState struct {
	Visible  bool
	Priority int
}

func (m *LayerManager) GetFullPathLayerNames() []string {
	tmpMap := map[int]string{}
	for fullpath, fi := range m.FullPath {
		tmpMap[fi] = fullpath
	}
	r := make([]string, len(m.Flat))
	for i := range m.Flat {
		r[i] = tmpMap[i]
	}
	return r
}

func (m *LayerManager) GetFlatIndex(l *composite.Layer) int {
	for fidx, mi := range m.Flat {
		if mi == l.SeqID {
			return fidx
		}
	}
	return -1
}

func (m *LayerManager) Deserialize(s string, flip Flip, pfv *PFV) (bool, Flip, error) {
	layers := make(map[int]*deserializingState, len(m.Flat))
	n := make([]deserializingState, len(m.Flat))
	for index, seqID := range m.Flat {
		n[index].Visible = m.Mapped[seqID].Visible
		layers[seqID] = &n[index]
	}

	newFlip := flip
	for priority, line := range strings.Split(s, " ") {
		if len(line) < 2 {
			continue
		}
		switch line[:2] {
		case "L.":
			if len(line) != 3 {
				ods.ODS("unknown flip parameter: %q. skipped.", line[2:])
				continue
			}
			fl := int(line[2] - '0')
			if 0 <= fl && fl <= 3 {
				newFlip = Flip(fl)
			}
		case "V.":
			buf, err := deserializeBits(line[2:])
			if err != nil {
				return false, FlipNone, errors.Wrap(err, "img: cannot deserialize")
			}
			if len(n) != int(binary.LittleEndian.Uint16(buf)) {
				return false, FlipNone, errors.New("img: number of layers mismatch")
			}
			i := 0
			for _, v := range buf[2:] {
				if i+7 < len(n) {
					n[i+0].Visible = v&0x80 != 0
					n[i+0].Priority = priority + 1
					n[i+1].Visible = v&0x40 != 0
					n[i+1].Priority = priority + 1
					n[i+2].Visible = v&0x20 != 0
					n[i+2].Priority = priority + 1
					n[i+3].Visible = v&0x10 != 0
					n[i+3].Priority = priority + 1
					n[i+4].Visible = v&0x08 != 0
					n[i+4].Priority = priority + 1
					n[i+5].Visible = v&0x04 != 0
					n[i+5].Priority = priority + 1
					n[i+6].Visible = v&0x02 != 0
					n[i+6].Priority = priority + 1
					n[i+7].Visible = v&0x01 != 0
					n[i+7].Priority = priority + 1
					i += 8
					continue
				}
				v <<= 8 - uint(len(n)-i)
				for i < len(n) {
					n[i].Visible = v&0x80 != 0
					n[i].Priority = priority + 1
					v <<= 1
					i++
				}
			}
		case "v0", "v1":
			if len(line) < 5 || line[2] != '.' {
				ods.ODS("unexpected format: %q. skipped.", line)
				continue
			}
			ln, err := prop.Decode(line[2:])
			if err != nil {
				ods.ODS("%q is not a valid layer name. skipped. %v", line[2:], err)
				continue
			}
			idx, ok := m.FullPath[ln]
			if !ok {
				ods.ODS("layer %q is not found. skipped.", ln)
				continue
			}
			n[idx].Visible = line[1] == '1'
			n[idx].Priority = priority + 1

			if line[1] == '1' {
				// set parents too
				for rpos := len(ln) - 1; rpos > 0; rpos-- {
					if ln[rpos] != '/' {
						continue
					}
					idx, ok := m.FullPath[ln[:rpos]]
					if !ok {
						ods.ODS("layer %q is not found. skipped.", ln)
						break
					}
					n[idx].Visible = true
					n[idx].Priority = priority + 1
				}
			}
		case "F.", "F_":
			if pfv == nil {
				ods.ODS("do not have favorite data. skipped.")
				continue
			}
			s, err := prop.Decode(line[1:])
			if err != nil {
				ods.ODS("%q is not a valid state. skipped. %v", line[1:], err)
				continue
			}
			fn, err := pfv.FindNode(s, false)
			if err != nil {
				ods.ODS("failed to find favorite node. %v", err)
				continue
			}
			if fn == nil {
				ods.ODS("favorite node %q not found. skipped.", s)
				continue
			}
			if f, v := fn.RawState(); f == nil {
				for i, visible := range v {
					n[i].Visible = visible
					n[i].Priority = priority + 1
				}
			} else {
				for i, pass := range f {
					if !pass {
						continue
					}
					n[i].Visible = v[i]
					n[i].Priority = priority + 1
				}
			}
		case "S.", "S_":
			if pfv == nil {
				ods.ODS("do not have favorite data. skipped.")
				continue
			}
			s, err := prop.Decode(line[1:])
			if err != nil {
				ods.ODS("%q is not a valid state. skipped. %v", line[1:], err)
				continue
			}
			kv := strings.Split(s, "~")
			if len(kv) != 2 {
				ods.ODS("unexpected format: %q. skipped. %v", s, err)
				continue
			}
			fn, err := pfv.FindFaviewNode(kv[0], false)
			if err != nil {
				ods.ODS("failed to find faview node. %v", err)
				continue
			}
			if fn == nil {
				ods.ODS("faview node %q not found. skipped.", kv[0])
				continue
			}
			idx := fn.FindItem(kv[1])
			if idx == -1 {
				ods.ODS("faview node item %q not found. skipped.", kv[1])
				continue
			}
			f, v := fn.Items[idx].RawState()
			for i, pass := range f {
				if !pass {
					continue
				}
				n[i].Visible = v[i]
				n[i].Priority = priority + 1
			}
		}
	}

	m.NormalizeMap(layers, newFlip)
	modified := flip != newFlip
	r := m.Renderer
	for seqID, dstate := range layers {
		l := m.Mapped[seqID]
		if l.Visible != dstate.Visible {
			l.Visible = dstate.Visible
			r.SetDirtyByLayer(l)
			modified = true
		}
	}
	return modified, newFlip, nil
}

func (m *LayerManager) Serialize() (string, error) {
	v := make([]bool, len(m.Flat))
	for i, seqID := range m.Flat {
		v[i] = m.Mapped[seqID].Visible
	}
	s, err := serializeBits(v)
	if err != nil {
		return "", errors.Wrap(err, "LayerManager.Serialize: failed to serialize")
	}
	return "V." + s, nil
}

type SerializedData struct {
	Visible    bool
	FolderOpen bool
}

func (m *LayerManager) SerializeSafe() map[string]SerializedData {
	v := make(map[string]SerializedData, len(m.Flat))
	for fullPath, idx := range m.FullPath {
		l := m.Mapped[m.Flat[idx]]
		v[fullPath] = SerializedData{
			Visible:    l.Visible,
			FolderOpen: l.Folder && l.FolderOpen,
		}
	}
	return v
}

func (m *LayerManager) DeserializeSafe(state map[string]SerializedData) (warn.Warning, error) {
	var wr warn.Warning
	for fullPath, d := range state {
		if idx, ok := m.FullPath[fullPath]; ok {
			l := m.Mapped[m.Flat[idx]]
			l.Visible = d.Visible
			if l.Folder {
				l.FolderOpen = d.FolderOpen
			}
		} else {
			wr = append(wr, errors.Errorf("img: layer %q not found", fullPath))
		}
	}
	return wr, nil
}

package img

import (
	"encoding/base64"
	"encoding/binary"
	"strconv"
	"strings"

	"github.com/pkg/errors"

	"github.com/oov/aviutl_psdtoolkit/src/go/ods"
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
	FullPath map[string]int // int != SeqID

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

func (m *LayerManager) normalizeGroupMap(seqID int, layers map[int]*bool) bool {
	g, ok := m.Group[seqID]
	if !ok {
		return false
	}

	modified := false
	var done bool
	for i := len(*g) - 1; i >= 0; i-- {
		seqID := (*g)[i]
		if !*layers[seqID] {
			continue
		}
		if !done {
			done = true
			continue
		}
		*layers[seqID] = false
		modified = true
	}
	if !done && len(*g) > 0 {
		*layers[(*g)[len(*g)-1]] = true
		modified = true
	}
	return modified
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

func (m *LayerManager) normalizeFlipOneMap(seqID int, flip Flip, layers map[int]*bool, processed map[int]struct{}) bool {
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

	org := layers[orgID]

	xID := m.FlipXPair.FindMirror(orgID)
	yID := m.FlipYPair.FindMirror(orgID)
	xyID := m.FlipXYPair.FindMirror(orgID)
	var mrrX, mrrY, mrrXY *bool
	if xID != -1 {
		mrrX = layers[xID]
		processed[xID] = struct{}{}
	}
	if yID != -1 {
		mrrY = layers[yID]
		processed[yID] = struct{}{}
	}
	if xyID != -1 {
		mrrXY = layers[xyID]
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
	layers := make(map[int]*bool, len(m.Flat))
	for _, seqID := range m.Flat {
		b := m.Mapped[seqID].Visible
		layers[seqID] = &b
	}
	if !m.NormalizeMap(layers, flip) {
		return false
	}
	for seqID, visible := range layers {
		l := m.Mapped[seqID]
		if l.Visible != *visible {
			l.Visible = *visible
			m.Renderer.SetDirtyByLayer(l)
		}
	}
	return true
}

func (m *LayerManager) NormalizeMap(layers map[int]*bool, flip Flip) bool {
	modified := false
	for seqID := range m.ForceVisible {
		if *layers[seqID] == true {
			continue
		}
		*layers[seqID] = true
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
	return len(s) > 2 && s[0] == '!' && s != "!?"
}

func isGroup(s string) bool {
	return len(s) > 2 && s[0] == '*' && s[1] != '*'
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

func (m *LayerManager) Deserialize(s string, flip Flip, faviewRoot *FaviewNode) (bool, Flip, error) {
	layers := make(map[int]*bool, len(m.Flat))
	n := make([]bool, len(m.Flat))
	for index, seqID := range m.Flat {
		n[index] = m.Mapped[seqID].Visible
		layers[seqID] = &n[index]
	}

	var items []*FaviewNode
	newFlip := flip
	for _, line := range strings.Split(s, " ") {
		if len(line) < 2 {
			continue
		}
		switch line[:2] {
		case "L.":
			if len(line) != 3 {
				return false, FlipNone, errors.New("img: unknown flip parameter")
			}
			fl := int(line[2] - '0')
			if 0 <= fl && fl <= 3 {
				newFlip = Flip(fl)
			}
		case "V.":
			buf, err := base64.RawURLEncoding.DecodeString(line[2:])
			if err != nil {
				return false, FlipNone, err
			}
			if buf, err = decodePackBits(buf); err != nil {
				return false, FlipNone, err
			}
			if len(n) != int(binary.LittleEndian.Uint16(buf)) {
				return false, FlipNone, errors.New("img: number of layers mismatch")
			}
			i := 0
			for _, v := range buf[2:] {
				if i+7 < len(n) {
					n[i+0] = v&0x80 != 0
					n[i+1] = v&0x40 != 0
					n[i+2] = v&0x20 != 0
					n[i+3] = v&0x10 != 0
					n[i+4] = v&0x08 != 0
					n[i+5] = v&0x04 != 0
					n[i+6] = v&0x02 != 0
					n[i+7] = v&0x01 != 0
					i += 8
					continue
				}
				v <<= 8 - uint(len(n)-i)
				for i < len(n) {
					n[i] = v&0x80 != 0
					v <<= 1
					i++
				}
			}
		case "F.":
			buf, err := base64.RawURLEncoding.DecodeString(line[2:])
			if err != nil {
				return false, FlipNone, err
			}
			if buf, err = decodePackBits(buf); err != nil {
				return false, FlipNone, err
			}
			if len(n) != int(binary.LittleEndian.Uint16(buf)) {
				return false, FlipNone, errors.New("img: number of layers mismatch")
			}
			i := 0
			for _, v := range buf[2:] {
				if i+3 < len(n) {
					if v&0x80 != 0 {
						n[i+0] = v&0x40 != 0
					}
					if v&0x20 != 0 {
						n[i+1] = v&0x10 != 0
					}
					if v&0x08 != 0 {
						n[i+2] = v&0x04 != 0
					}
					if v&0x02 != 0 {
						n[i+3] = v&0x01 != 0
					}
					i += 4
					continue
				}
				v <<= (4 - uint(len(n)-i)) * 2
				for i < len(n) {
					if v&0x80 != 0 {
						n[i] = v&0x40 != 0
					}
					v <<= 2
					i++
				}
			}
		case "S.":
			if items == nil {
				if faviewRoot == nil {
					ods.ODS("do not have faview data. skipped.")
					continue
				}
				items = faviewRoot.EnumItemNode()
			}
			values := strings.Split(line[2:], "_")
			for i := 0; i < len(items) && i < len(values); i++ {
				if values[i] == "" {
					continue
				}
				index, err := strconv.Atoi(values[i])
				if err != nil {
					ods.ODS("%q is not a valid number. skipped.", values[i])
					continue
				}
				if index < 0 || index >= len(items[i].Items) {
					ods.ODS("index %d is out of range. skipped.", index)
					continue
				}
				f, v := items[i].Items[index].RawState()
				for i, pass := range f {
					if !pass {
						continue
					}
					n[i] = v[i]
				}
			}
		}
	}

	m.NormalizeMap(layers, newFlip)
	modified := flip != newFlip
	r := m.Renderer
	for seqID, visible := range layers {
		l := m.Mapped[seqID]
		if l.Visible != *visible {
			l.Visible = *visible
			r.SetDirtyByLayer(l)
			modified = true
		}
	}
	return modified, newFlip, nil
}

func (m *LayerManager) Serialize() string {
	v := make([]bool, len(m.Flat))
	for i, seqID := range m.Flat {
		v[i] = m.Mapped[seqID].Visible
	}
	return serialize(nil, v)
}

package img

import (
	"encoding/base64"
	"encoding/binary"
	"strings"

	"github.com/pkg/errors"

	"github.com/oov/psd/layertree"
)

type Flip int

const (
	FlipNone Flip = iota
	FlipX
	FlipY
	FlipXY
)

type LayerManager struct {
	Renderer *layertree.Renderer

	flip Flip

	Mapped   map[int]*layertree.Layer
	Flat     []int
	FullPath map[string]int // int != SeqID

	ForceVisible map[int]struct{}
	Group        map[int]*[]int
	FlipXPair    map[int]*[2]int
	FlipYPair    map[int]*[2]int
	FlipXYPair   map[int]*[2]int
}

func NewLayerManager(root *layertree.Root) *LayerManager {
	m := &LayerManager{
		Renderer: root.Renderer,

		Mapped:   map[int]*layertree.Layer{},
		Flat:     []int{},
		FullPath: map[string]int{},

		ForceVisible: map[int]struct{}{},
		Group:        map[int]*[]int{},
		FlipXPair:    map[int]*[2]int{},
		FlipYPair:    map[int]*[2]int{},
		FlipXYPair:   map[int]*[2]int{},
	}
	dup := map[string]int{}
	var g []int
	for i := range root.Children {
		enumChildren(m, &root.Children[i], root.Children, nil, dup)
		if isGroup(root.Children[i].Name) {
			g = append(g, root.Children[i].SeqID)
		}
	}
	for _, seqID := range g {
		m.Group[seqID] = &g
	}
	return m
}

func (m *LayerManager) SetFlip(f Flip) {
	m.flip = f
}

func (m *LayerManager) Flip() Flip {
	return m.flip
}

func (m *LayerManager) FlipX() bool {
	return m.flip == FlipX || m.flip == FlipXY
}

func (m *LayerManager) FlipY() bool {
	return m.flip == FlipY || m.flip == FlipXY
}

func (m *LayerManager) SetFlipX(v bool) bool {
	if (m.flip&FlipX != 0) == v {
		return false
	}
	if v {
		m.flip |= FlipX
	} else {
		m.flip &= ^FlipX
	}
	return true
}

func (m *LayerManager) SetFlipY(v bool) bool {
	if (m.flip&FlipY != 0) == v {
		return false
	}
	if v {
		m.flip |= FlipY
	} else {
		m.flip &= ^FlipY
	}
	return true
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

func (m *LayerManager) SetVisible(seqID int, visible bool) bool {
	modified := m.setVisible(seqID, visible)
	modified = m.NormalizeFlipOne(seqID) || modified
	return modified
}

func (m *LayerManager) normalizeGroup(seqID int) {
	g, ok := m.Group[seqID]
	if !ok {
		return
	}

	r := m.Renderer
	var done bool
	for i := len(*g) - 1; i >= 0; i-- {
		l := m.Mapped[(*g)[i]]
		if !l.Visible {
			continue
		}
		if !done {
			done = true
			continue
		}
		l.Visible = false
		r.SetDirtyByLayer(l)
	}
	if !done && len(*g) > 0 {
		l := m.Mapped[(*g)[len(*g)-1]]
		l.Visible = true
		r.SetDirtyByLayer(l)
	}
}

func (m *LayerManager) normalizeFlipOne(seqID int, Pair map[int]*[2]int, flipped bool) bool {
	r := m.Renderer
	pair, ok := Pair[seqID]
	if !ok {
		return false
	}
	org, mrr := m.Mapped[pair[0]], m.Mapped[pair[1]]
	if !org.Visible && !mrr.Visible {
		return false
	}
	modified := false
	if org.Visible == flipped {
		org.Visible = !flipped
		r.SetDirtyByLayer(org)
		modified = true
	}
	if mrr.Visible != flipped {
		mrr.Visible = flipped
		r.SetDirtyByLayer(mrr)
		modified = true
	}
	return modified
}

func (m *LayerManager) NormalizeFlipOne(seqID int) bool {
	modified := m.normalizeFlipOne(seqID, m.FlipXPair, m.flip == FlipX)
	modified = m.normalizeFlipOne(seqID, m.FlipYPair, m.flip == FlipY) || modified
	modified = m.normalizeFlipOne(seqID, m.FlipXYPair, m.flip == FlipXY) || modified
	return modified
}

func (m *LayerManager) normalizeFlip(Pair map[int]*[2]int, flipped bool) {
	r := m.Renderer
	processed := map[*[2]int]struct{}{}
	for _, pair := range Pair {
		if _, ok := processed[pair]; ok {
			continue
		}
		org, mrr := m.Mapped[pair[0]], m.Mapped[pair[1]]
		if !org.Visible && !mrr.Visible {
			continue
		}
		if org.Visible == flipped {
			org.Visible = !flipped
			r.SetDirtyByLayer(org)
		}
		if mrr.Visible != flipped {
			mrr.Visible = flipped
			r.SetDirtyByLayer(mrr)
		}
		processed[pair] = struct{}{}
	}
}

func (m *LayerManager) Normalize() {
	for seqID := range m.ForceVisible {
		l := m.Mapped[seqID]
		if l.Visible == true {
			continue
		}
		l.Visible = false
		m.Renderer.SetDirtyByLayer(l)
	}

	processed := map[*[]int]struct{}{}
	for seqID, g := range m.Group {
		if _, ok := processed[g]; ok {
			continue
		}
		m.normalizeGroup(seqID)
		processed[g] = struct{}{}
	}

	m.normalizeFlip(m.FlipXPair, m.flip == FlipX)
	m.normalizeFlip(m.FlipYPair, m.flip == FlipY)
	m.normalizeFlip(m.FlipXYPair, m.flip == FlipXY)
}

func isForceVisible(s string) bool {
	return len(s) > 2 && s[0] == '!' && s != "!?"
}

func isGroup(s string) bool {
	return len(s) > 2 && s[0] == '*' && s[1] != '*'
}

func registerFlips(m *LayerManager, l *layertree.Layer, sib []layertree.Layer) {
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

	var org *layertree.Layer
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

func enumChildren(m *LayerManager, l *layertree.Layer, sib []layertree.Layer, dir []byte, dup map[string]int) {
	dir = append(dir, '/')
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

func (m *LayerManager) DeserializeVisibility(s string) (bool, error) {
	n := make([]bool, len(m.Flat))
	var newFlip Flip
	for _, line := range strings.Split(s, " ") {
		buf, err := base64.RawURLEncoding.DecodeString(line[2:])
		if err != nil {
			return false, err
		}
		if len(m.Flat) != int(binary.LittleEndian.Uint16(buf)) {
			return false, errors.New("img: number of layers mismatch")
		}
		newFlip = Flip(buf[2])
		i := 0
		switch line[:2] {
		case "V.":
			for _, v := range buf[3:] {
				if i+7 < len(m.Flat) {
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
				v <<= 8 - uint(len(m.Flat)-i)
				for i < len(m.Flat) {
					n[i] = v&0x80 != 0
					v <<= 1
					i++
				}
			}
		case "F.":
			for _, v := range buf[3:] {
				if i+3 < len(m.Flat) {
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
				v <<= (4 - uint(len(m.Flat)-i)) * 2
				for i < len(m.Flat) {
					if v&0x80 != 0 {
						n[i] = v&0x40 != 0
					}
					v <<= 2
					i++
				}
			}
		}
	}

	r := m.Renderer
	modified := newFlip != m.flip
	for i, b := range n {
		l := m.Mapped[m.Flat[i]]
		if l.Visible != b {
			l.Visible = b
			r.SetDirtyByLayer(l)
			modified = true
		}
	}
	if modified {
		m.flip = newFlip
		m.Normalize()
	}
	return modified, nil
}

func (m *LayerManager) SerializeVisibility() string {
	buf := make([]byte, 3+(len(m.Flat)+7)/8)
	binary.LittleEndian.PutUint16(buf, uint16(len(m.Flat)))
	buf[2] = byte(m.flip)
	var v byte
	d := 3
	for i, l := range m.Flat {
		l := m.Mapped[l]
		v <<= 1
		if l.Visible {
			v++
		}
		if i&0x7 == 0x7 {
			buf[d] = v
			v = 0
			d++
		}
	}
	if d < len(buf) {
		buf[d] = v
	}
	return "V." + base64.RawURLEncoding.EncodeToString(buf)
}

func (m *LayerManager) SerializeVisibilityWithFilter(filter []bool) string {
	if len(m.Flat) != len(filter) {
		panic("len(m.Flat) != len(filter)")
	}
	buf := make([]byte, 3+(len(m.Flat)*2+7)/8)
	binary.LittleEndian.PutUint16(buf, uint16(len(m.Flat)))
	buf[2] = byte(m.flip)
	var v byte
	d := 3
	for i, l := range m.Flat {
		l := m.Mapped[l]
		v <<= 2
		if filter[i] {
			v += 2
		}
		if l.Visible {
			v++
		}
		if i&0x3 == 0x3 {
			buf[d] = v
			v = 0
			d++
		}
	}
	if d < len(buf) {
		buf[d] = v
	}
	return "F." + base64.RawURLEncoding.EncodeToString(buf)
}

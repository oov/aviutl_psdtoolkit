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

type LayerManager struct {
	Renderer *composite.Renderer

	Mapped   map[int]*composite.Layer
	Flat     []int
	FullPath map[string]int // int != SeqID

	ForceVisible map[int]struct{}
	Group        map[int]*[]int
	FlipXPair    map[int]*[2]int
	FlipYPair    map[int]*[2]int
	FlipXYPair   map[int]*[2]int
}

func NewLayerManager(tree *composite.Tree) *LayerManager {
	m := &LayerManager{
		Renderer: tree.Renderer,

		Mapped:   map[int]*composite.Layer{},
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

func (m *LayerManager) NormalizeFlipOne(seqID int, flip Flip) bool {
	modified := m.normalizeFlipOne(seqID, m.FlipXPair, flip == FlipX)
	modified = m.normalizeFlipOne(seqID, m.FlipYPair, flip == FlipY) || modified
	modified = m.normalizeFlipOne(seqID, m.FlipXYPair, flip == FlipXY) || modified
	return modified
}

func (m *LayerManager) normalizeFlipMap(Pair map[int]*[2]int, flipped bool, layers map[int]*bool) bool {
	modified := false
	processed := map[*[2]int]struct{}{}
	for _, pair := range Pair {
		if _, ok := processed[pair]; ok {
			continue
		}
		org, mrr := layers[pair[0]], layers[pair[1]]
		if !*org && !*mrr {
			continue
		}
		if *org == flipped {
			*org = !flipped
			modified = true
		}
		if *mrr != flipped {
			*mrr = flipped
			modified = true
		}
		processed[pair] = struct{}{}
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

	processed := map[*[]int]struct{}{}
	for seqID, g := range m.Group {
		if _, ok := processed[g]; ok {
			continue
		}
		modified = m.normalizeGroupMap(seqID, layers) || modified
		processed[g] = struct{}{}
	}

	modified = m.normalizeFlipMap(m.FlipXPair, flip == FlipX, layers) || modified
	modified = m.normalizeFlipMap(m.FlipYPair, flip == FlipY, layers) || modified
	modified = m.normalizeFlipMap(m.FlipXYPair, flip == FlipXY, layers) || modified
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

func (m *LayerManager) DeserializeVisibility(s string, flip Flip, faviewRoot *FaviewNode) (bool, Flip, error) {
	layers := make(map[int]*bool, len(m.Flat))
	n := make([]bool, len(m.Flat))
	for index, seqID := range m.Flat {
		n[index] = m.Mapped[seqID].Visible
		layers[seqID] = &n[index]
	}

	var items []*FaviewNode
	for _, line := range strings.Split(s, " ") {
		if len(line) < 2 {
			continue
		}
		switch line[:2] {
		case "V.":
			buf, err := base64.RawURLEncoding.DecodeString(line[2:])
			if err != nil {
				return false, flip, err
			}
			if len(n) != int(binary.LittleEndian.Uint16(buf)) {
				return false, flip, errors.New("img: number of layers mismatch")
			}
			i := 0
			flip = Flip(buf[2])
			for _, v := range buf[3:] {
				if i+7 < len(layers) {
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
				v <<= 8 - uint(len(layers)-i)
				for i < len(layers) {
					n[i] = v&0x80 != 0
					v <<= 1
					i++
				}
			}
		case "F.":
			buf, err := base64.RawURLEncoding.DecodeString(line[2:])
			if err != nil {
				return false, flip, err
			}
			if buf, err = decodePackBits(buf); err != nil {
				return false, flip, err
			}
			if len(n) != int(binary.LittleEndian.Uint16(buf)) {
				return false, flip, errors.New("img: number of layers mismatch")
			}
			i := 0
			for _, v := range buf[2:] {
				if i+3 < len(layers) {
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
				v <<= (4 - uint(len(layers)-i)) * 2
				for i < len(layers) {
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
				f, v := items[i].RawState(index)
				for i, pass := range f {
					if !pass {
						continue
					}
					n[i] = v[i]
				}
			}
		}
	}

	m.NormalizeMap(layers, flip)
	modified := false
	r := m.Renderer
	for seqID, visible := range layers {
		l := m.Mapped[seqID]
		if l.Visible != *visible {
			l.Visible = *visible
			r.SetDirtyByLayer(l)
			modified = true
		}
	}
	return modified, flip, nil
}

func (m *LayerManager) SerializeVisibility(flip Flip) string {
	buf := make([]byte, 3+(len(m.Flat)+7)/8)
	binary.LittleEndian.PutUint16(buf, uint16(len(m.Flat)))
	buf[2] = byte(flip)
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

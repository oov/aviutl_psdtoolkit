package img

import (
	"fmt"
	"strings"

	"github.com/oov/psd/composite"
	"github.com/pkg/errors"

	"psdtoolkit/img/prop"
	"psdtoolkit/ods"
	"psdtoolkit/warn"
)

type SeqID int

type flipPair struct {
	Original SeqID
	Mirror   SeqID
	Children []*group
}

type flipPairMap map[SeqID]*flipPair

func (fp flipPairMap) FindOriginal(seqID SeqID) SeqID {
	p, ok := fp[seqID]
	if !ok {
		return -1
	}
	return p.Original
}

func (fp flipPairMap) FindMirror(seqID SeqID) SeqID {
	p, ok := fp[seqID]
	if !ok {
		return -1
	}
	return p.Mirror
}

type forceVisibleMap map[SeqID]struct{}

type group []SeqID
type groupMap map[SeqID]*group

type flatIndex int

type Layer struct {
	Layer     *composite.Layer
	FlatIndex flatIndex
	FullPath  string
}

type LayerManager struct {
	Renderer *composite.Renderer
	Flip     Flip

	Layers   []Layer
	Mapped   map[SeqID]flatIndex
	FullPath map[string]flatIndex

	ForceVisible forceVisibleMap
	GroupMap     groupMap
	SyncedMap    groupMap
	FlipXMap     flipPairMap
	FlipYMap     flipPairMap
	FlipXYMap    flipPairMap
}

func NewLayerManager(tree *composite.Tree) *LayerManager {
	m := &LayerManager{
		Renderer: tree.Renderer,

		Mapped:   map[SeqID]flatIndex{},
		Layers:   []Layer{},
		FullPath: map[string]flatIndex{},

		ForceVisible: forceVisibleMap{},
		GroupMap:     groupMap{},
		SyncedMap:    groupMap{},
		FlipXMap:     flipPairMap{},
		FlipYMap:     flipPairMap{},
		FlipXYMap:    flipPairMap{},
	}
	dup := map[string]int{}
	var g group
	for i := range tree.Root.Children {
		enumChildren(m, &tree.Root.Children[i], tree.Root.Children, nil, dup)
		if isGroup(tree.Root.Children[i].Name) {
			g = append(g, SeqID(tree.Root.Children[i].SeqID))
		}
	}
	for _, seqID := range g {
		m.GroupMap[seqID] = &g
	}
	for _, fp := range m.FlipXMap {
		registerSyncs(m, &fp.Children, fp.Original, fp.Mirror)
	}
	for _, fp := range m.FlipYMap {
		registerSyncs(m, &fp.Children, fp.Original, fp.Mirror)
	}
	for _, fp := range m.FlipXYMap {
		registerSyncs(m, &fp.Children, fp.Original, fp.Mirror)
	}
	m.Normalize()
	return m
}

func (m *LayerManager) FindLayerBySeqID(seqID SeqID) *Layer {
	fi, ok := m.Mapped[seqID]
	if !ok {
		return nil
	}
	return &m.Layers[fi]
}

func (m *LayerManager) FindLayerByFullPath(path string) *Layer {
	fi, ok := m.FullPath[path]
	if !ok {
		return nil
	}
	return &m.Layers[fi]
}

func (m *LayerManager) SetVisible(seqID SeqID, visible bool) bool {
	ls := NewLayerStates(m)
	err := ls.SetVisible(seqID, visible)
	if err != nil {
		return false
	}
	return ls.Apply()
}

func (m *LayerManager) SetVisibleExclusive(seqID SeqID, visible bool) bool {
	ls := NewLayerStates(m)
	err := ls.SetVisibleExclusive(seqID, visible)
	if err != nil {
		return false
	}
	return ls.Apply()
}

func (m *LayerManager) SetFlip(flip Flip) bool {
	ls := NewLayerStates(m)
	err := ls.SetFlip(flip)
	if err != nil {
		return false
	}
	return ls.Apply()
}

func (m *LayerManager) Normalize() bool {
	return NewLayerStates(m).Apply()
}

func isForceVisible(s string) bool {
	return len(s) > 1 && s[0] == '!' && s != "!?"
}

func isGroup(s string) bool {
	return len(s) > 1 && s[0] == '*' && s[1] != '*'
}

func registerSyncs(lm *LayerManager, children *[]*group, id0 SeqID, id1 SeqID) {
	l0, l1 := lm.FindLayerBySeqID(id0), lm.FindLayerBySeqID(id1)
	if (l0 == nil) || (l1 == nil) {
		return
	}
	for i := range l0.Layer.Children {
		cid0 := SeqID(l0.Layer.Children[i].SeqID)
		cl0 := lm.FindLayerBySeqID(cid0)
		if cl0 == nil {
			continue
		}
		path := l1.FullPath + cl0.FullPath[len(l0.FullPath):]
		cl1 := lm.FindLayerByFullPath(path)
		if cl1 == nil {
			continue
		}
		cid1 := SeqID(cl1.Layer.SeqID)
		g := &group{cid0, cid1}
		lm.SyncedMap[cid0] = g
		lm.SyncedMap[cid1] = g
		*children = append(*children, g)
		if len(cl0.Layer.Children) > 0 && len(cl1.Layer.Children) > 0 {
			registerSyncs(lm, children, cid0, cid1)
		}
	}
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
	f := &flipPair{
		Children: []*group{},
		Original: SeqID(org.SeqID),
		Mirror:   SeqID(l.SeqID),
	}
	for i := len(tokens) - 1; i >= 0; i-- {
		switch tokens[i] {
		case "flipx":
			m.FlipXMap[f.Original] = f
			m.FlipXMap[f.Mirror] = f
		case "flipy":
			m.FlipYMap[f.Original] = f
			m.FlipYMap[f.Mirror] = f
		case "flipxy":
			m.FlipXYMap[f.Original] = f
			m.FlipXYMap[f.Mirror] = f
		default:
			return
		}
	}
}

func enumChildren(m *LayerManager, l *composite.Layer, sib []composite.Layer, dir []byte, dup map[string]int) {
	if dir != nil {
		dir = append(dir, '/')
	}
	n, ok := dup[l.Name]
	if ok {
		n++
		dir = append(dir, encodeName(l.Name+"\\"+itoa(n))...)
	} else {
		dir = append(dir, encodeName(l.Name)...)
	}
	dup[l.Name] = n

	fullPath := string(dir)
	layerIndex := flatIndex(len(m.Layers))
	m.Layers = append(m.Layers, Layer{
		FlatIndex: layerIndex,
		Layer:     l,
		FullPath:  fullPath,
	})
	m.Mapped[SeqID(l.SeqID)] = layerIndex
	m.FullPath[fullPath] = layerIndex

	if isForceVisible(l.Name) {
		m.ForceVisible[SeqID(l.SeqID)] = struct{}{}
	}
	registerFlips(m, l, sib)

	dup = map[string]int{}
	var g group
	for i := range l.Children {
		enumChildren(m, &l.Children[i], l.Children, dir, dup)
		if isGroup(l.Children[i].Name) {
			g = append(g, SeqID(l.Children[i].SeqID))
		}
	}
	for _, seqID := range g {
		m.GroupMap[seqID] = &g
	}
}

func (m *LayerManager) GetFullPathLayerNames() []string {
	r := make([]string, len(m.Layers))
	for i, l := range m.Layers {
		r[i] = l.FullPath
	}
	return r
}

func (m *LayerManager) GetFlatIndex(l *composite.Layer) int {
	fi, ok := m.Mapped[SeqID(l.SeqID)]
	if !ok {
		return -1
	}
	return int(fi)
}

func (m *LayerManager) Deserialize(s string, pfv *PFV) (bool, error) {
	ls := NewLayerStates(m)
	return m.deserialize(ls, strings.Split(s, " "), pfv)
}

func (m *LayerManager) deserialize(ls *layerStates, params []string, pfv *PFV) (bool, error) {
	for _, line := range params {
		if len(line) < 2 {
			continue
		}
		switch line[:2] {
		case "L.":
			if len(line) != 3 {
				ods.ODS("unknown flip parameter: %q. skipped.", line[2:])
				continue
			}
			err := ls.SetFlip(Flip(line[2] - '0'))
			if err != nil {
				ods.ODS("failed to apply flip. %v", err)
				continue
			}
		case "V.":
			err := ls.SetAll(line[2:])
			if err != nil {
				return false, fmt.Errorf("img: cannot deserialize: %w", err)
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
			fi, ok := ls.lm.FullPath[ln]
			if !ok {
				ods.ODS("layer %q is not found. skipped.", ln)
				continue
			}
			ls.Increment()
			ls.setVisible(fi, line[1] == '1')
			if line[1] == '1' {
				// set parents too
				for rpos := len(ln) - 1; rpos > 0; rpos-- {
					if ln[rpos] != '/' {
						continue
					}
					pfi, ok := ls.lm.FullPath[ln[:rpos]]
					if !ok {
						ods.ODS("layer %q is not found. skipped.", ln[:rpos])
						break
					}
					ls.setVisible(pfi, true)
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
			ls.Increment()
			if f, v := fn.RawState(); f == nil {
				for i, visible := range v {
					ls.setVisible(flatIndex(i), visible)
				}
			} else {
				for i, pass := range f {
					if !pass {
						continue
					}
					ls.setVisible(flatIndex(i), v[i])
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
			ls.Increment()
			f, v := fn.Items[idx].RawState()
			for i, pass := range f {
				if !pass {
					continue
				}
				ls.setVisible(flatIndex(i), v[i])
			}
		}
	}
	oldFlip := m.Flip
	modified := ls.Apply()
	return modified || (m.Flip != oldFlip), nil
}

func (m *LayerManager) Serialize() (string, error) {
	v := make([]bool, len(m.Layers))
	for i, l := range m.Layers {
		v[i] = l.Layer.Visible
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
	v := make(map[string]SerializedData, len(m.Layers))
	for fullPath, fi := range m.FullPath {
		l := &m.Layers[fi]
		v[fullPath] = SerializedData{
			Visible:    l.Layer.Visible,
			FolderOpen: l.Layer.Folder && l.Layer.FolderOpen,
		}
	}
	return v
}

func (m *LayerManager) DeserializeSafe(state map[string]SerializedData) (warn.Warning, error) {
	var wr warn.Warning
	for fullPath, d := range state {
		if fi, ok := m.FullPath[fullPath]; ok {
			l := &m.Layers[fi]
			l.Layer.Visible = d.Visible
			if l.Layer.Folder {
				l.Layer.FolderOpen = d.FolderOpen
			}
		} else {
			wr = append(wr, errors.Errorf("img: layer %q not found", fullPath))
		}
	}
	return wr, nil
}

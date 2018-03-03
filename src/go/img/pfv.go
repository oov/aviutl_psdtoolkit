package img

import (
	"bufio"
	"io"
	"strings"
	"time"

	"github.com/pkg/errors"
)

type warning []error

func (e warning) Error() string {
	var b []byte
	b = append(b, "there were some warnings:\n"...)
	for _, s := range e {
		b = append(b, s.Error()...)
		b = append(b, '\n')
	}
	return string(b)
}

func IsWarning(err error) bool {
	_, ok := err.(warning)
	return ok
}

type PFV struct {
	Setting    map[string]string
	Root       Node
	FaviewRoot FaviewNode
}

type Node struct {
	Name          string
	Open          bool
	Children      []Node
	Parent        *Node
	FilterSetting []bool
	Setting       []bool
}

func (n *Node) FullPath() string {
	names := []string{}
	c := n
	for c != nil {
		names = append(names, encodeName(c.Name))
		c = c.Parent
	}
	for i, l := 0, len(names)/2; i < l; i++ {
		names[i], names[len(names)-i-1] = names[len(names)-i-1], names[i]
	}
	return strings.Join(names, "/")
}

func (n *Node) Item() bool   { return n.Setting != nil }
func (n *Node) Folder() bool { return n.Children != nil && n.FilterSetting == nil }
func (n *Node) Filter() bool { return n.FilterSetting != nil }

func (n *Node) RawState() (filter, visibility []bool) {
	if !n.Item() {
		panic("node is not item")
	}
	filter = make([]bool, len(n.Setting))
	for i := range filter {
		filter[i] = true
	}
	if makeFilter(filter, n) {
		return filter, n.Setting
	}
	return nil, n.Setting
}

func NewPFV(r io.Reader, mgr *LayerManager) (*PFV, error) {
	sc := bufio.NewScanner(r)
	if !sc.Scan() {
		return nil, errors.New("img: pfv header not found")
	}
	if err := sc.Err(); err != nil {
		return nil, errors.Wrap(err, "img: unexpected error")
	}
	if sc.Text() != "[PSDToolFavorites-v1]" {
		return nil, errors.New("img: found unexpected string")
	}

	p := &PFV{
		Setting: make(map[string]string),
		Root: Node{
			Name:     "Favorites",
			Children: []Node{},
		},
	}

	var warns warning

	header := true
	var name, typ string
	var data []string
	var err error
	for sc.Scan() {
		t := sc.Text()
		if t == "" {
			continue
		}
		if len(t) > 2 && t[:2] == "//" {
			if header {
				if s, ok := p.Setting["root-name"]; ok {
					p.Root.Name = s
				}
				header = false
			} else {
				if err = insert(&p.Root, typ, name, data, mgr); err != nil {
					if w, ok := err.(warning); ok {
						warns = append(warns, w...)
					} else {
						return nil, err
					}
				}
			}
			t = t[2:]
			if pos := strings.IndexByte(t, '~'); pos != -1 {
				typ = t[pos+1:]
				name = t[:pos]
			} else {
				typ = "item"
				name = t
			}
			data = make([]string, 0)
			continue
		}
		if header {
			s := strings.SplitN(t, "/", 2)
			if len(s) != 2 {
				continue
			}
			if s[0], err = decodeName(s[0]); err != nil {
				warns = append(warns, errors.Errorf("img: %q could not decode, skipped: %v", s[0], err))
				continue
			}
			if s[1], err = decodeName(s[1]); err != nil {
				warns = append(warns, errors.Errorf("img: %q could not decode, skipped: %v", s[0], err))
				continue
			}
			p.Setting[s[0]] = s[1]
		} else {
			data = append(data, t)
		}
	}
	if err := sc.Err(); err != nil {
		return nil, errors.Wrap(err, "img: unexpected error")
	}
	if len(data) > 0 {
		if err = insert(&p.Root, typ, name, data, mgr); err != nil {
			if w, ok := err.(warning); ok {
				warns = append(warns, w...)
			} else {
				return nil, err
			}
		}
	}
	if err := registerFaview(p); err != nil {
		return nil, errors.Wrap(err, "img: unexpected error")
	}
	if warns != nil {
		return p, warns
	}
	return p, nil
}

func cloneNode(src, dest *Node) {
	dest.Name = src.Name
	dest.Open = src.Open
	dest.FilterSetting = src.FilterSetting
	dest.Setting = src.Setting
	if src.Children != nil {
		dest.Children = []Node{}
	}
	for i := range src.Children {
		dest.Children = append(dest.Children, Node{
			Parent: dest,
		})
		cloneNode(&src.Children[i], &dest.Children[i])
	}
}

func (pfv *PFV) Clone() (*PFV, error) {
	if pfv == nil {
		return nil, nil
	}

	other := &PFV{
		Setting: make(map[string]string),
		Root: Node{
			Name: pfv.Root.Name,
		},
	}
	for k, v := range pfv.Setting {
		other.Setting[k] = v
	}
	cloneNode(&pfv.Root, &other.Root)
	if err := registerFaview(other); err != nil {
		return nil, errors.Wrap(err, "img: unexpected error")
	}
	return other, nil
}

type PFVNodeSerializedData struct {
	Open bool
}

func serializeNode(n *Node, m map[string]PFVNodeSerializedData) {
	var d PFVNodeSerializedData
	d.Open = n.Open
	m[n.FullPath()] = d
	for i := range n.Children {
		serializeNode(&n.Children[i], m)
	}
}

func (pfv *PFV) serializeNode() map[string]PFVNodeSerializedData {
	m := map[string]PFVNodeSerializedData{}
	serializeNode(&pfv.Root, m)
	return m
}

func (pfv *PFV) deserializeNode(data map[string]PFVNodeSerializedData) error {
	var e warning
	for fullPath, d := range data {
		n, err := pfv.FindNode(fullPath, false)
		if err != nil {
			return err
		}
		if n != nil {
			n.Open = d.Open
		} else {
			e = append(e, errors.Errorf("img: node %q is not found", fullPath))
		}
	}
	if e != nil {
		return e
	}
	return nil
}

type PFVFaviewNodeSerializedData struct {
	SelectedName string
}

func serializeFaviewNode(fn *FaviewNode, m map[string]PFVFaviewNodeSerializedData) {
	var d PFVFaviewNodeSerializedData
	if len(fn.Items) > 0 {
		d.SelectedName = fn.SelectedName()
	}
	m[fn.FullPath()] = d
	for i := range fn.Children {
		serializeFaviewNode(&fn.Children[i], m)
	}
}

func (pfv *PFV) serializeFaviewNode() map[string]PFVFaviewNodeSerializedData {
	m := map[string]PFVFaviewNodeSerializedData{}
	serializeFaviewNode(&pfv.FaviewRoot, m)
	return m
}

func (pfv *PFV) deserializeFaviewNode(data map[string]PFVFaviewNodeSerializedData) error {
	var e warning
	for fullPath, d := range data {
		fn, err := pfv.FindFaviewNode(fullPath, false)
		if err != nil {
			return err
		}
		if fn != nil {
			for i := range fn.Items {
				if fn.Items[i].Name == d.SelectedName {
					fn.SelectedIndex = i
					break
				}
			}
		} else {
			e = append(e, errors.Errorf("img: faview node %q is not found", fullPath))
		}
	}
	if e != nil {
		return e
	}
	return nil
}

type PFVSerializedData struct {
	Node       map[string]PFVNodeSerializedData
	FaviewNode map[string]PFVFaviewNodeSerializedData
}

func (pfv *PFV) Serialize() PFVSerializedData {
	if pfv == nil {
		return PFVSerializedData{}
	}
	return PFVSerializedData{
		Node:       pfv.serializeNode(),
		FaviewNode: pfv.serializeFaviewNode(),
	}
}

func (pfv *PFV) Deserialize(data PFVSerializedData) error {
	var e warning
	if data.Node != nil {
		if err := pfv.deserializeNode(data.Node); err != nil {
			if de, ok := err.(warning); ok {
				e = append(e, de...)
			} else {
				return errors.Wrap(err, "img: failed to deserialize node")
			}
		}
	}
	if data.FaviewNode != nil {
		if err := pfv.deserializeFaviewNode(data.FaviewNode); err != nil {
			if de, ok := err.(warning); ok {
				e = append(e, de...)
			} else {
				return errors.Wrap(err, "img: failed to deserialize faview node")
			}
		}
	}
	if e != nil {
		return e
	}
	return nil
}

func (pfv *PFV) FindNode(fullPath string, ignoreRootName bool) (*Node, error) {
	if fullPath == "" {
		return nil, errors.New("img: fullPath must not be empty")
	}
	names := strings.Split(fullPath, "/")
	name, err := decodeName(names[0])
	if err != nil {
		return nil, errors.Wrapf(err, "img: failed to decode favorite root node name: %q", names[0])
	}
	if name != pfv.Root.Name && !ignoreRootName {
		return nil, nil
	}

	n := &pfv.Root

nameLoop:
	for _, encodedName := range names[1:] {
		name, err = decodeName(encodedName)
		if err != nil {
			return nil, errors.Wrapf(err, "img: failed to decode favorite node name: %q", encodedName)
		}
		for i := range n.Children {
			if n.Children[i].Name == name {
				n = &n.Children[i]
				continue nameLoop
			}
		}
		return nil, nil
	}
	return n, nil
}

func (pfv *PFV) FindFaviewNode(fullPath string, ignoreRootName bool) (*FaviewNode, error) {
	if fullPath == "" {
		return nil, errors.New("img: fullPath must not be empty")
	}
	names := strings.Split(fullPath, "/")
	name, err := decodeName(names[0])
	if err != nil {
		return nil, errors.Wrapf(err, "img: failed to decode faview root node name: %q", names[0])
	}
	if name != pfv.FaviewRoot.NameNode.Name && !ignoreRootName {
		return nil, nil
	}

	fn := &pfv.FaviewRoot

nameLoop:
	for _, encodedName := range names[1:] {
		name, err = decodeName(encodedName)
		if err != nil {
			return nil, errors.Wrapf(err, "img: failed to decode faview node name: %q", encodedName)
		}
		for i := range fn.Children {
			if fn.Children[i].NameNode.Name == name {
				fn = &fn.Children[i]
				continue nameLoop
			}
		}
		return nil, nil
	}
	return fn, nil
}

/*
func dump(n *Node, indent string) {
	ods.ODS("%s%s", indent, n.Name)
	for i := range n.Children {
		cn := &n.Children[i]
		dump(cn, indent+"  ")
	}
}
*/

func reencodeLayerName(s string) (string, error) {
	var err error
	ss := strings.Split(s, "/")
	for i := range ss {
		ss[i], err = decodeName(ss[i])
		if err != nil {
			return "", errors.Wrapf(err, "img: failed to decode layer name: %q", s)
		}
		ss[i] = encodeName(ss[i])
	}
	return strings.Join(ss, "/"), nil
}

func insert(root *Node, typ string, name string, data []string, mgr *LayerManager) error {
	var warns warning
	n, err := insertNodeRecursive(root, name)
	if err != nil {
		return err
	}
	switch typ {
	case "item":
		set := make([]bool, len(mgr.Flat))
		for _, l := range data {
			l, err = reencodeLayerName(l)
			if err != nil {
				return err
			}
			p := 0
			for {
				lp := strings.IndexByte(l[p:], '/')
				if lp == -1 {
					idx, ok := mgr.FullPath[l]
					if !ok {
						warns = append(warns, errors.Errorf("img: layer %q not found", l))
						break
					}
					set[idx] = true
					break
				}
				idx, ok := mgr.FullPath[l[:lp+p]]
				if !ok {
					warns = append(warns, errors.Errorf("img: layer %q not found", l[:lp+p]))
					break
				}
				set[idx] = true
				p += lp + 1
			}
		}
		n.Setting = set
	case "folder":
		// do nothing
	case "filter":
		set := make([]bool, len(mgr.Flat))
		for _, l := range data {
			l, err = reencodeLayerName(l)
			if err != nil {
				return err
			}
			p := 0
			for {
				lp := strings.IndexByte(l[p:], '/')
				if lp == -1 {
					idx, ok := mgr.FullPath[l]
					if !ok {
						warns = append(warns, errors.Errorf("img: layer %q not found", l))
						break
					}
					set[idx] = true
					break
				}
				idx, ok := mgr.FullPath[l[:lp+p]]
				if !ok {
					warns = append(warns, errors.Errorf("img: layer %q not found", l[:lp+p]))
					break
				}
				set[idx] = true
				p += lp + 1
			}
		}
		n.FilterSetting = set
	default:
		return errors.New("img: unexpected pfv node type: " + typ)
	}
	if warns != nil {
		return warns
	}
	return nil
}

func insertNodeRecursive(root *Node, name string) (*Node, error) {
	var err error
	cur := root
	for _, s := range strings.Split(name, "/") {
		s, err = decodeName(s)
		if err != nil {
			return nil, err
		}
		found := false
		for i := range cur.Children {
			n := &cur.Children[i]
			if n.Name == s {
				found = true
				cur = n
				break
			}
		}
		if !found {
			cur.Children = append(cur.Children, Node{
				Name:   s,
				Parent: cur,
			})
			cur = &cur.Children[len(cur.Children)-1]
		}
	}
	return cur, nil
}

type FaviewNode struct {
	NameNode *Node

	Items         []*Node
	ItemNameList  string
	SelectedIndex int
	LastModified  time.Time

	Parent   *FaviewNode
	Children []FaviewNode
}

func registerFaview(p *PFV) error {
	p.FaviewRoot.NameNode = &p.Root
	enumFaviewRoot(&p.FaviewRoot, &p.Root)
	if len(p.FaviewRoot.Children) > 0 {
		nameList := make([]string, 0, len(p.FaviewRoot.Children))
		for i := range p.FaviewRoot.Children {
			nameList = append(nameList, p.FaviewRoot.Children[i].NameNode.Name)
		}
		p.FaviewRoot.ItemNameList = strings.Join(nameList, "\x00")
	}
	return nil
}

func enumFaviewRoot(fn *FaviewNode, n *Node) {
	if len(n.Name) > 2 && n.Name[0] == '*' {
		fn.Children = append(fn.Children, FaviewNode{
			NameNode: n,
			Parent:   fn,
		})
		switch registerFaviewChildren(&fn.Children[len(fn.Children)-1]) {
		case 0:
			fn.Children = fn.Children[:len(fn.Children)-1]
		default:
			return
		}
	}
	for i := range n.Children {
		enumFaviewRoot(fn, &n.Children[i])
	}
}

func registerFaviewChildren(fn *FaviewNode) int {
	n := 0
	nameList := []string{}
	for i := range fn.NameNode.Children {
		cn := &fn.NameNode.Children[i]
		if cn.Item() {
			fn.Items = append(fn.Items, cn)
			n++
			nameList = append(nameList, cn.Name)
			continue
		}
		if cn.Filter() || cn.Folder() {
			fn.Children = append(fn.Children, FaviewNode{
				NameNode: cn,
				Parent:   fn,
			})
			n += registerFaviewChildren(&fn.Children[len(fn.Children)-1])
		}
	}
	if len(nameList) > 0 {
		fn.ItemNameList = strings.Join(nameList, "\x00")
		fn.SelectedIndex = 0
	}
	return n
}

func (fn *FaviewNode) Name() string {
	return fn.NameNode.Name
}

func (fn *FaviewNode) FullName() string {
	n := []string{}
	c := fn
	for c != nil {
		if c.Parent != nil {
			n = append(n, c.NameNode.Name)
		}
		c = c.Parent
	}
	for i, l := 0, len(n)/2; i < l; i++ {
		n[i], n[len(n)-i-1] = n[len(n)-i-1], n[i]
	}
	return strings.Join(n, "\\")
}

func (fn *FaviewNode) FullPath() string {
	n := []string{}
	c := fn
	for c != nil {
		n = append(n, encodeName(c.NameNode.Name))
		c = c.Parent
	}
	for i, l := 0, len(n)/2; i < l; i++ {
		n[i], n[len(n)-i-1] = n[len(n)-i-1], n[i]
	}
	return strings.Join(n, "/")
}

func (fn *FaviewNode) EnumItemNode() []*FaviewNode {
	r := []*FaviewNode{}
	enumItemNode(fn, &r)
	return r
}

func enumItemNode(n *FaviewNode, a *[]*FaviewNode) {
	if n.Items != nil {
		*a = append(*a, n)
	}
	for i := range n.Children {
		enumItemNode(&n.Children[i], a)
	}
}

func (fn *FaviewNode) SelectedName() string {
	return fn.Items[fn.SelectedIndex].Name
}

func (fn *FaviewNode) EncodedSelectedName() string {
	return encodeName(fn.Items[fn.SelectedIndex].Name)
}

func (fn *FaviewNode) AllName() []string {
	r := make([]string, len(fn.Items))
	for i := range fn.Items {
		r[i] = fn.Items[i].Name
	}
	return r
}

func (fn *FaviewNode) AllEncodedName() []string {
	r := make([]string, len(fn.Items))
	for i := range fn.Items {
		r[i] = encodeName(fn.Items[i].Name)
	}
	return r
}

func (fn *FaviewNode) FindItem(encodedName string) int {
	name, err := decodeName(encodedName)
	if err != nil {
		return -1
	}
	for i := range fn.Items {
		if fn.Items[i].Name == name {
			return i
		}
	}
	return -1
}

func makeFilter(f []bool, n *Node) bool {
	modified := false
	if n.Filter() {
		for i, v := range n.FilterSetting {
			if !v {
				f[i] = false
				modified = true
			}
		}
	}
	if n.Parent != nil {
		modified = makeFilter(f, n.Parent) || modified
	}
	return modified
}

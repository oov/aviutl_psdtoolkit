package img

import (
	"bufio"
	"encoding/base64"
	"encoding/binary"
	"io"
	"strings"
	"time"

	"github.com/oov/aviutl_psdtoolipc/psdtoolipc/ods"
	"github.com/pkg/errors"
)

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

func (n *Node) Item() bool   { return n.Setting != nil }
func (n *Node) Folder() bool { return n.Children != nil && n.FilterSetting == nil }
func (n *Node) Filter() bool { return n.FilterSetting != nil }

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
					return nil, err
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
				ods.ODS("%q could not decode, skipped: %v", s[0], err)
				continue
			}
			if s[1], err = decodeName(s[1]); err != nil {
				ods.ODS("%q could not decode, skipped: %v", s[1], err)
				continue
			}
			p.Setting[s[0]] = s[1]
		} else {
			data = append(data, t)
		}
	}
	if err := sc.Err(); err != nil {
		return nil, errors.Wrap(err, "pfv: unexpected error")
	}
	if err := registerFaview(p); err != nil {
		return nil, errors.Wrap(err, "pfv: unexpected error")
	}
	return p, nil
}

func dump(n *Node, indent string) {
	ods.ODS("%s%s", indent, n.Name)
	for i := range n.Children {
		cn := &n.Children[i]
		dump(cn, indent+"  ")
	}
}

func insert(root *Node, typ string, name string, data []string, mgr *LayerManager) error {
	n, err := insertNodeRecursive(root, name)
	if err != nil {
		return err
	}
	switch typ {
	case "item":
		set := make([]bool, len(mgr.Flat))
		for _, l := range data {
			p := 0
			for {
				lp := strings.IndexByte(l[p:], '/')
				if lp == -1 {
					idx, ok := mgr.FullPath[l]
					if !ok {
						ods.ODS("img: WARN: layer %q not found", l)
						break
					}
					set[idx] = true
					break
				}
				idx, ok := mgr.FullPath[l[:lp+p]]
				if !ok {
					ods.ODS("img: WARN: layer %q not found", l[:lp+p])
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
			p := 0
			for {
				lp := strings.IndexByte(l[p:], '/')
				if lp == -1 {
					idx, ok := mgr.FullPath[l]
					if !ok {
						ods.ODS("img: WARN: layer %q not found", l)
						break
					}
					set[idx] = true
					break
				}
				idx, ok := mgr.FullPath[l[:lp+p]]
				if !ok {
					ods.ODS("img: WARN: layer %q not found", l[:lp+p])
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
	SelectedIndex int32
	LastModified  time.Time

	Parent   *FaviewNode
	Children []FaviewNode
}

func registerFaview(p *PFV) error {
	enumFaviewRoot(&p.FaviewRoot, &p.Root)
	if len(p.FaviewRoot.Children) > 0 {
		nameList := make([]string, 0, len(p.FaviewRoot.Children))
		for i := range p.FaviewRoot.Children {
			nameList = append(nameList, p.FaviewRoot.Children[i].NameNode.Name)
		}
		p.FaviewRoot.ItemNameList = strings.Join(nameList, "\x00")
		p.FaviewRoot.SelectedIndex = 0
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

func (fn *FaviewNode) State() string {
	settings := fn.Items[fn.SelectedIndex].Setting
	filter := make([]bool, len(settings))
	for i := range filter {
		filter[i] = true
	}
	makeFilter(filter, fn.NameNode)

	buf := make([]byte, 2+(len(settings)*2+7)/8)
	binary.LittleEndian.PutUint16(buf, uint16(len(settings)))
	var v byte
	d := 2
	for i, pass := range filter {
		v <<= 2
		if pass {
			if settings[i] {
				v += 3
			} else {
				v += 2
			}
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
	return "F." + base64.RawURLEncoding.EncodeToString(encodePackBits(buf))
}

func makeFilter(f []bool, n *Node) {
	if n.Filter() {
		for i, v := range n.FilterSetting {
			if !v {
				f[i] = false
			}
		}
	}
	if n.Parent != nil {
		makeFilter(f, n.Parent)
	}
}

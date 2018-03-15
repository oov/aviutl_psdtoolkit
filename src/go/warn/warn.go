package warn

type Warning []error

func (e Warning) Error() string {
	var b []byte
	b = append(b, "there were some warnings:\n"...)
	for _, s := range e {
		b = append(b, s.Error()...)
		b = append(b, '\n')
	}
	return string(b)
}

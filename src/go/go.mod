module psdtoolkit

go 1.20

require (
	github.com/disintegration/gift v1.2.1
	github.com/go-gl/gl v0.0.0-20231021071112-07e5d0ea2e71
	github.com/go-gl/glfw v0.0.0-20240506104042-037f3cc74f2a
	github.com/golang-ui/nuklear v0.0.0-20231115163627-2440a671efc5
	github.com/oov/downscale v0.0.0-20170819221759-1bbcb5d498e2
	github.com/oov/psd v0.0.0-20220121172623-5db5eafcecbb
	github.com/pkg/errors v0.9.1
	golang.org/x/image v0.24.0
	golang.org/x/sys v0.30.0
	golang.org/x/text v0.22.0
)

require (
	github.com/gopherjs/gopherjs v1.17.2 // indirect
	github.com/veandco/go-sdl2 v0.4.40 // indirect
	github.com/xlab/android-go v0.0.0-20221106204035-3cc54d5032fa // indirect
)

replace github.com/golang-ui/nuklear v0.0.0-20231115163627-2440a671efc5 => github.com/oov/nuklear v0.0.0-20220408193832-85323be4ee0b

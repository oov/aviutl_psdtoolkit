module psdtoolkit

go 1.18

require (
	github.com/disintegration/gift v1.2.1
	github.com/go-gl/gl v0.0.0-20211210172815-726fda9656d6
	github.com/go-gl/glfw v0.0.0-20220622232848-a6c407ee30a0
	github.com/golang-ui/nuklear v0.0.0-20200321220456-89da3f6a587a
	github.com/oov/downscale v0.0.0-20170819221759-1bbcb5d498e2
	github.com/oov/psd v0.0.0-20220121172623-5db5eafcecbb
	github.com/pkg/errors v0.9.1
	golang.org/x/image v0.0.0-20220617043117-41969df76e82
	golang.org/x/sys v0.0.0-20220704084225-05e143d24a9e
	golang.org/x/text v0.3.7
)

require (
	github.com/gopherjs/gopherjs v1.17.2 // indirect
	github.com/veandco/go-sdl2 v0.4.24 // indirect
	github.com/xlab/android-go v0.0.0-20180723170811-ebf4d6dd1830 // indirect
)

replace github.com/golang-ui/nuklear v0.0.0-20200321220456-89da3f6a587a => github.com/oov/nuklear v0.0.0-20220408193832-85323be4ee0b

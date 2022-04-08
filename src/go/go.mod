module psdtoolkit

go 1.18

require (
	github.com/disintegration/gift v1.2.1
	github.com/go-gl/gl v0.0.0-20211210172815-726fda9656d6
	github.com/go-gl/glfw v0.0.0-20220320163800-277f93cfa958
	github.com/golang-ui/nuklear v0.0.0-20200321220456-89da3f6a587a
	github.com/gopherjs/gopherjs v0.0.0-20210202160940-bed99a852dfe // indirect
	github.com/oov/downscale v0.0.0-20170819221759-1bbcb5d498e2
	github.com/oov/psd v0.0.0-20201203182240-dad9002861d9
	github.com/pkg/errors v0.9.1
	golang.org/x/image v0.0.0-20210220032944-ac19c3e999fb
	golang.org/x/sys v0.0.0-20220406163625-3f8b81556e12
	golang.org/x/text v0.3.5
)

replace github.com/golang-ui/nuklear v0.0.0-20200321220456-89da3f6a587a => github.com/oov/nuklear v0.0.0-20220408193832-85323be4ee0b

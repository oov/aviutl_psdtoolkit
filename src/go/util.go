package main

import (
	"path/filepath"
	"strings"
)

func extractPSDAndPFV(filenames []string) string {
	var psd, pfv string
	for _, s := range filenames {
		switch strings.ToLower(filepath.Ext(s)) {
		case ".psd", ".psb":
			psd = s
		case ".pfv":
			pfv = s
		}
	}
	if psd != "" && pfv != "" && filepath.Dir(psd) == filepath.Dir(pfv) {
		psd += "|" + filepath.Base(pfv)
	}
	return psd
}

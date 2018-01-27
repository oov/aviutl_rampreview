package main

import (
	"flag"

	"github.com/oov/aviutl_rampreview/src/go/ipc"
	"github.com/oov/aviutl_rampreview/src/go/ods"
)

func main() {
	defer func() {
		if err := recover(); err != nil {
			ods.Recover(err)
		}
	}()

	flag.Parse()

	ipcm := ipc.New()
	exitCh := make(chan struct{})
	ipcm.Main(exitCh)
}

package ipc

import (
	"encoding/binary"
	"fmt"
	"io"
	"math/rand"
	"os"
	"runtime"
	"runtime/debug"
	"syscall"
	"unsafe"

	"github.com/pkg/errors"

	"github.com/oov/aviutl_rampreview/src/go/ods"
)

type IPC struct {
	cache   map[int][]byte
	storage map[string][]byte

	mappedFile syscall.Handle
	mappedView uintptr
	reply      chan error
	replyDone  chan struct{}
}

func (ipc *IPC) dispatch(cmd string) error {
	switch cmd {
	case "HELO":
		return writeUint32(0x80000000)

	case "FMO ":
		h, err := readUInt64()
		if err != nil {
			return err
		}
		if ipc.mappedView != 0 {
			if err = syscall.UnmapViewOfFile(ipc.mappedView); err != nil {
				return err
			}
			ipc.mappedView = 0
		}
		if ipc.mappedFile != 0 {
			if err = syscall.CloseHandle(ipc.mappedFile); err != nil {
				return err
			}
			ipc.mappedFile = 0
		}
		ipc.mappedFile = syscall.Handle(h)
		ipc.mappedView, err = syscall.MapViewOfFile(ipc.mappedFile, syscall.FILE_MAP_WRITE, 0, 0, 0)
		if err != nil {
			syscall.CloseHandle(ipc.mappedFile)
			ipc.mappedFile = 0
			return err
		}
		return writeUint32(0x80000000)

	case "PUT ":
		key, err := readInt32()
		if err != nil {
			return err
		}
		size, err := readInt32()
		if err != nil {
			return err
		}
		if ipc.mappedFile == 0 || ipc.mappedView == 0 {
			return errors.New("not initialized yet")
		}
		buf := make([]byte, size)
		copy(buf, ((*[1 << 49]byte)(unsafe.Pointer(ipc.mappedView)))[:size:size])
		ipc.cache[key] = buf
		return writeUint32(0x80000000)

	case "GET ":
		key, err := readInt32()
		if err != nil {
			return err
		}
		if ipc.mappedFile == 0 || ipc.mappedView == 0 {
			return errors.New("not initialized yet")
		}
		if err = writeUint32(0x80000000); err != nil {
			return err
		}
		if buf, ok := ipc.cache[key]; ok {
			copy(((*[1 << 49]byte)(unsafe.Pointer(ipc.mappedView)))[:], buf)
			err = writeInt32(int32(len(buf)))
		} else {
			err = writeInt32(0)
		}
		return err

	case "CLR ":
		ipc.cache = map[int][]byte{}
		runtime.GC()
		debug.FreeOSMemory()
		return writeUint32(0x80000000)

	case "PUTS":
		key, err := readString()
		if err != nil {
			return err
		}
		size, err := readInt32()
		if err != nil {
			return err
		}
		if ipc.mappedFile == 0 || ipc.mappedView == 0 {
			return errors.New("not initialized yet")
		}
		buf := make([]byte, size)
		copy(buf, ((*[1 << 49]byte)(unsafe.Pointer(ipc.mappedView)))[:size:size])
		ipc.storage[key] = buf
		if rand.Float32() > 0.97 {
			runtime.GC()
			debug.FreeOSMemory()
		}
		return writeUint32(0x80000000)

	case "DELS":
		key, err := readString()
		if err != nil {
			return err
		}
		delete(ipc.storage, key)
		return writeUint32(0x80000000)

	case "GETS":
		key, err := readString()
		if err != nil {
			return err
		}
		if ipc.mappedFile == 0 || ipc.mappedView == 0 {
			return errors.New("not initialized yet")
		}
		if err = writeUint32(0x80000000); err != nil {
			return err
		}
		if buf, ok := ipc.storage[key]; ok {
			copy(((*[1 << 49]byte)(unsafe.Pointer(ipc.mappedView)))[:], buf)
			err = writeInt32(int32(len(buf)))
		} else {
			err = writeInt32(0)
		}
		return err

	case "CLRS":
		ipc.storage = map[string][]byte{}
		runtime.GC()
		debug.FreeOSMemory()
		return writeUint32(0x80000000)

	case "STAT":
		var m runtime.MemStats
		runtime.ReadMemStats(&m)
		if err := writeUint32(0x80000000); err != nil {
			return err
		}
		return writeUint64(m.Alloc)
	}
	return errors.New("unknown command")
}

func (ipc *IPC) readCommand(r chan string) {
	cmd := make([]byte, 4)
	for {
		ods.ODS("wait next command...")
		if read, err := io.ReadFull(os.Stdin, cmd); err != nil || read != 4 {
			r <- fmt.Sprintf("error: %v", err)
			return
		}
		l := binary.LittleEndian.Uint32(cmd)
		if l&0x80000000 == 0 {
			break
		}
		l &= 0x7fffffff
		if l == 0 {
			ods.ODS("readCommand: reply no error")
			ipc.reply <- nil
		} else {
			buf := make([]byte, l)
			read, err := io.ReadFull(os.Stdin, buf)
			if err != nil {
				r <- fmt.Sprintf("error: %v", err)
				return
			}
			if read != int(l) {
				r <- fmt.Sprintf("error: %v", errors.New("unexcepted read size"))
				return
			}
			ods.ODS("readCommand: reply %s", string(buf))
			ipc.reply <- errors.New(string(buf))
		}
		<-ipc.replyDone
	}
	ods.ODS("readCommand: cmd %s", string(cmd))
	r <- string(cmd)
}

func (ipc *IPC) gc() {
}

func (ipc *IPC) Main(exitCh chan<- struct{}) {
	defer func() {
		if err := recover(); err != nil {
			ods.Recover(err)
		}
		if ipc.mappedView != 0 {
			syscall.UnmapViewOfFile(ipc.mappedView)
			ipc.mappedView = 0
		}
		if ipc.mappedFile != 0 {
			syscall.CloseHandle(ipc.mappedFile)
			ipc.mappedFile = 0
		}
		close(exitCh)
	}()

	cmdCh := make(chan string)
	go ipc.readCommand(cmdCh)
	for {
		select {
		case cmd := <-cmdCh:
			if len(cmd) != 4 {
				ods.ODS("%s", cmd) // report error
				return
			}
			ods.ODS("%s", cmd)
			if err := ipc.dispatch(cmd); err != nil {
				ods.ODS("error: %v", err)
				if err = writeReply(err); err != nil {
					return
				}
			}
			ods.ODS("%s END", cmd)
			go ipc.readCommand(cmdCh)
		}
	}
}

func New() *IPC {
	r := &IPC{
		cache:   map[int][]byte{},
		storage: map[string][]byte{},

		reply:     make(chan error),
		replyDone: make(chan struct{}),
	}
	return r
}

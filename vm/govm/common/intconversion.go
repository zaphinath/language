package common

import (
	"errors"
)

var (
	BufferNotLength4 = errors.New("Buffer not of length 4")
)

func Int32ToBytes(i int32) []byte {
	return []byte{
		byte(i) & 0xFF,
		byte(i>>8) & 0xFF,
		byte(i>>16) & 0xFF,
		byte(i>>24) & 0xFF,
	}
}

func BytesToInt32(buf []byte) (int32, error) {
	if len(buf) != 4 {
		return 0, BufferNotLength4
	}
	o := int32(0)
	for i, b := range buf {
		o |= int32(b) << uint32(i*8)
	}
	return o, nil
}

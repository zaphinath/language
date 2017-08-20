package debug

import (
	"fmt"
	"runtime"
)

func CallingFunction() string {
	fn, fl := functionNameAndLine(3)
	return fmt.Sprintf("calling function: %s %d", fn, fl)
}

func functionNameAndLine(n int) (string, int) {
	ptr, _, _, _ := runtime.Caller(n)
	f := runtime.FuncForPC(ptr)
	_, fl := f.FileLine(ptr)
	return f.Name(), fl
}

func PrintStackRange(n int) {
	for si := 0; si < n; si++ {
		fn, fl := functionNameAndLine(3 + si)
		fmt.Println(fn, fl)
	}
}

package codegen

import (
	"fmt"
	"io"
)

func searchList(symID string, registerList []string) bool {
	for _, s := range registerList {
		if s == symID {
			return true
		}
	}
	return false
}

func write(out io.Writer, instructions ...string) error {
	for _, i := range instructions {
		_, err := fmt.Fprintln(out, i)
		if err != nil {
			return err
		}
	}
	return nil
}

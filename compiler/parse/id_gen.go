package parse

import (
	"strconv"
)

var (
	IdGen = IdGenerator(0)
)

type IdGenerator uint64

func (id *IdGenerator) GenID() string {
	sym := makeSymbol(uint64(*id))
	(*id)++
	return sym
}

func (id IdGenerator) GetLastNID(n uint64) []string {
	ids := make([]string, n)
	for i := uint64(0); i < n; i++ {
		ids[n-i-1] = makeSymbol(uint64(id) - i - 1)
	}
	return ids
}

func makeSymbol(n uint64) string {
	return "s" + strconv.FormatUint(n, 10)
}

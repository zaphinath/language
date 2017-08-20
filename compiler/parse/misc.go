package parse

import (
	"strings"
)

const (
	arrayMarker = "[]"
)

func isArray(typeStr string) bool {
	return strings.HasSuffix(typeStr, arrayMarker)
}

func makeArrayType(typeStr string) string {
	return typeStr + arrayMarker
}

func stripBrackets(typeStr string) string {
	return strings.TrimRight(typeStr, arrayMarker)
}

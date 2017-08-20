package parse

import (
	"errors"
	"strings"
)

const (
	global         = "g"
	scopeSeparator = "."
)

var (
	mainScope = Scope("g.main")
)

type Scope string

func DefaultScope() Scope {
	return Scope(global)
}

// assumes that you haven't provided global scope
func NewScope(levels ...string) Scope {
	joinedScopes := strings.Join(levels, scopeSeparator)
	globalScope := DefaultScope()
	globalScope.AddScope(Scope(joinedScopes))
	return globalScope
}

func (s Scope) SplitScope() []string {
	return strings.Split(string(s), scopeSeparator)
}

func (s Scope) IsGlobalScope() bool {
	return s == global
}

func (s *Scope) AddScope(newScope Scope) {
	*s += scopeSeparator + newScope
}

func (s *Scope) PopScope() error {
	if s.IsGlobalScope() {
		return errors.New("Cannot delete global scope")
	}

	lastSep := strings.LastIndex(string(*s), scopeSeparator)
	if lastSep == -1 {
		return errors.New("Bad scope string: no separator")
	}

	*s = (*s)[:lastSep]

	return nil
}

func (s Scope) PeekScope() string {
	levels := s.SplitScope()
	return levels[len(levels)-1]
}

func (s *Scope) Depth() int {
	return len(s.SplitScope())
}

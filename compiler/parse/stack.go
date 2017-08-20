package parse

type stack []interface{}

func (s *stack) pop() {
	if len(*s) > 0 {
		*s = (*s)[:len(*s)-1]
	}
}

func (s *stack) push(i interface{}) {
	*s = append(*s, i)
}

func (s stack) peek() (interface{}, bool) {
	if len(s) > 0 {
		return s[len(s)-1], true
	} else {
		return nil, false
	}
}

func (s *stack) peekPop() (interface{}, bool) {
	val, ok := s.peek()
	s.pop()
	return val, ok
}

func (s *stack) empty() {
	*s = stack([]interface{}{})
}

func (s stack) isEmpty() bool {
	return len(s) == 0
}

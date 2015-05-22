package mastermind

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestCartesianProductOrderTwo(t *testing.T) {
	assert.Equal(
		t,
		[]string{"aa", "ba", "ca", "ab", "bb", "cb", "ac", "bc", "cc"},
		cartesianProduct([]string{"abc", "abc"}))
}

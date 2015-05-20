package main

import (
	"fmt"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestValidateSecretEmptySecret(t *testing.T) {
	var game = Game{
		NumOfPegs: 3,
		Symbols:   []rune{'1', '2', '3', '4'},
		Secret:    "",
	}
	var err = game.validateSecret()
	assert.Equal(t, err, fmt.Errorf("The length of the secret should be 3"))
}

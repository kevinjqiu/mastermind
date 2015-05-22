package mastermind

import (
	"fmt"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestValidateSecretEmptySecret(t *testing.T) {
	var game = Game{
		NumOfPegs: 3,
		Symbols:   "1234",
		Secret:    "",
	}
	var err = game.validateSecret()
	assert.Equal(t, err, fmt.Errorf("The length of the secret should be 3"))
}

func TestValidateSecretLongerThanPegs(t *testing.T) {
	var game = Game{
		NumOfPegs: 3,
		Symbols:   "1234",
		Secret:    "1234",
	}
	var err = game.validateSecret()
	assert.Equal(t, err, fmt.Errorf("The length of the secret should be 3"))
}

func TestValidateSecretContainsIllegalSymbol(t *testing.T) {
	var game = Game{
		NumOfPegs: 4,
		Symbols:   "1234",
		Secret:    "123a",
	}
	var err = game.validateSecret()
	assert.Equal(t, err, fmt.Errorf("The secret contains invalid symbols"))
}

func TestValidateSecretNoError(t *testing.T) {
	var game = Game{
		NumOfPegs: 4,
		Symbols:   "1234",
		Secret:    "1234",
	}
	var err = game.validateSecret()
	assert.Nil(t, err)
}

func TestValidateGuessAllCorrect(t *testing.T) {
	var game = Game{
		NumOfPegs: 4,
		Symbols:   "1234",
		Secret:    "1234",
	}
	assert.Equal(t, game.validateGuess("1234"), Result{4, 0})
}

func TestValidateGuessNoneCorrect(t *testing.T) {
	var game = Game{
		NumOfPegs: 4,
		Symbols:   "1234567890",
		Secret:    "1234",
	}
	assert.Equal(t, game.validateGuess("5678"), Result{0, 0})
}

func TestValidateGuessAllSymbolsCorrect(t *testing.T) {
	var game = Game{
		NumOfPegs: 4,
		Symbols:   "1234567890",
		Secret:    "1234",
	}
	assert.Equal(t, game.validateGuess("4321"), Result{0, 4})
}

func TestValidateGuessRepeatedSymbols(t *testing.T) {
	var game = Game{
		NumOfPegs: 4,
		Symbols:   "1234567890",
		Secret:    "1234",
	}
	assert.Equal(t, game.validateGuess("1122"), Result{1, 3})
}

func TestGenerateSolutionSpace(t *testing.T) {
	var game = Game{
		NumOfPegs: 2,
		Symbols:   "123",
	}
	assert.Equal(
		t, game.generateSolutionSpace(),
		[]string{"11", "21", "31", "12", "22", "32", "13", "23", "33"},
	)
}

func TestGenerateInitialGuess(t *testing.T) {
	var game = Game{
		NumOfPegs: 2,
		Symbols:   "123",
	}
	assert.Equal(t, game.generateInitialGuess(), "12")
}

func TestGenerateInitialGuessFourPegs(t *testing.T) {
	var game = Game{
		NumOfPegs: 4,
		Symbols:   "123",
	}
	assert.Equal(t, game.generateInitialGuess(), "1122")
}

func TestGenerateInitialGuessThreePegs(t *testing.T) {
	var game = Game{
		NumOfPegs: 3,
		Symbols:   "123",
	}
	assert.Equal(t, game.generateInitialGuess(), "112")
}

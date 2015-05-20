package main

import (
	"fmt"
	"os"
	"strings"
)

// A Result is a pair of integers indicating:
// - the number of correct symbols and positions
// - the number of correct symbols (but wrong position)
type Result [2]int

// This is the structure representing a mastermind game
type Game struct {
	NumOfPegs int
	Symbols   string
	Secret    string
}

func (game *Game) validateSecret() error {
	if len(game.Secret) != game.NumOfPegs {
		return fmt.Errorf("The length of the secret should be %d", game.NumOfPegs)
	}

	for _, s := range game.Secret {
		if !strings.ContainsRune(game.Symbols, s) {
			return fmt.Errorf("The secret contains invalid symbols")
		}
	}
	return nil
}

func (game *Game) generateInitialGuess() {
}

func (game *Game) validateGuess(guess string) Result {
	var (
		correctPositions = 0
		correctSymbols   = 0
	)

	for i, g := range guess {
		s := rune(game.Secret[i])
		if g == s {
			correctPositions += 1
		} else {
			if strings.ContainsRune(game.Secret, g) {
				correctSymbols += 1
			}
		}
	}

	return Result{correctPositions, correctSymbols}
}

func (game *Game) Solve() (int, error) {
	if err := game.validateSecret(); err != nil {
		return 0, err
	}
	return 0, nil
}

func main() {
	if len(os.Args) != 2 {
		fmt.Printf("Usage: mastermind [secret]\n")
		os.Exit(-1)
	}

	secret := os.Args[1]

	game := Game{
		NumOfPegs: 4,
		Symbols:   "123456",
		Secret:    secret,
	}

	if numSteps, err := game.Solve(); err != nil {
		fmt.Printf("%s", err)
		os.Exit(-2)
	} else {
		fmt.Printf("Solved in %s steps", numSteps)
		os.Exit(0)
	}
}

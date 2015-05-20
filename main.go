package main

import (
	"fmt"
	"math"
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

func (game *Game) generateInitialGuess() string {
	var guess []rune
	for i := 0; i < (game.NumOfPegs+1)/2; i++ {
		guess = append(guess, rune(game.Symbols[0]))
	}
	for i := 0; i < game.NumOfPegs/2; i++ {
		guess = append(guess, rune(game.Symbols[1]))
	}
	return string(guess)
}

func (game *Game) generateSolutionSpace() []string {
	sets := make([]string, game.NumOfPegs)
	for i := 0; i < game.NumOfPegs; i++ {
		sets[i] = game.Symbols
	}
	return cartesianProduct(sets)
}

func (game *Game) validateGuess(guess string) Result {
	return validateGuess(game.Secret, guess)
}

func (game *Game) Solve() (int, error) {
	if err := game.validateSecret(); err != nil {
		return 0, err
	}

	var (
		result     Result
		numGuesses int
	)

	solutionSpace := game.generateSolutionSpace()
	guess := game.generateInitialGuess()

	for {
		fmt.Printf("|solution_space| = %d\n", len(solutionSpace))
		fmt.Printf("guess = %s\n", guess)

		result = game.validateGuess(guess)
		numGuesses += 1

		fmt.Printf("result = %s\n", result)

		if result[0] == game.NumOfPegs {
			return numGuesses, nil
		}
		solutionSpace = eliminateSolutionSpace(solutionSpace, result, guess)
		if len(solutionSpace) > 0 {
			guess = solutionSpace[0]
		} else {
			panic("No candidate solution left.\n")
		}
	}
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
		fmt.Printf("Solved in %s steps\n", numSteps)
		os.Exit(0)
	}
}

func cartesianProduct(sets []string) []string {
	// Transliterated from:
	// http://stackoverflow.com/questions/2419370/how-can-i-compute-a-cartesian-product-iteratively
	var (
		i      int
		j      int
		item   []rune
		result []string
	)

	for {
		item = []rune{}
		j = i
		for _, str := range sets {
			item = append(item, rune(str[int(math.Mod(float64(j), float64(len(str))))]))
			j /= len(str)
		}
		if j > 0 {
			break
		}
		result = append(result, string(item))
		i += 1
	}

	return result
}

func validateGuess(secret string, guess string) Result {
	var (
		correctPositions int
		correctSymbols   int
	)

	for i, g := range guess {
		s := rune(secret[i])
		if g == s {
			correctPositions += 1
		} else {
			if strings.ContainsRune(secret, g) {
				correctSymbols += 1
			}
		}
	}

	return Result{correctPositions, correctSymbols}
}

func eliminateSolutionSpace(solutionSpace []string, result Result, guess string) []string {
	retval := []string{}
	for _, candidate := range solutionSpace {
		if validateGuess(candidate, guess) == result {
			fmt.Println(candidate)
			retval = append(retval, candidate)
		}
	}

	return retval
}

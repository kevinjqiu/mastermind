package main

import (
	"fmt"
	"os"
)

type Game struct {
	NumOfPegs int
	Symbols   []rune
	Secret    string
}

func (game *Game) validateSecret() error {
	if len(game.Secret) != game.NumOfPegs {
		return fmt.Errorf("The length of the secret should be %d", game.NumOfPegs)
	}
	return nil
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

	var secret = os.Args[1]

	var game = Game{
		NumOfPegs: 4,
		Symbols:   []rune{'1', '2', '3', '4', '5', '6'},
		Secret:    secret,
	}
	fmt.Printf("%s", game)
}

package main

import (
	"fmt"
	"github.com/kevinjqiu/mastermind/mastermind"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Printf("Usage: mastermind [secret]\n")
		os.Exit(-1)
	}

	secret := os.Args[1]

	game := mastermind.Game{
		NumOfPegs:        4,
		Symbols:          "123456",
		Secret:           secret,
		CandidateChooser: &mastermind.RandomCandidateChooser{},
	}

	if numSteps, err := game.Solve(); err != nil {
		fmt.Println(err)
		os.Exit(-2)
	} else {
		fmt.Printf("Solved in %d steps\n", numSteps)
		os.Exit(0)
	}
}

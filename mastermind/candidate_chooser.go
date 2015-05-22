package mastermind

import (
	"math/rand"
	"time"
)

// This is the strategy that governs how the candidate is chosen
// given a solution space
type CandidateChooser interface {
	Choose(solutionSpace []string) string
}

type RandomCandidateChooser struct {
}

func (chooser RandomCandidateChooser) Choose(solutionSpace []string) string {
	rand.Seed(time.Now().Unix())
	r := rand.Intn(len(solutionSpace))
	return solutionSpace[r]
}

type PremierCandidateChooser struct{}

func (chooser PremierCandidateChooser) Choose(solutionSpace []string) string {
	return solutionSpace[0]
}

import sys
import random


NUM_PEGS = 4
ALPHABET = "123456"
S = ["%s%s%s%s" % (a, b, c, d)
     for a in ALPHABET for b in ALPHABET
     for c in ALPHABET for d in ALPHABET]


def attempt(guess, secret):
    r, w = 0, 0
    for i in range(len(guess)):
        if guess[i] == secret[i]:
            r += 1
        else:
            if guess[i] in secret:
                w += 1
    return r, w


def solve(solution_space, secret, guess):
    num_guess = 0
    while True:
        r = attempt(secret, guess)
        num_guess += 1
        print("Response: %r" % (r,))
        if r == (NUM_PEGS, 0):
            assert guess == secret, "No, you didn't solve it..."
            print("Secret is: %s; Took %s guesses" % (guess, num_guess))
            break

        solution_space = [s for s in solution_space if attempt(s, guess) == r]

        print("Possible candidates: %s" % len(solution_space))
        guess = random.choice(solution_space)
        print("Current guess is: %s" % guess)
    return num_guess


if __name__ == '__main__':
    # if len(sys.argv) != 3:
    #     print("Usage: mastermind.py [secret] [initial_guess]")
    #     sys.exit(1)

    # secret = sys.argv[1]
    # assert len(secret) == NUM_PEGS
    # assert all(s in ALPHABET for s in secret)

    # guess = sys.argv[2]
    # solve(list(S), secret, guess)

    def generate_secret():
        return "%s%s%s%s" % (random.choice(ALPHABET), random.choice(ALPHABET),
                             random.choice(ALPHABET), random.choice(ALPHABET))

    NUM = 1000
    print(sum(
        [solve(list(S), generate_secret(), "1122") for _ in range(NUM)]) / NUM)

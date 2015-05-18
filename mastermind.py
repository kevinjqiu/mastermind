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


if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: mastermind.py [secret] [initial_guess]")
        sys.exit(1)

    secret = sys.argv[1]
    assert len(secret) == NUM_PEGS
    assert all(s in ALPHABET for s in secret)

    guess = sys.argv[2]
    num_guess = 0
    while True:
        r = attempt(secret, guess)
        num_guess += 1
        print("Response: %r" % (r,))
        if r == (NUM_PEGS, 0):
            assert guess == secret, "No, you didn't solve it..."
            print("Secret is: %s" % guess)
            print("It took %s guess" % num_guess)
            break

        S = [s for s in S if attempt(s, guess) == r]

        print("Possible candidates: %s" % len(S))
        guess = random.choice(S)
        print("Current guess is: %s" % guess)

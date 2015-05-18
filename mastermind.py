import random


ALPHABET = "123456"
S = set(["%s%s%s%s" % (a, b, c, d)
         for a in ALPHABET for b in ALPHABET
         for c in ALPHABET for d in ALPHABET])


def response(guess, secret):
    r, w = 0, 0
    for i in range(len(guess)):
        if guess[i] == secret[i]:
            r += 1
        else:
            if guess[i] in secret:
                w += 1
    return r, w


if __name__ == '__main__':
    secret = input("Secret=").lower()
    print(secret)
    assert len(secret) == 4
    assert all(s in ALPHABET for s in secret)

    guess = input("Initial Guess=")
    while True:
        r = response(secret, guess)
        print(r)
        if r == (4, 0):
            print("Secret is: %s" % guess)
            break

        S = set([s for s in S if response(s, guess) == r])

        print(S)
        guess = random.choice(list(S))
        print("Current guess is: %s" % guess)

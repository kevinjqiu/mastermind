```
    __  ______   _____________________  __  ________   ______ 
   /  |/  /   | / ___/_  __/ ____/ __ \/  |/  /  _/ | / / __ \
  / /|_/ / /| | \__ \ / / / __/ / /_/ / /|_/ // //  |/ / / / /
 / /  / / ___ |___/ // / / /___/ _, _/ /  / // // /|  / /_/ / 
/_/  /_/_/  |_/____//_/ /_____/_/ |_/_/  /_/___/_/ |_/_____/  
                                                              
```

A [Golang](http://golang.org/) implementation of the solution to the [MasterMind](http://en.wikipedia.org/wiki/Mastermind_%28board_game%29) game.

[![Build Status](https://travis-ci.org/kevinjqiu/mastermind.svg)](https://travis-ci.org/kevinjqiu/mastermind)

Usage
=====

Checkout the source code:

    git clone https://github.com/kevinjqiu/mastermind.git

Run it from source:

    go run main.go <secret>

e.g., if your secret is 1234:

    $ go run main.go 1234
    |solution_space| = 1296
    guess = 1122
    result = (1, 3)
    |solution_space| = 100
    guess = 6512
    result = (0, 2)
    |solution_space| = 21
    guess = 4221
    result = (1, 3)
    |solution_space| = 4
    guess = 1234
    result = (4, 0)
    Solved in 4 steps

You can also build the binary:

    go build -o mm

or simply use the Makefile:

    make build

and invoke it:

    ./mm 1234

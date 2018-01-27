# MiaNet [![Build Status](https://travis-ci.org/ob-fun-ws17/studienarbeit-mianet.svg?branch=master)](https://travis-ci.org/ob-fun-ws17/studienarbeit-mianet)
Haskell application implements the game "Mäxchen" (eng. = "Mia") / "Lügen"
This game can be played over the network.

Have a look at the hasekell code:
https://ob-fun-ws17.github.io/studienarbeit-mianet/

# Requirements

-   [Haskell](https://www.haskell.org/downloads)
-   on MacOS: ```$ brew install stack```

# Quickstart

## Install
download: ```git clone https://github.com/ob-fun-ws17/studienarbeit-mianet.git```
In the top directory do
    $ stack install
build: ```stack build```

## Run
To run the game you can type ```stack exec mianet-exe```

## Tests
For unit tests simply run ```stack test```

## MiaNet --- Usage
[click here for the rules](https://github.com/ob-fun-ws17/studienarbeit-mianet/blob/nils/docs/Rules.txt)


### Assignment of tasks:
| Game (Server & Client)          | Documentation    | Testing          |
| ------------------------------- | ---------------- | -----------------|
| Philipp Mayer                   | Nils Engelbrecht | Nils Engelbrecht |
| Nils Engelbrecht (Broadcasting) |                  | Philipp Mayer    |
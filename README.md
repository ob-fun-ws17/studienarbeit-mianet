# MiaNet [![Build Status](https://travis-ci.org/ob-fun-ws17/studienarbeit-mianet.svg?branch=master)](https://travis-ci.org/ob-fun-ws17/studienarbeit-mianet)
Haskell application implements the game 'Mäxchen' (eng. = 'Mia') / 'Lügen'

This game can be played over the network.

Have a look at the Haskell code:
https://ob-fun-ws17.github.io/studienarbeit-mianet/

# Requirements

-   [Haskell](https://www.haskell.org/downloads) (on MacOS: ```$ brew install stack```)

# Quickstart

## Install
(1) download

    git clone https://github.com/ob-fun-ws17/studienarbeit-mianet.git
    
(2) In the top directory do

    $ stack install
    
(3) build

    $ stack build

## Run
To run the game you can type

    $ stack exec MiaNet-exe

## Tests
For unit tests simply run

    $ stack test

## MiaNet --- Usage
[click here for the rules](https://github.com/ob-fun-ws17/studienarbeit-mianet/blob/nils/docs/Rules.txt)

    $ stack exec MiaNet-exe
    Nur Spiel beitreten (join / j) oder auch erzeugen (create / c): c
    Port eingeben: 9900
    Punkte (max): 2
    Bitte Login durchführen (login <username>)
    login userName
    Herzlich Willkommen! Spieler-Liste: userName
    userName ist dem Spiel beigetreten. Spiel starten mit (start)
    
other User:

    $ stack exec MiaNet-exe
    Nur Spiel beitreten (join / j) oder auch erzeugen (create / c): j
    Spiel gefunden: Nils-MBP.fritz.box:9900
    Bitte Login durchführen (login <username>)
    login otherUserName
    Herzlich Willkommen! Spieler-Liste: otherUserName, userName
    otherUserName ist dem Spiel beigetreten. Spiel starten mit (start)
    start
    Spiel wurde gestartet!
    Du bist am Zug. Du musst Würfeln (rolldices). (otherUserName->userName)
    rolldices
    dein Würfelergebnis: 41. Ergebnis der letzten Runde: 0. Ergebnis eingeben (logresult <würfelergebnis>)
    logresult 41
    warten auf userName
    
for help (available after min. two players are in the game and it has been started):

    $ stack exec MiaNet-exe
    ...
    ...
    help
    help      --- User: All. Vorangegangenes Kommando: -.                     Descr: Hilfe anzeigen
    chat      --- User: All. Vorangegangenes Kommando: -.                     Descr: Nachricht an alle anderen Teilnehmer senden (Single-Quotes bei Sätzen)
    actor     --- User: All. Vorangegangenes Kommando: -.                     Descr: Nachricht an Actor senden (Single-Quotes bei Sätzen)
    reactor   --- User: All. Vorangegangenes Kommando: -.                     Descr: Nachricht an Actor senden (Single-Quotes bei Sätzen)
    chatall   --- User: All. Vorangegangenes Kommando: -.                     Descr: Nachricht an alle Teilnehmer senden (Single-Quotes bei Sätzen)
    rolldices --- User: Actor. Vorangegangenes Kommando: -.                   Descr: Kommando zum Würfeln
    getwin    --- User: All. Vorangegangenes Kommando: -.                     Descr: Ergebnis eines bestimmten Spielers abfragen
    help      --- User: All. Vorangegangenes Kommando: -.                     Descr: Hilfe anzeigen
    logresult --- User: Actor. Vorangegangenes Kommando: rolldices.           Descr: Würfelergebnis oder Lüge kann eingegeben werden (Eingabe > Ergebnis vorangegangener Runde)
    nextdraw  --- User: Reactor. Vorangegangenes Kommando: rolldices (Actor). Descr: Dem Actor wird geglaubt --> nächste Runde beginnt
    accuse    --- User: Reactor. Vorangegangenes Kommando: rolldices (Actor). Descr: Dem Actor wird nicht geglaubt --> Lüge: Punkt Reactor; Wahrheit: Punkt Actor
    start     --- 
    stop      --- 
    closeGame --- 
    rematch   --- 


### Assignment of tasks:
| Game (Server & Client)          | Documentation           | Testing          |
| ------------------------------- | ----------------------- | -----------------|
| Philipp Mayer                   | Nils Engelbrecht        | Nils Engelbrecht |
| Nils Engelbrecht (Broadcasting) | Philipp Mayer (Haddock) | Philipp Mayer    |

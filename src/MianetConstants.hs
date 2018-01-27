{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{- |
Module      : MianetConstants
Description : This module is mainly used for storing commands listed in help.
Copyright   : BSD3
License     : 2018 Philipp Mayer & Engelbrecht Nils
Maintainer  : Philipp Mayer
-}
module MianetConstants where

import Types

-- | actions method returns a list of actions.
actions :: Action
actions = [
            --0
            ("help", 0, "User: All. Vorangegangenes Kommando: -. Descr: Hilfe anzeigen"),
            --1
            ("chat", 1, "User: All. Vorangegangenes Kommando: -. Descr: Nachricht an alle anderen Teilnehmer senden (Single-Quotes bei Sätzen)"),
            --2
            ("actor", 1, "User: All. Vorangegangenes Kommando: -. Descr: Nachricht an Actor senden (Single-Quotes bei Sätzen)"),
            --3
            ("reactor", 1, "User: All. Vorangegangenes Kommando: -. Descr: Nachricht an Actor senden (Single-Quotes bei Sätzen)"),
            --4
            ("chatall", 1, "User: All. Vorangegangenes Kommando: -. Descr: Nachricht an alle Teilnehmer senden (Single-Quotes bei Sätzen)"),
            --5
            ("rolldices", 0, "User: Actor. Vorangegangenes Kommando: -. Descr: Kommando zum Würfeln"),
            --6
            ("getwin", 1, "User: All. Vorangegangenes Kommando: -. Descr: Ergebnis eines bestimmten Spielers abfragen"),
            --7
            ("help", 0, "User: All. Vorangegangenes Kommando: -. Descr: Hilfe anzeigen"),
            --8
            ("logresult", 1, "User: Actor. Vorangegangenes Kommando: rolldices. Descr: Würfelergebnis oder Lüge kann eingegeben werden (Eingabe > Ergebnis vorangegangener Runde)"),
            --9
            ("nextdraw", 0, "User: Reactor. Vorangegangenes Kommando: rolldices (Actor). Descr: Dem Actor wird geglaubt --> nächste Runde beginnt"),
            --10
            ("accuse", 0, "User: Reactor. Vorangegangenes Kommando: rolldices (Actor). Descr: Dem Actor wird nicht geglaubt --> Lüge: Punkt Reactor; Wahrheit: Punkt Actor"),
            --11
            ("start", 0, ""),
            --12
            ("stop", 0, ""),
            --13
            ("closeGame", 0, ""),
            --14
            ("rematch", 0, "")
            ]

-- | initiates the serverState.
newServerState :: ServerState
newServerState = []

-- | initiates the last draw base.
lastDrawBase :: (Int, Int, Int)
lastDrawBase = (0, 0, 0)

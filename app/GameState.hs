{-# LANGUAGE OverloadedRecordDot #-}
module GameState
(GameState(State), 
battlefield,
stack,
hand,
graveyard,
library,
exile,
lifeTotals,
updateGameState,
initialState,
eID,
-- update,
)where 

import Card

data GameState = 
    State
    {battlefield:: [Entity],
    graveyard:: [Entity],
    hand:: [Entity],
    exile:: [Entity],
    library::[Entity],
    stack::[Entity],
    lifeTotals::[Int]
    }








initialState :: GameState
initialState = State {battlefield=[], graveyard=[], hand=[], exile=[], library=[], stack=[], lifeTotals=[]}

updateGameState::GameState->GameState->GameState
updateGameState state newstate=
  State{battlefield=newstate.battlefield, graveyard=newstate.graveyard, hand=newstate.hand, exile=newstate.exile, library=newstate.library, stack=newstate.stack, lifeTotals=newstate.lifeTotals}

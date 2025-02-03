{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Apecs
import GameState
import Card
import CreatureType
--import OverloadedRecordDot


data GameAction = CastSpell
        | PlayLand
        | ActivateAbility
        deriving Eq





data Player = 
        PlayerValues{
                energy :: Int, 
                poison :: Int,
                whitemana::Int,
                bluemana::Int,
                blackmana::Int,
                redmana::Int,
                greenmana::Int,
                colourlessmana::Int
        }

        

-- validcast:: Card -> Zone->Zone
-- validcast (Card cn ct t tgts) = 


--data Graveyard = End
    --    | Card CardType Maybe PermanentType Graveyard




-- type Permanent = Int
-- type Instant = Int
-- type Sorcery = Int
-- type CardName = String
-- type Controller = String
-- type Text = String



--this is more like a shallow embedding, try to change it to deep
-- card :: CardName -> CardType->Maybe PermanentType->Text ->Controller(Permanent, Instant, Sorcery)->(Permanent, Instant, Sorcery)
-- card "Grizzly Bear" Permanent Creature(p, i, s) = ((p+1), i, s)
-- card "Counterspell" Instant None (p,i,s)=(p,(i+1),s)
-- card "Divination" Sorcery None (p,i,s)=(p,i,(s+1))
-- card "Sol Ring" Permanent Artifact (p, i, s) = ((p+1), i, s)
-- card "Counterbalance" Permanent Enchantment (p, i, s) = ((p+1), i, s)
-- card "Wastes" Permanent Land (p, i, s) = ((p+1), i, s)

-- playLand :: GameState->Entity->Maybe GameState
-- playLand gs l = gs.battlefield ++ [l]

-- cast :: GameState->Entity -> Maybe GameState
-- cast gs c 
--         | c == Castable = gs.stack ++ [c]
--         | c != Castable = Nothing

-- counter::GameState->Entity->Maybe GameState
-- counter gs t
--         | !(elem t gs.stack) = gs.stack -- some kind of failure message?
--         | elem t gs.stack = removeItem t gs.stack


-- removeItem::Entity->[Entity]->[Int]
-- removeItem _ []                 = []
-- removeItem x (y:ys) | x.eID == y.eID    = removeItem x ys
--                     | otherwise = y : removeItem x ys

-- spendMana :: Player -> [Int] -> System' ()
-- spendMana p [[x][][][][][][]] = p.whitemana -=x
-- --etc ^
-- --mana is hard because of so many possibilities (1, W, 1W, 1WR etc)
-- --do i run it once for each symbol in the cost? maybe

-- resolveStack :: GameState->GameState
-- resolveStack [] = []
-- resolveStack [stack:stacks] = "resolve 'OnResolution' effects(?), then resolveStack stacks"

--resolveCard :: Entity->GameState->GameState
-- ?? i need a way to interpret the onResolve string or otherwise use a type for it
--can do this recursively through the list? but requires me to list onResolves in some priority order 

goToBattlefield :: Entity->GameState->GameState

--this isn't going to work exactly because of my imperative instincts, try and ask e+j how to work at this
--finding it really hard to make cast work when i can't direct access the stack

--how to interpret text in haskell?


-- initialise :: System' ()
-- initialise = do


--   liftIO $ putStrLn "testIO"


--the below is all imperative garbage.
main::IO()
main=do
        let state = initialState 
        let card1 = 
                Card
                {cardName="Kobolds of Kher Keep",
                cardType = "Permanent",
                manaCost = [0,0,0,0,0,0,0], -- WUBRG, then strict colorless, then any
                permanentType="Creature",
                keywords = [],
                text = "This card is a red spell when cast and Kobolds are a red creature",
                colour = "Red",
                onCast=[],
                onResolve = ["GoToBattlefield"],
                targetZones=[]
                }
        let newcard = card card1 0
        let card2 = 
                Card{cardName="Counterspell",
                cardType = "Instant",
                manaCost = [0,2,0,0,0,0,0], -- BB
                permanentType="None",-- shouldn't need to be here but see Card.hs for explanation
                keywords = [],
                text = "Counter target spell",
                colour = "Blue",
                onCast = ["DeclareTargets"],
                onResolve = ["CounterTargets","GoToGrave"],
                targetZones=[state.stack]--i dont know how to initialise a card targeting a zone which doesn't yet exist..
                }
        let newcard2 = card card2 1
        
        let card3 =
                Card{cardName="Sol Ring",
                cardType = "Permanent", 
                --i need to somehow develop a class of classes? maybe? ie having Permanents just naturally have their own attributes..
                --would also let me have the GoTox and targetZones values simplified
                manaCost = [0,0,0,0,0,0,1], -- 1 generic
                permanentType="Artifact",
                keywords = [],
                text = "T: Add 2 colourless mana",
                colour = "Colourless",
                onCast = [],
                onResolve = ["GoToBattlefield"],
                targetZones=[]
                }
        let newcard3 = card card3 2
        let newstate =  state {hand = [newcard, newcard2, newcard3]}
        updateGameState state newstate
        --I REALLY HATE DO BLOCKS WHY MUST ALL CONVERGE TO IO

        --this is horrendously inefficient but it's just for the test build
        -- once i have a list of cards this goes in a reproducible function loop
        

        

        let result = ((head newstate.hand).eID)
        print result
        -- let result1 = 12
        -- print result1
        -- i have to assume this is working because i cannot for the life of me find a way to prove it, hs wont print to terminal


        
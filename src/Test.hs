{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedRecordDot    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Test where

import SimpleCard
import Apecs
-- import Data.List
-- import Control.Monad.IO.Class
import Control.Monad
-- import Apecs.Core
-- import Debug.Trace

--thoughts on making a resolveStack function exclusively for testing, which doesnt return the user to the menu system?
--only came up with this late monday, i think i want to implement it

--this system currently assumes that testSetup can properly reset the gamestate from simplecard
test1 :: System'()
test1 =do
    testSetup
    liftIO(print("this is test 1: Populating Libraries"))
    cil<-cardsinlib
    forM_ (cil) $ \(card) -> do
        liftIO(print(card.cardName))
--test that cards are placed into the library properly on creation

test2 :: System'()
test2 = do
    testSetup
    liftIO(print("this is test 2: Drawing Cards"))
    drawcards 2
    cih<-cardsinhand
    forM_ (cih) $ \(card) -> do
        liftIO(print(card.cardName))
    cil<-cardsinlib
    forM_ (cil) $ \(card) -> do
        liftIO(print(card.cardName))
--test that cards are drawn properly
--past here, cards aren't drawn unless by a card effect, as castCard can bypass cardsinhand purely for testing purposes
--this should NOT be possible in the actual setup

test3 :: System'()
test3  = do
    testSetup
    liftIO(print("this is test 3: Playing Lands"))
    playLand basicLand
    cib<-cardsonbattlefield
    forM_ (cib) $ \(card) -> do
        ACardName name <- getComponent card 
        liftIO(print(name))
--test that lands are played properly

test4 :: System'()
test4 = do
    testSetup
    liftIO(print("this is test 4: Casting Cards"))
    addmana 1
    castCard basicGuy
    cis<-cardsonstack
    forM_ (cis) $ \(card) -> do
        ACardName name <- getComponent card 
        --why is this not working??
        liftIO(print(name))
--test that cards are cast properly

manamorphose :: Card
manamorphose = Card {
    cardName = "Manamorphose",
    cardType = Sorcery,
    manaCost=2,
    onActivate = [],
    onEvent = [],
    additcost = Nothing,
    onResolve = [(Then (AddMana 2) (DrawCards 1))],
    possibleTargets = Nothing,
    attributes = [],
    currentZone = None
}

test5 ::System'()
test5 = do
    testSetup
    liftIO(print("this is test 5: Resolving the Stack"))
    addmana 3
    playLand basicLand
    addmana 1
    bg<-castnewcard basicGuy []
    AManaCost mc<-getComponent bg
    paymana mc
    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (bg : stackcards)
    mmp<-castnewcard manamorphose []
    AManaCost mmc<-getComponent mmp
    paymana mmc
    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (mmp : stackcards)
    resolveEffect All
    showGS
--test that the stack resolves properly

test6 :: System'()
test6 = do
    testSetup
    liftIO(print("this is test 6: Targeting and Countering"))
    addmana 3
    bg<-castnewcard basicGuy []
    AManaCost mc<-getComponent bg
    paymana mc
    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (bg : stackcards)
    cs<-castnewcard counterspell [bg]
    AManaCost manc<-getComponent cs
    paymana manc
    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (cs : stackcards)
    resolveEffect All
    showGS
    liftIO(print("Memnite should NOT be on the battlefield."))
--test that spells can be targeted (and countered)

test7 :: System'()
test7= do
    testSetup
    liftIO(print("this is test 7: Targeting and Destruction"))
    addmana 3
    bg<-castnewcard basicGuy []
    AManaCost mc<-getComponent bg
    paymana mc
    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (bg : stackcards)
    resolveEffect All
    vind<-castnewcard vindicate [bg]
    AManaCost manc<-getComponent vind
    paymana manc
    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (vind : stackcards)
    resolveEffect All
    showGS
    liftIO(print("Memnite should NOT be on the battlefield."))
--test that permanents can be targeted (and destroyed)

test8 :: System'()
test8= do
    testSetup
    liftIO(print("this is test 8: Triggered Abilities"))
    addmana 2
    liftIO(print("current mana: (should be 2)"))
    cmapM $ \ (CurrentMana cm)->liftIO(print(cm))
    lc <- castnewcard lotusCobra []
    AManaCost mc <- getComponent lc
    paymana mc
    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (lc : stackcards)
    resolveEffect All
    showGS
    playLand basicLand
    --this one will go to the menu but there's not much way around it, that's how triggers are handled, and it proves it works
    liftIO(print("current mana: (should be 1)"))
    cmapM $ \ (CurrentMana cm)->liftIO(print(cm))

--test that triggered abilities will activate and resolve

test9 :: System'()
test9= do
    testSetup
    liftIO(print("this is test 9 Activated Abilities"))
    addmana 1
    liftIO(print("current mana: (should be 1)"))
    cmapM $ \ (CurrentMana cm)->liftIO(print(cm))
    sr<-castnewcard solRing []
    AManaCost mc <- getComponent sr
    paymana mc
    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (sr : stackcards)
    showStack
    resolveEffect All
    activateAbility sr
    showStack
    resolveEffect All
    liftIO(print("current mana: (should be 2)"))
    cmapM $ \ (CurrentMana cm)->liftIO(print(cm))
    
--test that activated abilities will activate and resolve

-- castcardx :: Int->System'()
-- castcardx 0 = return()
-- castcardx x = do
--     testSetup
--     bg<-castnewcard basicGuy []
--     cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (bg : stackcards)
--     resolveEffect All
--     castcardx (x-1)

-- test10 :: System'()
-- test10= do
--     testSetup
--     liftIO(print("this is test 10: Scute Swarm"))
--     castcardx 10
--     showGS

runtest :: System'()->IO()
runtest test = do
    w<-initWorld
    runWith w test

runtests:: IO()
runtests = do
    runtest test9

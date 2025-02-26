{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedRecordDot    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module SimpleCard where

import Apecs
import Data.List
import Control.Monad.IO.Class
import Control.Monad
import Apecs.Core

-- import SimpleCard (initWorld)

data CardType = Instant
        | Sorcery
        | Creature
        | Artifact
        | Enchantment
        | Land
        deriving (Eq, Show)


data Card =
    Card
    {   cardName :: String,
        cardType :: CardType,
        manaCost::Int,
        onActivate :: [(Cost,Action)],
        onEvent :: [(Trigger, Action)],
        onResolve :: [Action],
        possibleTargets :: Maybe TargetType,
        attributes :: [Attribute],
        currentZone :: Zone
    } deriving Show

appendcard :: Card->[Card]->[Card]
-- appendcard thiscard cards = foldr (:) [thiscard] cards
appendcard thiscard cards = cards ++ [thiscard]
--i think this blue line is wrong but correct me if thats not the case

data Zone = Hand | Battlefield | Graveyard | Library | Exile | Stack | None deriving Show

data Action = GoToBattlefield| CounterTarget | DestroyTarget| AddMana Int| DrawCards Int | DoNothing | Then Action Action deriving Show

data Colour = White | Blue | Black | Red | Green | Colourless | And Colour Colour

data Cost = Tap deriving Show

data Trigger = Landfall | Enters deriving Show

data TargetType = Spell | Permanent deriving (Show, Eq)

data Attribute =
    BasePower Int
    | BaseToughness Int
    deriving Show

data AbilityAttribute =
    Source Entity
    | SourceColour Colour
    | SourceCardType CardType


basicLand :: Card
basicLand = Card {
    cardName = "BasicLand",
    cardType = Land,
    manaCost=0,
    onActivate = [(Tap,AddMana 1)],
    onEvent = [],
    onResolve = [],
    possibleTargets = Nothing,
    attributes = [],
    currentZone = None
}

counterspell :: Card
counterspell = Card {
    cardName = "Counterspell",
    cardType = Instant,
    manaCost=2,
    onActivate = [],
    onEvent = [],
    onResolve = [CounterTarget],
    possibleTargets = Just Spell,
    attributes = [],
    currentZone = None
}

divination :: Card
divination = Card {
    cardName = "Divination",
    cardType = Sorcery,
    manaCost=3,
    onActivate = [],
    onEvent = [],
    onResolve = [(DrawCards 2)],
    possibleTargets = Nothing,
    attributes = [],
    currentZone = None
}

basicGuy :: Card
basicGuy = Card {
    cardName = "Memnite",
    cardType = Creature,
    manaCost=0,
    onActivate = [],
    onEvent = [],
    onResolve = [GoToBattlefield],
    possibleTargets = Nothing,
    attributes = [BasePower 1, BaseToughness 1],
    currentZone = None

}

solRing :: Card
solRing = Card {
    cardName = "Sol Ring",
    cardType = Artifact,
    manaCost=1,
    onActivate = [(Tap, AddMana 2)],
    onEvent = [],
    onResolve = [GoToBattlefield],
    possibleTargets = Nothing,
    attributes = [],
    currentZone = None

}

newtype CurrentMana = CurrentMana Int deriving (Show)
instance Component CurrentMana where type Storage CurrentMana = Unique CurrentMana
--i tried this as a global but things got weird so it's a unique

data IsCard = IsCard deriving Show
instance Component IsCard where type Storage IsCard = Map IsCard

data IsAbility = IsAbility deriving Show
instance Component IsAbility where type Storage IsAbility = Map IsAbility

newtype AbilityCost = AbilityCost Cost deriving Show
instance Component AbilityCost where type Storage AbilityCost = Map AbilityCost

newtype AbilityEffect = AbilityEffect Action deriving Show
instance Component AbilityEffect where type Storage AbilityEffect = Map AbilityEffect

newtype AbilityAttributes = AbilityAttributes Entity deriving Show
instance Component AbilityAttributes where type Storage AbilityAttributes = Map AbilityAttributes

data TheLibrary = TheLibrary deriving Show
instance Component TheLibrary where type Storage TheLibrary = Unique TheLibrary

data TheExile = TheExile deriving Show
instance Component TheExile where type Storage TheExile = Unique TheExile

data TheHand = TheHand deriving Show
instance Component TheHand where type Storage TheHand = Unique TheHand

data TheStack = TheStack deriving Show
instance Component TheStack where type Storage TheStack = Unique TheStack

data TheBattlefield = TheBattlefield deriving Show
instance Component TheBattlefield where type Storage TheBattlefield = Unique TheBattlefield

data TheGraveyard = TheGraveyard deriving Show
instance Component TheGraveyard where type Storage TheGraveyard = Unique TheGraveyard

newtype ACardName = ACardName String deriving Show
instance Component ACardName where type Storage ACardName = Map ACardName

newtype ACardType = ACardType CardType
instance Component ACardType where type Storage ACardType = Map ACardType

newtype AManaCost = AManaCost Int deriving Show
instance Component AManaCost where type Storage AManaCost = Map AManaCost

newtype AnOnActivate = AnOnActivate [(Cost,Action)]
instance Component AnOnActivate where type Storage AnOnActivate = Map AnOnActivate

newtype AnOnEvent = AnOnEvent [(Trigger,Action)]
instance Component AnOnEvent where type Storage AnOnEvent = Map AnOnEvent

newtype AnAttributes = AnAttributes [Attribute]
instance Component AnAttributes where type Storage AnAttributes = Map AnAttributes

newtype AZone = AZone Zone
instance Component AZone where type Storage AZone = Map AZone

newtype CardsinZone = CardsinZone [Card]
instance Component CardsinZone where type Storage CardsinZone = Map CardsinZone

newtype EntitiesinZone = EntitiesinZone [Entity]
instance Component EntitiesinZone where type Storage EntitiesinZone = Map EntitiesinZone

newtype CurrentTargets = CurrentTargets [Entity]
instance Component CurrentTargets where type Storage CurrentTargets = Map CurrentTargets

makeWorld "World" [

    ''IsCard, ''ACardName, ''ACardType, ''AManaCost, ''AnOnActivate, ''AnOnEvent, ''AnAttributes, ''AZone,
 ''TheHand, ''TheGraveyard, ''TheBattlefield, ''TheStack, ''TheLibrary, ''TheExile, ''CardsinZone, ''CurrentTargets, ''CurrentMana,
    ''EntitiesinZone, ''AbilityAttributes, ''AbilityEffect, ''AbilityCost, ''IsAbility
    ]

type System' a = System World a
main :: IO ()
main = do
    w <- initWorld
    runWith w setupGame


setupGame :: System'()
setupGame = do
    createZones
    createManaBank
    -- putCardInPlace basicLand None Hand 
    -- putCardInPlace solRing None Hand
    --putCardInPlace divination None Library
    -- putCardInPlace basicGuy None Hand
    --putCardInPlace counterspell None Library
    populateLibrary [basicLand, basicGuy]
    castCard basicGuy
    castCard basicGuy
    castCard basicGuy
    castCard basicGuy
    cs<-cardsonstack

    forM_ (zip [1 ..] cs) $ \(ix, c) -> do
        ACardName name <- getComponent c
        liftIO (print (show ix ++ ". " ++ name))

    target <- obtainInput ("pick one")
    if target > length cs
        then return ()
        else do
            let entity = cs !! target
            debug entity
            return ()

debug :: Show a => a -> System' ()
debug x = liftIO (print x)

cdebug :: (Show a, Get w m c, Members w m c, MonadIO m)
       => (c -> a) -> SystemT w m ()
cdebug f = cmapM_ (\x -> liftIO (print (f x)))

getComponent :: forall c. (Has World IO c, ExplGet IO (Storage c)) => Entity -> System' c
getComponent entity = get entity

cardsonbattlefield :: System' [[Entity]]
cardsonbattlefield = collect $ \ (TheBattlefield, EntitiesinZone entities) -> Just entities


cardsinhand :: System' [[Card]]
cardsinhand = collect $ \ (TheHand, CardsinZone cards) -> Just cards

cardsinlib :: System' [Card]
cardsinlib = do
    cil<-collect $ \ (TheLibrary, CardsinZone cards) -> Just cards
    return (concat cil)

cardsonstack :: System' [Entity]
cardsonstack = do
    css <- collect $ \ (TheStack, EntitiesinZone entities) -> Just entities
    return (concat css)

effectsonstack :: System' [(Entity,[(Trigger, Action)])]
effectsonstack = do
    theents<-collect $ \ (TheStack, EntitiesinZone entities) -> Just (entities)
    forM (concat theents) $ \(e) -> do
        AnOnEvent triggeraction<- getComponent e
        return (e, triggeraction)

--I need to get specifically the entity that is having its triggeractions collected somehow

createManaBank :: System'()
createManaBank = do
    newEntity_
        (CurrentMana 0)

createZones :: System'()
createZones= do
    newEntity_
        (TheHand,

        CardsinZone []
        )
    newEntity_
        (
        TheGraveyard,
        CardsinZone []
        )
    newEntity_
        (TheLibrary,

        CardsinZone []
        )
    newEntity_
        (
        TheBattlefield,
        EntitiesinZone []
        )
    newEntity_
        (
        TheExile,
        CardsinZone []
        )
    newEntity_
        (
        TheStack,
        EntitiesinZone []
        )

populateLibrary ::[Card] ->System'()
populateLibrary cards = cmap $ \ (TheLibrary, CardsinZone cards') -> CardsinZone cards

newcard :: Card-> System' ()
newcard card= do
    newEntity_
        (IsCard,
        ACardName card.cardName,
        ACardType card.cardType,
        AManaCost card.manaCost,
        (AnOnActivate card.onActivate,
        AnOnEvent card.onEvent),
        AnAttributes card.attributes,
        CurrentTargets [],
        AZone Battlefield
        )
castnewcard :: Card->[Entity]-> System' Entity
castnewcard card targets= do
    newEntity
        (IsCard,
        ACardName card.cardName,
        ACardType card.cardType,
        AManaCost card.manaCost,
        (AnOnActivate card.onActivate,
        AnOnEvent card.onEvent),
        AnAttributes card.attributes,
        CurrentTargets targets,
        AZone Stack
        )

playLand :: Card->System'()
playLand land = do
    newcard land
    --putCardInPlace land.cardID Hand Battlefield


castCard :: Card->System'()
castCard card = do
    case card.possibleTargets of 
        Nothing -> do
            entity<-castnewcard card []
            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (entity : stackcards)
            
        Just targetType -> do
            target<-choosetarget targetType
            entity<-castnewcard card [target]
            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (entity : stackcards)
            
    

    --resolve its onCast trigger

resolveStack :: System'()
resolveStack = do
    triggeractions<-effectsonstack
    return()
    --resolvingTriggeractions triggeractions

drawcard :: System'()
drawcard = cmapM $ \(TheLibrary, CardsinZone cards) ->
    case cards of
        [] -> return (CardsinZone cards)
        (card : cards') -> do
            cmap $ \ (TheHand, CardsinZone handcards) -> CardsinZone (appendcard card handcards)
            return (CardsinZone cards')

drawcards :: Int->System'()
drawcards 0 = return ()
drawcards x = do
    drawcard
    drawcards (x-1)

addmana :: Int->System'()
addmana x = cmap $ \(CurrentMana mananow) -> CurrentMana (mananow+x)

paymana :: Int->System'()
paymana x = cmap $ \(CurrentMana mananow) -> CurrentMana (mananow-x)

setmana :: Int->System'()
setmana x = cmap $ \(CurrentMana mananow) -> CurrentMana x

obtainInput :: Read a => String->SystemT World IO a --this might come back to bite me?
obtainInput prompt = do
    liftIO (putStrLn prompt)
    target<-liftIO (getLine)
    return (read target)

findtarget :: TargetType->System' [[Entity]]
findtarget targetType= collect $ \ (TheStack, EntitiesinZone entities) -> Just (entities)
findtarget Permanent = collect $ \ (TheBattlefield, EntitiesinZone entities) -> Just (entities)
--i know for a fact there's a way to filter collect but the syntax is being annoying and it's 2am


choosetarget :: TargetType->System' Entity
--do run a function that prompts the user to target something, and feed it back into this 
choosetarget (targetType) = do
    targets<- (findtarget (targetType))
    getUserChoice (concat targets) (\target -> do
            ACardName name <- getComponent target
            return name
        )

getUserChoice :: [a] -> (a -> System' String) -> System' a
getUserChoice options display = do
    forM_ (zip [1 ..] options) $ \(ix, option) -> do
        optionText <- display option
        liftIO (print (show ix ++ ". " ++ optionText))
    target <- obtainInput ("pick one")
    let option = options !! target
    if target > length options
        then do
            liftIO (putStrLn "invalid option!")
            getUserChoice options display
        else do
            return option

-- these have the same issue as above, can't convert system' string to String

getTargets :: Entity->System'[Entity]
getTargets card = do
    CurrentTargets targets <- getComponent card
    return targets

getName :: Entity->System'(String)
getName card = do
    ACardName targets <- getComponent card
    return targets

deletethese :: [Entity]->[Entity]->[Entity]
deletethese targets list = foldr delete targets list--do
--     forM (targets) $ \(target) -> do
--         delete target list


counterTargets :: Entity->System'()
counterTargets card = do
    cardtargets<-getTargets card
    cmapM $ \(TheStack, EntitiesinZone cards) ->
        case cards of
            [] -> return ()
            (thiscard : cards') -> do
                cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (deletethese cardtargets stackcards)

destroyTargets :: Entity->System'()
destroyTargets card = do
    cardtargets<-getTargets card
    cmapM $ \(TheBattlefield, EntitiesinZone cards) ->
        case cards of
            [] -> return ()
            (thiscard : cards') -> do
                cmap $ \ (TheBattlefield, EntitiesinZone stackcards) -> EntitiesinZone (deletethese cardtargets stackcards)
-- not going to work right now as can't track if individual cardtargets are in cards' -> a) loop through each cardtarget and b) delete them individually           


--currently only works on one target, ig just do this multiple times for each target?

parseaction :: (Action,Entity)->System'()
parseaction (DoNothing, _) = return ()
parseaction (DrawCards x, _) = drawcards x
parseaction (DestroyTarget, entity)= destroyTargets entity 
parseaction (CounterTarget, entity)=counterTargets entity 
parseaction (AddMana x, _) = addmana x
parseaction (Then x y, z) = do
    parseaction (x,z)
    parseaction (y,z)

--[(Entity,[(Trigger,Action)])]
-- resolvingTriggeractions :: [(Entity,[(Trigger,Action)])] -> System'()
-- resolvingTriggeractions [] = parseaction (DoNothing,0)
-- resolvingTriggeractions ((name,[(OnResolve,action)]):tas) = do
--     parseaction (action,name)
--     resolvingTriggeractions tas
-- resolvingTriggeractions ((name,[(OnCast,_)]):tas) = do
--     parseaction (DoNothing,name)
--     resolvingTriggeractions tas
-- resolvingTriggeractions _ = parseaction (DoNothing,0)

--we've differentiated onResolves from triggeractions, they resolve differently i think

-- getOnActivates :: Entity -> [(Cost,Action)]
-- getOnActivates card = 
--   let AnOnActivate abilities = get card
--    in abilities

-- activateEffect :: Entity->System'()
-- activateEffect card = do 
-- effects<-getOnActivates card
-- whicheffect <- obtainInput "which effect to use?"
-- make effects their own entity, use the 1... from up top to cycle through them



-- iterateTriggeractions [e,[(trigger,action)]:tas] = if trigger = onresolve: pass action into a parsing function which carries it out, then iterateTriggeractions tas
--i THINK i need to separate oncast and onresolve again, because they're triggered in entirely different circumstances, and it makes a generic parsing function really rough
--do this recursively? go through, try and resolve their onResolves

--we can clear entities by returning Nothing instead of a component adjustment
--but how exactly am i going to get that specific targeted entity? 

--the only place entities exist is stack/battlefield, gy doesnt exist. library/hand just store Cards. 
--resolveStack !!

--all three of these effectively should function in sequence to update a card's position, wasn't sure how cmap works 
--(ie if i could resolve these in sequence with one cmap) 
--because the documentation seems shockingly vacant
-- putCardInPlace :: Card->Zone->Zone-> System'()
-- putCardInPlace thisid zone newzone = cmap $ \(AnId thisid, AZone zone) -> AZone newzone

--pretty sure this ^ is just a setup command now

-- addCardToPlace :: Card->Zone->System'()
-- addCardToPlace card zone = cmapIf (\(WhichZone thezone)->thezone==zone) 
--                                 (\(WhichZone thezone, CardsinZone cards)->CardsinZone (appendcard card cards))

-- removeCardFromPlace :: Card->Zone->System'()
-- removeCardFromPlace card zone = cmapIf (\(WhichZone thezone)->thezone==zone) 
--                                 (\(WhichZone thezone, CardsinZone cards)->CardsinZone (removeCard card cards))



-- gameChecks :: System' ()
-- gameChecks = do



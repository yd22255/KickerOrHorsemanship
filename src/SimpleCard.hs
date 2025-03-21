{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedRecordDot    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
module SimpleCard where

import Apecs
import Data.List
import Control.Monad.IO.Class
import Control.Monad
import Apecs.Core

-- import SimpleCard (initWorld)

--i have tried to make coloured mana work and it did not go well :')

--TODO: 
--remove gatherpossibletriggers and just manual do it at each instance, see playLand
 -- options for testing : unit tests-> just do your tests via manual function inputs in main
 --                                 -> Probably needs some way of recording values/inputs during the process, debug trace?
 -- using quickcheck? tough. again, hard to tell what has happened. More common for property-based testing, describes a universal property of a program you can test across inputs
  -- hard to tell what a property is 
  -- probably unit testing, design a suite of sampled interactions
  -- test.hs, each function is its own set of unit tests

--FIGURE OUT WHERE AND WHEN TO RESOLVE THE STACK!!!
--CHECK WHY WE'RE FAILING ON SETUPGAME

-- FUTURE WORK what should i do when certain 'the game works a different way' effects are in play, e.g. Rest In Peace
 -- current plan is to use a flag of some kind. alternative is to check each time on sendcardstogy but that seems somewhat inefficient

--'what code did i have to add to adapt a new thing'
--easy extension to match mtg's ever-expanding subset of cards can be a metric of success
--possible future work in the evaluation -> also whether it's a fair test of ecs, evaluate the model -- what it should have/should not have


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
        onActivate :: [(Cost, Maybe TargetType, Action)],
        onEvent :: [(Trigger, Maybe TargetType, Action)],
        onResolve :: [Action],
        possibleTargets :: Maybe TargetType,
        attributes :: [Attribute],
        currentZone :: Zone
    } deriving (Show,Eq)

appendcard :: Card->[Card]->[Card]
appendcard thiscard cards = cards ++ [thiscard]


data Zone = Hand | Battlefield | Graveyard | Library | Exile | Stack | None deriving (Show,Eq)

data Action = GoToBattlefield| CounterTarget | DestroyTarget| AddMana Int| DrawCards Int | DoNothing | Then Action Action deriving (Show,Eq)

data Colour = White | Blue | Black | Red | Green | Colourless | And Colour Colour deriving (Show,Eq)

data Cost = Tap deriving (Show,Eq)

data ManaCost = ManaColour Colour Int | Generic Int | Or ManaCost ManaCost | Plus ManaCost ManaCost deriving (Show,Eq)
--manapool can't have Or, maybe tuple?

data Trigger = ThisEnters [Attribute] | WhenCast [Attribute] | PermanentETB [Attribute] | SpellCast [Attribute] deriving (Show,Eq)
--trigger and associated attributes e.g. PermanentEnters as a 
--talk about trigger tradeoff vs having individual landfall, blueenters, whencast, etc -> infinitely expanding series of possible triggers

data TargetType = Spell | Permanent | TriggeredAbility | ActivatedAbility deriving (Show, Eq)

data Keyword = FirstStrike | DoubleStrike | Menace | Defender | Vigilance | Trample |
 Lifelink | Deathtouch | Haste | Flash | Flashback | Flying | Reach |
  Hexproof | Ward Int| Kicker| Indestructible deriving (Show, Eq)

data Attribute =
    BasePower Int
    | BaseToughness Int
    | Keywords [Keyword]
    | Tapped Bool
    | Colour Colour
    | CardType CardType
    | ManaCost Int
    deriving (Show,Eq)

data AbilityAttribute =
    Source Entity
    | SourceColour Colour
    | SourceCardType CardType
    deriving (Show,Eq)


basicLand :: Card
basicLand = Card {
    cardName = "BasicLand",
    cardType = Land,
    manaCost=0,
    onActivate = [(Tap, Nothing, AddMana 1)],
    onEvent = [],
    onResolve = [],
    possibleTargets = Nothing,
    attributes = [Tapped False],
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
    attributes = [BasePower 1, BaseToughness 1, Tapped False],
    currentZone = None

}

solRing :: Card
solRing = Card {
    cardName = "Sol Ring",
    cardType = Artifact,
    manaCost=1,
    onActivate = [(Tap, Nothing, AddMana 2)],
    onEvent = [],
    onResolve = [GoToBattlefield],
    possibleTargets = Nothing,
    attributes = [Tapped False],
    currentZone = None

}

lotusCobra :: Card
lotusCobra = Card {
    cardName = "Lotus Cobra",
    cardType = Creature,
    manaCost=2,
    onActivate = [],
    onEvent = [(PermanentETB [CardType Land],Nothing,AddMana 1)],
    onResolve = [GoToBattlefield],
    possibleTargets = Nothing,
    attributes = [Tapped False, BasePower 2, BaseToughness 1],
    currentZone = None

}

newtype CurrentMana = CurrentMana Int deriving (Show)
instance Component CurrentMana where type Storage CurrentMana = Unique CurrentMana

newtype LandsPlayed = LandsPlayed Int deriving (Show)
instance Component LandsPlayed where type Storage LandsPlayed = Unique LandsPlayed
newtype LandsPerTurn = LandsPerTurn Int deriving (Show)
instance Component LandsPerTurn where type Storage LandsPerTurn = Unique LandsPerTurn

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

newtype ABasePower = ABasePower Int deriving Show
instance Component ABasePower where type Storage ABasePower = Map ABasePower
newtype ABaseToughness = ABaseToughness Int deriving Show
instance Component ABaseToughness where type Storage ABaseToughness = Map ABaseToughness
newtype AColour = AColour Colour deriving Show
instance Component AColour where type Storage AColour = Map AColour
newtype IsTapped = IsTapped Bool deriving Show
instance Component IsTapped where type Storage IsTapped = Map IsTapped
newtype SomeKeywords = SomeKeywords [Keyword] deriving Show
instance Component SomeKeywords where type Storage SomeKeywords = Map SomeKeywords


newtype AnOnActivate = AnOnActivate [(Cost,Maybe TargetType, Action)]
instance Component AnOnActivate where type Storage AnOnActivate = Map AnOnActivate
--activated ability

newtype AnOnEvent = AnOnEvent [(Trigger, Maybe TargetType, Action)]
instance Component AnOnEvent where type Storage AnOnEvent = Map AnOnEvent
--triggered ability

newtype AnOnResolve = AnOnResolve [Action]
instance Component AnOnResolve where type Storage AnOnResolve = Map AnOnResolve
--effect of card/ability

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

    ''IsCard, ''ACardName, ''ACardType, ''AManaCost, ''AnOnActivate, ''AnOnEvent, ''AnAttributes, ''AZone, ''AnOnResolve,
 ''TheHand, ''TheGraveyard, ''TheBattlefield, ''TheStack, ''TheLibrary, ''TheExile, ''CardsinZone, ''CurrentTargets, ''CurrentMana,
    ''EntitiesinZone, ''AbilityAttributes, ''AbilityEffect, ''AbilityCost, ''IsAbility, ''ABasePower,''ABaseToughness,''AColour,
    ''SomeKeywords,''IsTapped, ''LandsPlayed, ''LandsPerTurn
    ]

type System' a = System World a
main :: IO ()
main = do
    w <- initWorld
    runWith w setupGame


setupGame :: System'()
setupGame = do
    createZones
    createGameState
    -- putCardInPlace basicLand None Hand 
    -- putCardInPlace solRing None Hand
    --putCardInPlace divination None Library
    -- putCardInPlace basicGuy None Hand
    --putCardInPlace counterspell None Library
    populateLibrary [basicLand, basicGuy, basicLand]
    drawcards 2
    --WHY ARE YOU FAILING :(
    --check this and why it's failing
    --first thing is just print cardsinhand and go from there?
    abilityorcast

-- gameplayLoop :: System'()
-- gameplayLoop = do


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

effectsonstack :: System' [(Entity,[Action])]
effectsonstack = do
    theents<-collect $ \ (TheStack, EntitiesinZone entities) -> Just (entities)
    forM (concat theents) $ \(e) -> do
        AnOnResolve resolveaction<- getComponent e
        return (e, resolveaction)

createGameState :: System'()
createGameState = do
    newEntity_
        (CurrentMana 1,
        LandsPlayed 0, LandsPerTurn 1)

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
--ONLY TO BE USED ON EMPTY LIBRARY

playnewcard :: Card-> System' Entity
playnewcard card= do
    ent<-newEntity
        (IsCard,
        ACardName card.cardName,
        ACardType card.cardType,
        AManaCost card.manaCost,
        (AnOnActivate card.onActivate,
        AnOnEvent card.onEvent,
        AnOnResolve card.onResolve),
        AnAttributes card.attributes,
        CurrentTargets [],
        AZone Battlefield
        )
    forM_ card.attributes $ \(attribute) ->
                    attributetocomponent ent attribute
    return (ent)


castnewcard :: Card->[Entity]-> System' Entity
castnewcard card targets= do
    ent<-newEntity
        (IsCard,
        ACardName card.cardName,
        ACardType card.cardType,
        AManaCost card.manaCost,
        (AnOnActivate card.onActivate,
        AnOnEvent card.onEvent,
        AnOnResolve card.onResolve),
        --(AnAttributes card.attributes),
        CurrentTargets targets,
        AZone Stack
        )
    forM_ card.attributes $ \(attribute) ->
                    attributetocomponent ent attribute
    return (ent)



attributetocomponent :: Entity->Attribute->System'()
attributetocomponent ent attribute=
    case attribute of
        BasePower x -> set ent (ABasePower x)
        BaseToughness x -> set ent (ABaseToughness x)
        Colour x -> set ent (AColour x)
        Tapped x -> set ent (IsTapped x)
        Keywords [keywords]->set ent (SomeKeywords [keywords])
        _ -> return ()

putabilityonstack :: Action->[Entity]->System' Entity
putabilityonstack resolve targets = do --do i want the resolution effect in here?
    --AnAttributes atts<- getComponent entity
    newEntity
        (IsAbility,
        AnOnResolve [resolve],
        CurrentTargets targets,
        AZone Stack
        --AnAttributes atts
        -- relevant attributes of the entity causing the trigger? i think this needs to happen at some point maybe
        )

checktrigger :: Trigger ->System'()
checktrigger trigger = do
    cmapM_ $ \(AZone thezone, AnOnEvent triggeractions) -> do
        case thezone of
            Battlefield -> do
                let targetactions = [(atargettype,action) | (newtrigger,atargettype, action) <- triggeractions,
                                        compareTriggers newtrigger trigger]
                forM_ targetactions $ \(targets, action) ->
                    case targets of
                        Nothing -> do
                            newability<-putabilityonstack action []
                            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (newability : stackcards)
                        Just thistarget -> do
                            effecttarget<-choosetarget (thistarget)
                            newability<-putabilityonstack action [effecttarget]
                            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (newability : stackcards)
            _ -> return ()

compareTriggers :: Trigger -> Trigger -> Bool
compareTriggers (PermanentETB attrs) (PermanentETB attrs') =
    all (\attr -> attr `elem` attrs') attrs
compareTriggers (SpellCast attrs) (SpellCast attrs') =
    all (\attr -> attr `elem` attrs') attrs
compareTriggers (_)(_) =
    False

checkforTriggers :: [Trigger]->System'()
checkforTriggers []=return ()
checkforTriggers (trigger:triggers)=do
    checktrigger trigger
    checkforTriggers triggers


playLand :: Card->System'()
playLand land = do
    thisland<-playnewcard land
    cmap $ \ (LandsPlayed landsplayed) -> LandsPlayed (landsplayed+1)
    AnAttributes atts <- getComponent thisland
    let triggerlist = [PermanentETB atts]
    checkforTriggers (triggerlist)


assembleTriggerlist::([Trigger],Trigger)->[Trigger]
assembleTriggerlist (triggerlist,trigger) = triggerlist ++ [trigger]

-- gatherpossibletriggers::(Entity,String)->System'[Trigger]
-- gatherpossibletriggers (ent,"Cast")= do
--     AnAttributes atts <- getComponent ent
--     let triggerlist = [SpellCast atts]
--     return triggerlist
-- gatherpossibletriggers (ent,"ETB")= do
--     AnAttributes atts<- getComponent ent
--     --add more attributes of the cast spell here?
--     let triggerlist = [PermanentETB atts]
--     return triggerlist
-- gatherpossibletriggers (_,_) = return []
--     --my default thinking here is just to throw a hundred if statements at a card, but that doesnt seem very Haskell
--     --ifs also dont exactly work as i want them to here hmm
-- --so instead of gatherpossibletriggers, 


castCard :: Card->System' ()
castCard card = do
    case card.possibleTargets of
        Nothing -> do
            entity<-castnewcard card []
            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (entity : stackcards)
            cmap $ \ (TheHand, CardsinZone handcards)->CardsinZone (delete card handcards)
            case card.onEvent of
                []-> do return ()
                [(WhenCast atts,Just thetargettype, action)] -> do
                    --parseaction (action, entity)
                    triggertarget<-choosetarget (thetargettype)
                    castentity<-putabilityonstack action [triggertarget]
                    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (castentity : stackcards)
                    --tension here, as i've realised this needs to create the new ability entity but that doesn't fit with the options here
                    --has been solved for the minute with newEntity_ as opposed to newEntity but this could be an issue in the future?
                _->return ()
            AnAttributes atts <- getComponent entity
            let triggerlist = [SpellCast atts]
            checkforTriggers (triggerlist)

    --cast a card without targets and then resolve its WhenCast trigger

--i need to figure out how to trigger other cast effects here in a succinct way, because there's a LOT of possible cast trigger configurations


        Just targetType -> do
            target<-choosetarget targetType
            entity<-castnewcard card [target]
            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (entity : stackcards)
            case card.onEvent of
                []-> do return ()
                [(WhenCast atts,Just thetargettype, action)] -> do
                    triggertarget<-choosetarget (thetargettype)
                    castentity<-putabilityonstack action [triggertarget]
                    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (castentity : stackcards)
                _->return ()
            AnAttributes atts <- getComponent entity
            let triggerlist = [SpellCast atts]
            checkforTriggers (triggerlist)
            --trigger other cast effects here
    --cast a card with targets and then resolve its WhenCast trigger

--i need to figure out how to trigger other cast effects here in a succinct way, because there's a LOT of possible cast trigger configurations


resolveStack :: System'()
resolveStack = do
    triggeractions<-effectsonstack
    resolvingOnResolves triggeractions

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
setmana x = cmap $ \(CurrentMana _) -> CurrentMana x


obtainInput :: Read a => String->SystemT World IO a --this might come back to bite me?
obtainInput prompt = do
    liftIO (putStrLn prompt)
    target<-liftIO (getLine)
    return (read target)

findtarget :: TargetType->System' [[Entity]]
findtarget Spell= collect $ \ (TheStack, EntitiesinZone entities) -> Just (entities)
findtarget Permanent = collect $ \ (TheBattlefield, EntitiesinZone entities) -> Just (entities)
findtarget _ = collect $ \ (TheStack) -> Just ([0])

choosetarget :: TargetType->System' Entity
choosetarget (targetType) = do
    targets<- (findtarget (targetType))
    getUserChoice (concat targets) (\target -> do
            ACardName name <- getComponent target
            return name
        )

getUserChoice :: [a] -> (a -> System' String) -> System' a
getUserChoice [] _ = error "User choice empty"
getUserChoice options display = do
    forM_ (zip [1 ..] options) $ \(ix, option) -> do
        optionText <- display option
        liftIO (print (show ix ++ ". " ++ optionText))
    target <- obtainInput ("pick one")
    let option = options !! (target-1)
    if target > length options
        then do
            liftIO (putStrLn "invalid option!")
            getUserChoice options display
        else do
            return option

getTargets :: Entity->System'[Entity]
getTargets card = do
    CurrentTargets targets <- getComponent card
    return targets

getName :: Entity->System'(String)
getName card = do
    ACardName targets <- getComponent card
    return targets

deletethese :: [Entity]->[Entity]->[Entity]
deletethese targets list = foldr delete targets list

sendcardstogy :: [Entity]->System'()
sendcardstogy cards = do
    --collect all entities on the battlefield and check them for rest in peace/like
    cmap $ \ (TheGraveyard, EntitiesinZone gycards) -> EntitiesinZone (cards ++ gycards)
--this only exists because of rest in peace

counterTargets :: Entity->System'()
counterTargets card = do
    cardtargets<-getTargets card
    cmapM $ \(TheStack, EntitiesinZone cards) ->
        case cards of
            [] -> return ()
            (thiscard : cards') -> do
                sendcardstogy cardtargets
                cmap $ \ (TheStack, EntitiesinZone stackcards) ->
                    EntitiesinZone (deletethese cardtargets stackcards)

destroyTargets :: Entity->System'()
destroyTargets card = do
    cardtargets<-getTargets card
    cmapM $ \(TheBattlefield, EntitiesinZone cards) ->
        case cards of
            [] -> return ()
            (thiscard : cards') -> do
                sendcardstogy cardtargets
                cmap $ \ (TheBattlefield, EntitiesinZone bfcards) -> EntitiesinZone (deletethese cardtargets bfcards)
--currently only works on one target, ig just do this multiple times for each target?

resolvePermanent :: Entity->System'()
resolvePermanent permanent = do
    set permanent (AZone Battlefield)
    cmap $ \ (TheBattlefield, EntitiesinZone bfcards) -> EntitiesinZone (permanent : bfcards)
    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (delete permanent stackcards)
    AnOnEvent events<- getComponent permanent
    forM_ events $ \(trigger,targets, action) ->
        case trigger of
            ThisEnters _ -> do
                case targets of
                    Nothing -> do
                        newability<-putabilityonstack action []
                        cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (newability : stackcards)
                    Just thistarget -> do
                        effecttarget<-choosetarget (thistarget)
                        newability<-putabilityonstack action [effecttarget]
                        cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (newability : stackcards)
            _ -> return ()
    AnAttributes atts <- getComponent permanent
    let triggerlist = [PermanentETB atts]
    checkforTriggers (triggerlist)




parseaction :: (Action,Entity)->System'()
parseaction (DoNothing, _) = return ()
parseaction (DrawCards x, _) = drawcards x
parseaction (DestroyTarget, entity)= destroyTargets entity
parseaction (CounterTarget, entity)=counterTargets entity
parseaction (GoToBattlefield, entity)=resolvePermanent entity
parseaction (AddMana x, _) = addmana x
parseaction (Then x y, z) = do
    parseaction (x,z)
    parseaction (y,z)

-- [(Entity,[(Trigger,Action)])]
resolvingOnResolves :: [(Entity,[Action])] -> System'()
resolvingOnResolves [] = parseaction (DoNothing,0)
resolvingOnResolves ((entity,[action]):tas) = do
    parseaction (action,entity)
    resolvingOnResolves tas
    --this presumes the trigger is met as the effect is already on the stack
resolvingOnResolves _ = parseaction (DoNothing,0)



doCost :: Cost->Entity->System'()
doCost Tap card = do
    set card (IsTapped True)


activateAbility :: Entity->System'()
activateAbility card= do
    AnOnActivate abilities<-getComponent card
    whichability<-getUserChoice (abilities) (\(cost,targettype, action) -> do
            return (show action)
        )
    --how do i represent the different abilities in a print? give them a name or something idk? do i seriously need to add a description to each ability kill me
    --alternatively i just go by numbers, straight through obtainInput like in the menu system
    let (cost,targets,action) = whichability
    case targets of
        Nothing-> do
            doCost cost card
            newability<-putabilityonstack action []
            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (newability : stackcards)
        Just (thistarget)-> do
            doCost cost card
            effecttarget<-choosetarget (thistarget)
            newability<-putabilityonstack action [effecttarget]
            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (newability : stackcards)

    --surely it's easier to do the effect choice inside this function lol

    --currently bottlenecking myself on how to represent the abilities in the choice section, as they're not entities until they go on the stack
    --going to assume we're just working with one ability which has already been chosen right now and work out
doesitActivate :: Entity->System' Bool
doesitActivate ent = do
    AnOnResolve onres<-getComponent ent
    if null onres
        then do return False
        else do return True

activatableAbilities :: System'()
activatableAbilities= do
    entswithabs<-collect $ \ (TheBattlefield, EntitiesinZone entities) -> Just (entities)
    newents<-filterM doesitActivate (concat entswithabs)
    chosenent<-getUserChoice (newents) (\target -> do
            ACardName name <- getComponent target
            return name
        )
    activateAbility chosenent
    --how to list the abilities?

isitCastable :: Int -> (Card -> Bool)
isitCastable currentMana card =
    if card.cardType == Land
        then False
        else card.manaCost < currentMana

-- >>> :t filter
-- filter :: (a -> Bool) -> [a] -> [a]

-- >>> :t isitCastable
-- isitCastable :: Int -> Card -> Bool

-- >>> :t isitCastable 5
-- isitCastable 5 :: Card -> Bool


castableCards :: System'()
castableCards = cmapM_ $ \(CurrentMana currentMana) -> do
    cih<-cardsinhand
    let newcih = filter (\card -> isitCastable currentMana card) (concat cih)
    chosencard<-getUserChoice (newcih) (\thiscard -> do
            return thiscard.cardName
        )
    castCard chosencard
    --displayGameState (which will then return us to abilityorcast)

--somehow gather Cards in the hand which have a manaCost that can be cast given the current manaPool and are not lands

isitThisType :: CardType->Card->Bool
isitThisType atype card =
    card.cardType == atype

playableLands :: System'()
playableLands=cmapM_ $ \(LandsPlayed landsplayed, LandsPerTurn landsperturn) -> do
    if landsplayed<landsperturn
        then do 
            cih<-cardsinhand
            let landscih=filter (\card -> isitThisType Land card) (concat cih)
            chosenland<-getUserChoice (landscih) (\thisland -> do
                    return thisland.cardName
                )
            playLand chosenland
        else do 
            liftIO(print("You have played your lands for the turn. Please do something else."))
            --displayGameState (which will then return us to abilityorcast)

abilityorcast :: System'()
abilityorcast = do
    let acceptables = [(1 :: Integer),2,3]
    aborc<-obtainInput ("1. Activate Ability  2. Cast Card(s) 3. Play Land")
    if aborc `elem` acceptables
        then do
            case aborc of
                1 -> activatableAbilities
                2-> castableCards
                3-> playableLands
                _-> do
                    liftIO (putStrLn "invalid option!")
                    abilityorcast
        else do
            liftIO (putStrLn "invalid option!")
            abilityorcast


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
--i THINK i need to separate WhenCast and onresolve again, because they're triggered in entirely different circumstances, and it makes a generic parsing function really rough
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



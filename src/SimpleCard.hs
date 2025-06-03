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
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Redundant if" #-}

module SimpleCard where

import Apecs
import Data.List
import Control.Monad.IO.Class
import Control.Monad
import Apecs.Core

import Text.Read (readMaybe)

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
        onActivate :: [(Cost, Maybe TargetType, Action,IsManaAbility)],
        onEvent :: [(Trigger, Maybe TargetType, Action)],
        additcost :: Maybe Cost,
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

data Cost = Tap | Free | PayLife Int | Discard Int deriving (Show,Eq)

data ManaCost = ManaColour Colour Int | Generic Int | Or ManaCost ManaCost | Plus ManaCost ManaCost deriving (Show,Eq)
--manapool can't have Or, maybe tuple?

data LandType = Plains| Island| Swamp| Mountain | Forest| Wastes

type IsManaAbility = Bool

data Trigger = ThisEnters [Attribute] | WhenCast [Attribute] | PermanentETB [Attribute] | SpellCast [Attribute] | ThisDies [Attribute] | PermanentLeaves [Attribute] | PermanentDies [ Attribute] | PermanentExiled [Attribute] deriving (Show,Eq)
--trigger and associated attributes e.g. PermanentEnters as a 

data TargetType = Spell | Permanent | CreaturePermanent | CreatureSpell | TriggeredAbility | ActivatedAbility deriving (Show, Eq)

data Keyword = FirstStrike | DoubleStrike | Menace | Defender | Vigilance | Trample |
 Lifelink | Deathtouch | Haste | Flash  | Flying | Reach |
  Hexproof | Ward Int| Indestructible deriving (Show, Eq)
  --created before combat was abstracted away, vast majority of these can't be implemented under the current framework.

data StateofGame = Open | Closed
data StackResolution = One | All

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
    onActivate = [(Tap, Nothing, AddMana 1,True)], --TODO: technically this isn't how mana abilities work, i dont think its that hard of a change but it's late rn
    onEvent = [],
    additcost = Nothing,
    onResolve = [],
    possibleTargets = Nothing,
    attributes = [CardType Land, Tapped False],
    currentZone = None
}

counterspell :: Card
counterspell = Card {
    cardName = "Counterspell",
    cardType = Instant,
    manaCost=2,
    onActivate = [],
    onEvent = [],
    additcost = Nothing,
    onResolve = [CounterTarget],
    possibleTargets = Just Spell,
    attributes = [CardType Instant, Colour Blue],
    currentZone = None
}

divination :: Card
divination = Card {
    cardName = "Divination",
    cardType = Sorcery,
    manaCost=3,
    onActivate = [],
    onEvent = [],
    additcost = Nothing,
    onResolve = [(DrawCards 2)],
    possibleTargets = Nothing,
    attributes = [CardType Sorcery],
    currentZone = None
}

vindicate :: Card
vindicate = Card {
    cardName = "Vindicate",
    cardType = Instant,
    manaCost=3,
    onActivate = [],
    onEvent = [],
    additcost = Nothing,
    onResolve = [(DestroyTarget)],
    possibleTargets = Just Permanent,
    attributes = [CardType Instant],
    currentZone = None
}

basicGuy :: Card
basicGuy = Card {
    cardName = "Memnite",
    cardType = Creature,
    manaCost=0,
    onActivate = [],
    onEvent = [],
    additcost = Nothing,
    onResolve = [GoToBattlefield],
    possibleTargets = Nothing,
    attributes = [CardType Artifact,CardType Creature, BasePower 1, BaseToughness 1, Tapped False],
    currentZone = None
}

solRing :: Card
solRing = Card {
    cardName = "Sol Ring",
    cardType = Artifact,
    manaCost=1,
    onActivate = [(Tap, Nothing, AddMana 2,True)],
    onEvent = [],
    additcost = Nothing,
    onResolve = [GoToBattlefield],
    possibleTargets = Nothing,
    attributes = [CardType Artifact, Tapped False],
    currentZone = None

}

lotusCobra :: Card
lotusCobra = Card {
    cardName = "Lotus Cobra",
    cardType = Creature,
    manaCost=2,
    onActivate = [],
    onEvent = [(PermanentETB [CardType Land],Nothing,AddMana 1)],
    additcost = Nothing,
    onResolve = [GoToBattlefield],
    possibleTargets = Nothing,
    attributes = [CardType Creature, Tapped False, BasePower 2, BaseToughness 1],
    currentZone = None

}

newtype CurrentMana = CurrentMana Int deriving (Show)
instance Component CurrentMana where type Storage CurrentMana = Unique CurrentMana

newtype PlayerLP = PlayerLP Int deriving (Show)
instance Component PlayerLP where type Storage PlayerLP = Unique PlayerLP

newtype LandsPlayed = LandsPlayed Int deriving (Show)
instance Component LandsPlayed where type Storage LandsPlayed = Unique LandsPlayed
newtype LandsPerTurn = LandsPerTurn Int deriving (Show)
instance Component LandsPerTurn where type Storage LandsPerTurn = Unique LandsPerTurn

data IsCard = IsCard deriving Show
instance Component IsCard where type Storage IsCard = Map IsCard

data IsAbility = IsAbility deriving Show
instance Component IsAbility where type Storage IsAbility = Map IsAbility

data IsToken = IsToken deriving Show
instance Component IsToken where type Storage IsToken = Map IsToken

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

newtype APower = APower Int deriving Show
instance Component APower where type Storage APower = Map APower
newtype AToughness = AToughness Int deriving Show
instance Component AToughness where type Storage AToughness = Map AToughness
newtype AColour = AColour Colour deriving Show
instance Component AColour where type Storage AColour = Map AColour
newtype IsTapped = IsTapped Bool deriving Show
instance Component IsTapped where type Storage IsTapped = Map IsTapped
newtype SomeKeywords = SomeKeywords [Keyword] deriving Show
instance Component SomeKeywords where type Storage SomeKeywords = Map SomeKeywords
newtype AttCardType = AttCardType CardType deriving Show
instance Component AttCardType where type Storage AttCardType = Map AttCardType

data HasFlash = HasFlash deriving Show
instance Component HasFlash where type Storage HasFlash = Map HasFlash
data HasFirstStrike = HasFirstStrike deriving Show
instance Component HasFirstStrike where type Storage HasFirstStrike = Map HasFirstStrike
data HasHexproof = HasHexproof deriving Show
instance Component HasHexproof where type Storage HasHexproof = Map HasHexproof
data HasIndestructible = HasIndestructible deriving Show
instance Component HasIndestructible where type Storage HasIndestructible = Map HasIndestructible
newtype HasWard = HasWard Int deriving Show
instance Component HasWard where type Storage HasWard = Map HasWard

newtype AnOnActivate = AnOnActivate [(Cost,Maybe TargetType, Action,IsManaAbility)]
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

type CardComps = ((IsCard,IsToken),(ACardName,ACardType,AManaCost,AnOnActivate,AnOnEvent,AnAttributes,
    AZone,AnOnResolve),CurrentTargets,(APower,AToughness,AColour,SomeKeywords, IsTapped, AttCardType))
type ZoneComps = ((TheHand,TheBattlefield,TheLibrary,TheGraveyard,TheStack,TheExile),
    (CardsinZone,EntitiesinZone))
type GameStateComps = (CurrentMana,LandsPlayed,LandsPerTurn,PlayerLP)

makeWorld "World" [

    ''IsCard, ''ACardName, ''ACardType, ''AManaCost, ''AnOnActivate, ''AnOnEvent, ''AnAttributes, ''AZone, ''AnOnResolve,
 ''TheHand, ''TheGraveyard, ''TheBattlefield, ''TheStack, ''TheLibrary, ''TheExile, ''CardsinZone, ''CurrentTargets, ''CurrentMana,
    ''EntitiesinZone, ''AbilityAttributes, ''AbilityEffect, ''AbilityCost, ''IsAbility, ''APower,''AToughness,''AColour, ''AttCardType,
    ''SomeKeywords,''IsTapped, ''LandsPlayed, ''LandsPerTurn, ''IsToken, ''PlayerLP,
    ''HasFlash,''HasHexproof, ''HasIndestructible, ''HasWard
    ]

type System' a = System World a

testSetup :: System'()
testSetup = do
    createZones
    createGameState
    populateLibrary [basicLand,basicGuy,divination]


main :: IO ()
main = do
    w <- initWorld
    runWith w setupGame
--world creator, running the setup function inside a new world.

castcardx :: Int->System'()
castcardx 0 = return ()
castcardx x = do
    lc<-playnewcard lotusCobra
    castcardx (x-1)

setupGame :: System'()
setupGame = do
    createZones
    createGameState
    castcardx 300
    populateLibrary [basicLand, basicLand, basicLand, divination, basicGuy, counterspell, vindicate ]
    drawcards 4
    showGS
    abilityorcast Open
--the baseline setup function to create a standard, interactive game. 


debug :: Show a => a -> System' ()
debug x = liftIO (print x)
--print inside the System for debugging purposes.

cdebug :: (Show a, Get w m c, Members w m c, MonadIO m)
       => (c -> a) -> SystemT w m ()
cdebug f = cmapM_ (\x -> liftIO (print (f x)))
--print with a Monad

getComponent :: forall c. (Has World IO c, ExplGet IO (Storage c)) => Entity -> System' c
getComponent entity = get entity

cardsonbattlefield :: System' [Entity]
cardsonbattlefield = do
    cob<-collect $ \ (TheBattlefield, EntitiesinZone entities) -> Just entities
    return (concat cob)

cardsinhand :: System' [Card]
cardsinhand = do
    cih<-collect $ \ (TheHand, CardsinZone cards) -> Just cards
    return (concat cih)

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
        (CurrentMana 0,
        LandsPlayed 0, LandsPerTurn 1,
        PlayerLP 20)

createZones :: System'()
createZones= do
    newEntity_
        (TheHand,
        CardsinZone []
        )
    newEntity_
        (
        TheGraveyard,
        EntitiesinZone []
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

deleteEntity :: Entity->System'()
deleteEntity ent = do
    destroy ent (Proxy @CardComps)

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
    cmap $ \ (TheBattlefield, EntitiesinZone ents)->EntitiesinZone (ent : ents)
    forM_ card.attributes $ \(attribute) ->
        case attribute of
            Keywords keywordlist->
                forM_ keywordlist $ \(keyword) ->
                    keywordtocomponent ent keyword
            _->
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
        (AnAttributes card.attributes),
        CurrentTargets targets,
        AZone Stack
        )
    forM_ card.attributes $ \(attribute) ->
        case attribute of
            Keywords keywordlist->
                forM_ keywordlist $ \(keyword) ->
                    keywordtocomponent ent keyword
            _->
                    attributetocomponent ent attribute
    return (ent)

keywordtocomponent :: Entity->Keyword->System'()
keywordtocomponent ent keyword =
    case keyword of
        Flash -> set ent (HasFlash)
        Hexproof -> set ent (HasHexproof)
        Indestructible -> set ent (HasIndestructible)
        Ward x -> set ent (HasWard x)
        _->return ()
        -- Flying -> set ent (HasFlying)
        -- FirstStrike -> set ent (HasFirstStrike)
        -- DoubleStrike -> set ent (HasDoubleStrike)
        -- Menace -> set ent (HasMenace)
        -- Vigilance -> set ent (HasVigilance)
        -- Defender -> set ent (HasDefender)
        -- Trample -> set ent (HasTrample)
        -- Haste -> set ent (HasHaste)
        -- Deathtouch -> set ent (HasDeathtouch)
        -- Reach -> set ent (HasReach)
        -- Lifelink -> set ent (HasLifelink)
        --removed as combat abstracted
--the beginnings of an overhaul to the keywords system that was not completed in time for full applicability.
--Flash, Hexproof and Indestructible were implemented in this way, however. 


attributetocomponent :: Entity->Attribute->System'()
attributetocomponent ent attribute=
    case attribute of
        BasePower x -> set ent (APower x)
        BaseToughness x -> set ent (AToughness x)
        Colour x -> set ent (AColour x)
        Tapped x -> set ent (IsTapped x)
        Keywords [keywords]->set ent (SomeKeywords [keywords])
        CardType ct -> set ent (AttCardType ct)
        _ -> return ()

putabilityonstack :: Action->[Entity]->System' Entity
putabilityonstack resolve targets = do 
    newEntity
        (IsAbility,
        AnOnResolve [resolve],
        CurrentTargets targets,
        AZone Stack
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
compareTriggers (PermanentDies attrs) (PermanentLeaves attrs') =
    all (\attr -> attr `elem` attrs') attrs
compareTriggers (PermanentExiled attrs) (PermanentLeaves attrs') =
    all (\attr -> attr `elem` attrs') attrs
compareTriggers (PermanentDies attrs) (PermanentDies attrs') =
    all (\attr -> attr `elem` attrs') attrs
compareTriggers (PermanentExiled attrs) (PermanentExiled attrs') =
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
    cmap $ \ (TheHand, CardsinZone handcards)->CardsinZone (delete land handcards)
    cmap $ \ (LandsPlayed landsplayed) -> LandsPlayed (landsplayed+1)
    AnAttributes atts <- getComponent thisland
    let triggerlist = [PermanentETB atts]
    checkforTriggers (triggerlist)
    cmapM $ \ (TheStack,EntitiesinZone ents)->
        case ents of
            []->do

                return ()
            _->displayGameState Closed
    --if there's a trigger, go to closed gamestate, otherwise play a land


assembleTriggerlist::([Trigger],Trigger)->[Trigger]
assembleTriggerlist (triggerlist,trigger) = triggerlist ++ [trigger]

castCard :: Card->System' ()
castCard card = do
    case card.possibleTargets of
        Nothing -> do
            entity<-castnewcard card []
            AManaCost mc<-getComponent entity
            paymana mc
            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (entity : stackcards)
            cmap $ \ (TheHand, CardsinZone handcards)-> CardsinZone (delete card handcards)
            case card.onEvent of
                []-> do return ()
                [(WhenCast atts,Just thetargettype, action)] -> do
                    --parseaction (action, entity)
                    triggertarget<-choosetarget (thetargettype)
                    castentity<-putabilityonstack action [triggertarget]
                    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (castentity : stackcards)
                _->return ()
            AnAttributes atts <- getComponent entity
            let triggerlist = [SpellCast atts]
            checkforTriggers (triggerlist)
            cmapM $ \ (TheStack, EntitiesinZone stackcards)->
                if null stackcards
                    then do displayGameState Open
                    else do displayGameState Closed
    --cast a card without targets and then resolve its WhenCast trigger

        Just targetType -> do
            target<-choosetarget targetType
            entity<-castnewcard card [target]
            AManaCost mc<-getComponent entity
            paymana mc
            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (entity : stackcards)
            cmap $ \ (TheHand, CardsinZone handcards)-> CardsinZone (delete card handcards)
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
            cmapM $ \ (TheStack, EntitiesinZone stackcards)->
                if null stackcards
                    then do displayGameState Open
                    else do displayGameState Closed
    --cast a card with targets and then resolve its WhenCast trigger


resolveEffect :: StackResolution->System'()
resolveEffect One = do
    triggeractions<-effectsonstack
    resolvingOnResolves triggeractions
resolveEffect All = do
    triggeractions<-effectsonstack
    resolveFullStack triggeractions
--reworked resolveStack to better mimic magic's Stack Resolution, additionally rE All helps with tests

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
    cmapM $ \ (TheLibrary, CardsinZone ents)-> do
        if (length ents)<x
            then endGame Deckout
            else do
                drawcard
                drawcards (x-1)

addmana :: Int->System'()
addmana x = cmap $ \(CurrentMana mananow) -> CurrentMana (mananow+x)

paymana :: Int->System'()
paymana x = cmap $ \(CurrentMana mananow) -> CurrentMana (mananow-x)

setmana :: Int->System'()
setmana x = cmap $ \(CurrentMana _) -> CurrentMana x


obtainInput :: Read a => String->SystemT World IO a
obtainInput prompt = do
    liftIO (putStrLn prompt)
    target<-liftIO (getLine)
    case (readMaybe target) of
        Nothing->do
            liftIO (print ("please dont input that, try again."))
            obtainInput prompt
        Just x ->return x

filterstackSpells :: Entity->System' Bool
filterstackSpells ent = do
    isabil<-exists ent (Proxy @IsAbility)
    return (not isabil)

filterstackAbilities :: Entity->System' Bool
filterstackAbilities ent = do
    isabil<-exists ent (Proxy @IsAbility)
    return (isabil)


findtarget :: TargetType->System'( [Entity])
findtarget Spell= do
    ents<-collect $ \ (TheStack, EntitiesinZone entities) -> Just (entities)
    spells<-filterM (\ent -> filterstackSpells ent) (concat ents)
    return (spells)
findtarget Permanent = do
    ents<-collect $ \ (TheBattlefield, EntitiesinZone entities) -> Just (entities)
    return (concat ents)
findtarget CreaturePermanent = do
    cib<-collect $ \ (TheBattlefield,EntitiesinZone entities)->Just (entities)
    creatures<-filterM (\card -> isentThisType Creature card) (concat cib)
    return (creatures)
findtarget _ = collect $ \ (TheStack) -> Just (0)

isitHexproof :: Entity->System' Bool
isitHexproof ent = do
    hpcheck<-exists ent (Proxy @HasHexproof)
    return (not hpcheck)

choosetarget :: TargetType->System' Entity
choosetarget (targetType) = do
    targets<- (findtarget (targetType))
    nothptargets<-filterM (\tgt -> isitHexproof tgt) targets
    getUserChoice (targets) (\target -> do
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
    cmap $ \ (TheGraveyard, EntitiesinZone gycards) -> EntitiesinZone (cards ++ gycards)
    forM_ cards $ \ (card) -> do
        AnOnEvent events<- getComponent card
        forM_ events $ \(trigger,targets, action) ->
            case trigger of
                ThisDies _ -> do
                    case targets of
                        Nothing -> do
                            newability<-putabilityonstack action []
                            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (newability : stackcards)
                        Just thistarget -> do
                            effecttarget<-choosetarget (thistarget)
                            newability<-putabilityonstack action [effecttarget]
                            cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (newability : stackcards)
                _->return ()
        AnAttributes atts <- getComponent card
        let triggerlist = [PermanentLeaves atts]
        checkforTriggers (triggerlist)
    --this only exists because of rest in peace

counterTargets :: Entity->System'()
counterTargets card = do
    cardtargets<-getTargets card
    cmapM $ \(TheStack, EntitiesinZone cards) ->
        case cards of
            [] -> return ()
            (thiscard : cards') -> do
                forM_ cardtargets $ \ (cardtarget)->do
                    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (delete cardtarget stackcards)
                    sendcardstogy [cardtarget]


destroyTargets :: Entity->System'()
destroyTargets card = do
    cardtargets<-getTargets card
    cmapM $ \(TheBattlefield, EntitiesinZone cards) ->
        case cards of
            [] -> return ()
            _ -> do
                isindes<-exists card (Proxy @HasIndestructible)
                unless isindes $ do
                        liftIO (print (cardtargets))
                        forM_ cardtargets $ \ (cardtarget)->do
                            cmap $ \ (TheBattlefield, EntitiesinZone bfcards) -> EntitiesinZone (delete cardtarget bfcards)
                            sendcardstogy [cardtarget]


resolvePermanent :: Entity->System'()
resolvePermanent permanent = do
    set permanent (AZone Battlefield)
    AZone az<-getComponent permanent
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
    cmapM $ \ (TheStack, EntitiesinZone stackcards)->
                if null stackcards
                    then do displayGameState Open
                    else do displayGameState Closed




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

resolveEnt :: Entity->CardType->System'()
resolveEnt ent Creature = do resolvePermanent ent
resolveEnt ent Artifact = do resolvePermanent ent
resolveEnt ent Enchantment = do resolvePermanent ent
resolveEnt ent Land = do resolvePermanent ent
resolveEnt ent Instant = do sendcardstogy [ent]
resolveEnt ent Sorcery = do sendcardstogy [ent]



resolvingOnResolves :: [(Entity,[Action])] -> System'()
resolvingOnResolves [] = parseaction (DoNothing,0)
resolvingOnResolves ((entity,actions):tas) = do
    forM_ actions $ \action ->
        parseaction (action,entity)


    isabil<-exists entity (Proxy @IsAbility)
    if isabil
        then do deleteEntity entity
        else do
            ACardType ct<-getComponent entity
            resolveEnt entity ct

    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (delete (head stackcards) stackcards)

    displayGameState Closed


resolveFullStack :: [(Entity,[Action])] -> System'()
resolveFullStack [] = return ()
resolveFullStack ((entity,actions):tas) = do
    cmap $ \ (TheStack, EntitiesinZone stackcards) -> EntitiesinZone (delete (head stackcards) stackcards)
    forM_ actions $ \action ->
        parseaction (action,entity)
    resolveEffect All  
    --pick up on new triggers being added in the process of resolution

loseLife :: Int ->System'()
loseLife x = do
    cmap $ \ (PlayerLP y)->PlayerLP (y-x)

gainLife :: Int ->System'()
gainLife x = do
    cmap $ \ (PlayerLP y)->PlayerLP (y+x)


tapEnt :: Entity->System'()
tapEnt card = do
    set card (IsTapped True)

chooseDiscard :: Int ->System'()
chooseDiscard 0 = return ()
chooseDiscard x = do
    cih<-cardsinhand
    chosencard<-getUserChoice (cih) (\thiscard -> do
            return thiscard.cardName
        )
    cmap $ \ (TheHand, CardsinZone handcards) -> CardsinZone (delete chosencard handcards)
    cmap $ \ (TheGraveyard, CardsinZone gycards) -> CardsinZone (chosencard : gycards)

doCost :: Cost->Entity->System'()
doCost Tap card = do
    tapEnt card
doCost (PayLife x) card = do
    loseLife x
doCost (Discard x) card = do
    chooseDiscard x
doCost Free _ = return ()

isitTapped :: Entity->System'(Bool)
isitTapped ent = do
    IsTapped it<-getComponent ent
    return (not it)

costExecutableability :: Cost->Entity->System' Bool
costExecutableability cost ent =
    case cost of
        Tap -> do
            x<-isitTapped ent
            return x
        PayLife x-> do
            lifetotal<-collect $ \ (PlayerLP life) -> Just (life)
            return (x<head lifetotal)
        Free->return True
        Discard x -> do
            cih<-cardsinhand
            return (x>=length cih)
        _->return False --(doesn't have a legal cost for an ability)

costExecutablespell :: Cost->System' Bool
costExecutablespell cost  =
    case cost of
        PayLife x-> do
            lifetotal<-collect $ \ (PlayerLP life) -> Just (life)
            return (x<head lifetotal)
        Discard x -> do
            cih<-cardsinhand
            return (x>=length cih)
        Free->return True
        _->return False --(doesn't have a legal cost for a spell)

legalTargets :: Maybe TargetType ->System' Bool
legalTargets Nothing = return True
legalTargets x =
    case x of
        Just y -> do
            newcards<-findtarget y
            if null newcards
                then do return False
                else do return True



filterabs :: (Cost, Maybe TargetType, Action, IsManaAbility)->(Entity->System' Bool)
filterabs (cost,tgt,act,isman) ent = do
    x<-costExecutableability cost ent
    if not x
        then do return False
        else do
            y<-legalTargets tgt
            if not y
                then do return False
                else do return True

activateAbility :: Entity->System'()
activateAbility card= do
    AnOnActivate abilities<-getComponent card
    newabs<-filterM (\abil -> filterabs abil card) abilities
    whichability<-getUserChoice (abilities) (\(cost,targettype, action,ismana) -> do
            return (show action)
        )
    let (cost,targets,action,ismana) = whichability
    if ismana
        then do
            doCost cost card
            parseaction (action,card)
        else do
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


doesitActivate :: Entity->System' Bool
doesitActivate ent = do
    AnOnActivate onact<-getComponent ent
    if null onact
        then do return False
        else do return True

canitActivate :: Entity -> System' Bool
canitActivate ent = do
    IsTapped it<-getComponent ent
    return (not it)




activatableAbilities :: StateofGame->System'()
activatableAbilities Open= do
    entswithabs<-collect $ \ (TheBattlefield, EntitiesinZone entities) -> Just (entities)
    newents<-filterM doesitActivate (concat entswithabs)
    availableents<-filterM canitActivate (newents)
    if null availableents
        then do
            liftIO (print ("Sorry, you don't have any abilities to activate right now"))
            displayGameState Open
        else do
            return ()
    chosenent<-getUserChoice (availableents) (\target -> do
            ACardName name <- getComponent target
            return name
        )
    activateAbility chosenent
    stackeffs<-effectsonstack
    if null stackeffs
        then do displayGameState Open
        else do displayGameState Closed


activatableAbilities Closed= do
    entswithabs<-collect $ \ (TheBattlefield, EntitiesinZone entities) -> Just (entities)
    newents<-filterM doesitActivate (concat entswithabs)
    availableents<-filterM canitActivate (newents)
    if null availableents
        then do
            liftIO (print ("Sorry, you don't have any abilities to activate right now"))
            displayGameState Closed
        else do
            return ()
    chosenent<-getUserChoice (availableents) (\target -> do
            ACardName name <- getComponent target
            return name
        )
    activateAbility chosenent
    displayGameState Closed



isitCastable :: Int -> (Card -> System' Bool)
isitCastable currentMana card =
    if card.cardType == Land
        then return False
        else
            case card.additcost of
                Nothing->
                    case card.possibleTargets of
                        Nothing->return (card.manaCost <= currentMana)
                        Just x->do
                            stuff <- findtarget x
                            if null stuff
                                then do return False
                                else do return (card.manaCost <= currentMana)
                Just cost -> do
                    truth <- costExecutablespell cost
                    if truth
                        then case card.possibleTargets of
                            Nothing->return (card.manaCost <= currentMana)
                            Just x->do
                                stuff <- findtarget x
                                if null stuff
                                    then do return False
                                    else do return (card.manaCost <= currentMana)
                        else return False





isitthisAtt :: Attribute->Attribute ->Bool
isitthisAtt att att2=
    if att==att2 then True else False

isitInstant :: Card -> Bool
isitInstant card =
    case card.cardType of
        Instant -> True
        _->do
            let atts = card.attributes
            let keywords = filter (\att -> isitthisAtt (Keywords [Flash]) att) (atts)
            if null keywords
                then False
                else True


castableCards :: StateofGame->System'()
castableCards Open = cmapM_ $ \(CurrentMana currentMana) -> do
    cih<-cardsinhand
    newcih <- filterM (\card -> isitCastable currentMana card) (cih)
    if null newcih
        then do
            liftIO (print ("Sorry, you don't have the mana to cast any cards right now"))
            displayGameState Open
        else do
            return ()
    chosencard<-getUserChoice (newcih) (\thiscard -> do
            return thiscard.cardName
        )
    castCard chosencard
    displayGameState Open
castableCards Closed = cmapM_ $ \(CurrentMana currentMana) -> do
    cih<-cardsinhand
    newcih <- filterM (\card -> isitCastable currentMana card) (cih)
    let instantcih = filter (\card->isitThisType Instant card) (newcih)
    if null instantcih
        then do
            liftIO (print ("Sorry, you don't have the mana to cast any cards right now"))
            displayGameState Closed
        else do
            return ()

    chosencard<-getUserChoice (instantcih) (\thiscard -> do
            return thiscard.cardName
        )
    castCard chosencard

    displayGameState Closed



isentThisType :: CardType->Entity->System' Bool
isentThisType atype ent = do
    ACardType ct <- getComponent ent
    return (ct==atype)

isitThisType :: CardType->Card->Bool
isitThisType atype card =
    card.cardType == atype

playableLands :: System'()
playableLands=cmapM_ $ \(LandsPlayed landsplayed, LandsPerTurn landsperturn) -> do
    if landsplayed<landsperturn
        then do
            cih<-cardsinhand
            let landscih=filter (\card -> isitThisType Land card) (cih)
            if null landscih
            then do
                liftIO (print ("Sorry, you don't have the lands in hand right now"))
                displayGameState Open
            else do
                return ()
            chosenland<-getUserChoice (landscih) (\thisland -> do
                    return thisland.cardName
                )
            playLand chosenland
            displayGameState Open
        else do
            liftIO (print ("You have played your lands for the turn. Please do something else."))
            displayGameState Open

showGS :: System'()
showGS = do
    liftIO (putStr ("\n"))
    liftIO (print ("Game State:"))
    liftIO (putStr ("\n"))
    cih<-cardsinhand
    liftIO (print ("the Hand: "))
    forM_ (zip [1 ..] (cih)) $ \(ix, thiscard) -> do
        liftIO (print (show ix ++ ". " ++ thiscard.cardName))
    liftIO (putStr ("\n"))
    cib<-cardsonbattlefield
    liftIO (print ("the Battlefield: "))
    forM_ (zip [1 ..] (cib)) $ \(ix, thiscard) -> do
        ACardName name<-getComponent thiscard
        liftIO (print (show ix ++ ". " ++ name))
    liftIO (putStr ("\n"))

showStack :: System'()
showStack = do
    cis<-effectsonstack
    liftIO (print ("the Stack: "))
    forM_ (zip [1 ..] (cis)) $ \(ix, ((ent,actions))) -> do
        liftIO (putStr (show ix ++ ". " ))
        forM_ (actions) $ \ (action)-> do
            liftIO (putStr ((show action)))
        liftIO (putStr ("\n"))
    liftIO (putStr ("\n"))

showMana :: System'()
showMana = do
    liftIO (print ("current Mana: "))
    cmapM $ \ (CurrentMana cm)->liftIO (print (cm))
    liftIO (putStr ("\n"))

displayGameState :: StateofGame->System'()
displayGameState sog = do
    showGS
    showStack
    showMana
    abilityorcast sog


checkStateBased :: System'()
checkStateBased = do
    cmapM $ \ (TheBattlefield, EntitiesinZone ents)-> do
        forM_ ents $ \ (ent) -> do
            ACardType ct<-getComponent ent
            case ct of
                Creature ->do
                        AToughness bt<-getComponent ent
                        case bt of
                            0->destroyTargets ent
                            _->return ()
                        --check if any creatures have toughness 0, kill them
                _->return ()
    cmapM $ \ (PlayerLP x)->do
        when (x<=0) $ endGame NoLife
    --check if any player is at 0 or less life

data GameEnd = Deckout | NoLife

endGame :: GameEnd->System'()
endGame Deckout = do
    liftIO (print ("no cards left in your library. Game Over!"))
    error "Game Completed - Deckout"
endGame NoLife = do
    liftIO (print ("You reached 0 Life. Game Over!"))
    error "Game Completed - Life Reached Zero"

nextTurn :: System'()
nextTurn = do
    cmap $ \ (LandsPlayed _, LandsPerTurn lpt)-> LandsPlayed 0
    cmap $ \ (CurrentMana _)->CurrentMana 0
    cmap $ \ (IsTapped _)->IsTapped False
    cmapM $ \(TheLibrary, CardsinZone cardsinlibr) -> do
        if null cardsinlibr
            then do endGame Deckout
            else do
                drawcard
                displayGameState Open



abilityorcast  :: StateofGame->System'()
abilityorcast Open = do
    checkStateBased
    let acceptables = [(1::Integer),2,3,4]
    cih<-cardsinhand
    forM_ (zip [1 ..] (cih)) $ \(ix, thiscard) -> do
        liftIO (print (show ix ++ ". " ++ thiscard.cardName))
    aborc<-obtainInput ("1. Activate Ability  2. Cast Card(s) 3. Play Land 4. Pass Priority")
    if aborc `elem` acceptables
        then do
            case aborc of
                1 -> activatableAbilities Open
                2-> castableCards Open
                3-> playableLands
                4-> do
                    nextTurn
                _-> do
                    liftIO (putStrLn "invalid option!")
                    abilityorcast Open
        else do
            liftIO (putStrLn "invalid option!")
            abilityorcast Open
abilityorcast Closed = do
    checkStateBased
    let acceptables = [(1::Integer),2,3,4]
    cih<-cardsinhand
    forM_ (zip [1 ..] cih) $ \(ix, thiscard) -> do
        liftIO (print (show ix ++ ". " ++ thiscard.cardName))
    aborc<-obtainInput ("1. Activate Ability  2. Cast Card(s) 3. Resolve Next Effect 4. Let Stack Resolve")
    if aborc `elem` acceptables
        then do
            case aborc of
                1 -> activatableAbilities Closed
                2-> castableCards Closed
                3-> resolveEffect One
                4-> do
                    resolveEffect All
                    displayGameState Open
                _-> do
                    liftIO (putStrLn "invalid option!")
                    abilityorcast Closed
        else do
            liftIO (putStrLn "invalid option!")
            abilityorcast Closed

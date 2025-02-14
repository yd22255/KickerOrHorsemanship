{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedRecordDot    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module SimpleCard where

import Apecs

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
        attributes :: [Attribute],
        currentZone :: Zone
    } deriving Show

appendcard :: Card->[Card]->[Card]
-- appendcard thiscard cards = foldr (:) [thiscard] cards
appendcard thiscard cards = cards ++ [thiscard]
--i think this blue line is wrong but correct me if thats not the case

removeCard :: Eq a => a -> [a] -> [a]
removeCard _ []                 = []
removeCard cardid (anid:ids) | anid == cardid    = removeCard anid ids
                    | otherwise = anid : removeCard anid ids

data Zone = Hand | Battlefield | Graveyard | Library | Exile | Stack | None deriving Show

data Action = ChooseTarget TargetType| CounterTarget | AddMana Int| DrawCards Int | DoNothing | Then Action Action deriving Show


data Cost = Tap deriving Show

data Trigger = OnCast | OnResolve deriving Show

data TargetType = Spell deriving Show

data Attribute =
    BasePower Int
    | BaseToughness Int
    deriving Show

basicLand :: Card
basicLand = Card {
    cardName = "BasicLand",
    cardType = Land,
    manaCost=0,
    onActivate = [(Tap,AddMana 1)],
    onEvent = [],
    attributes = [],
    currentZone = None
}

counterspell :: Card
counterspell = Card {
    cardName = "Counterspell",
    cardType = Instant,
    manaCost=2,
    onActivate = [],
    onEvent = [(OnCast, ChooseTarget Spell ), (OnResolve, CounterTarget)],
    attributes = [],
    currentZone = None
}

divination :: Card
divination = Card {
    cardName = "Divination",
    cardType = Sorcery,
    manaCost=3,
    onActivate = [],
    onEvent = [(OnResolve, DrawCards 2)],
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
    attributes = [],
    currentZone = None

}

--use apecs stuff, look at apecs github and jess boidemic project
--generate the land as an entity
--put it in a zone called Hand
--put it on the battlefield
--if possible, tap it for mana



newtype CurrentMana = CurrentMana Int deriving (Show)
instance Component CurrentMana where type Storage CurrentMana = Unique CurrentMana
--i tried this as a global but things got weird so it's a unique

data IsCard = IsCard deriving Show
instance Component IsCard where type Storage IsCard = Map IsCard
--should the zones be globals instead??
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

newtype CurrentTargets = CurrentTargets [Entity] -- needs the ability to target Entities as well: another type???
instance Component CurrentTargets where type Storage CurrentTargets = Map CurrentTargets

makeWorld "World" [

    ''IsCard, ''ACardName, ''ACardType, ''AManaCost, ''AnOnActivate, ''AnOnEvent, ''AnAttributes, ''AZone,
 ''TheHand, ''TheGraveyard, ''TheBattlefield, ''TheStack, ''TheLibrary, ''TheExile, ''CardsinZone, ''CurrentTargets, ''CurrentMana,
    ''EntitiesinZone
    ]
--i'm not sure if thehand, thegy, the library etc are necessary, but i put them in to try and make binding easier for cmaps 

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
    drawcards 3
    c<-cardsinhand
    debug c

debug :: Show a => a -> System' ()
debug x = liftIO (print x)


cardsonbattlefield :: System' [[Entity]]
cardsonbattlefield = collect $ \ (TheBattlefield, EntitiesinZone entities) -> Just entities


cardsinhand :: System' [[Card]]
cardsinhand = collect $ \ (TheHand, CardsinZone cards) -> Just cards

cardsinlib :: System' [[Card]]
cardsinlib = collect $ \ (TheLibrary, CardsinZone cards) -> Just cards

cardsonstack :: System' [[Entity]]
cardsonstack = collect $ \ (TheStack, EntitiesinZone entities) -> Just entities

effectsonstack :: System' [(String,[(Trigger, Action)])]
effectsonstack = collect $ \ (AnOnEvent triggeractions, AZone Stack, ACardName name) -> Just (name,triggeractions)
--tried a version of this where it returned the entity as well, but this became somewhat problematic -- not sure if i need to or not just wanted to record it
    --becoming maybe necessary to return entity as well, since I need to grab the targets for certain effects -- but equally thats not just in the entity

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
castnewcard :: Card-> System' ()
castnewcard card = do
    newEntity_
        (IsCard,
        ACardName card.cardName,
        ACardType card.cardType,
        AManaCost card.manaCost,
        (AnOnActivate card.onActivate,
        AnOnEvent card.onEvent),
        AnAttributes card.attributes,
        CurrentTargets [],
        AZone Stack
        )

playLand :: Card->System'()
playLand land = do
    newcard land
    --putCardInPlace land.cardID Hand Battlefield


castCard :: Card->System'()
castCard card = do
    castnewcard card
    --resolve its onCast trigger

resolveStack :: System'()
resolveStack = do
    triggeractions<-effectsonstack    
    resolvingTriggeractions triggeractions

drawcard :: System'()
drawcard = cmapM $ \(TheLibrary, CardsinZone cards) -> 
    case cards of
        [] -> return(CardsinZone cards)
        (card : cards') -> do
            cmap $ \ (TheHand, CardsinZone handcards) -> CardsinZone(appendcard card handcards)
            return (CardsinZone cards')

drawcards :: Int->System'()
drawcards 0 = return()
drawcards x = do 
    drawcard
    drawcards (x-1)

addmana :: Int->System'()
addmana x = cmap $ \(CurrentMana mananow) -> CurrentMana (mananow+x)

paymana :: Int->System'()
paymana x = cmap $ \(CurrentMana mananow) -> CurrentMana (mananow-x)

setmana :: Int->System'()
setmana x = cmap $ \(CurrentMana mananow) -> CurrentMana x

obtainInput :: String->SystemT World IO String --this might come back to bite me?
obtainInput prompt = do
    liftIO(putStrLn prompt)
    target<-liftIO(getLine)
    return target


findtarget :: String->System' [[Entity]]
findtarget targetname = collect $ \ (TheStack, EntitiesinZone entities) -> entities
--i know for a fact there's a way to filter collect but the syntax is being annoying and it's 2am


choosetarget :: (TargetType, String)->System'()
--do run a function that prompts the user to target something, and feed it back into this 
choosetarget (targettype, name) = do
    targetname<-obtainInput "select a target" --("hello")
    target<- findtarget targetname 
    cmapIf (\(AZone Stack, ACardName thename ) -> name == thename)
        (\(ACardName thename, CurrentTargets cards)->CurrentTargets target)
    debug(targetname)
-- not sure how to look at choosetarget here, because it's in some way associating the target with the entity, 
-- but idk whether I want to associate that with the capital E Entity or not

parseaction :: (Action,String)->System'()
parseaction (DoNothing, _) = return()
parseaction (DrawCards x, _) = drawcards x
--parseaction (CounterTarget, name): counterspell name -> take the currenttargets of name, then cmap to remove those entities from the stack
parseaction (ChooseTarget target, name) = choosetarget (target, name)
parseaction (AddMana x, _) = addmana x
parseaction (Then x y, z) = do 
    parseaction (x,z)
    parseaction (y,z) 

--[(Entity,[(Trigger,Action)])]
resolvingTriggeractions :: [(String,[(Trigger,Action)])] -> System'()
resolvingTriggeractions [] = parseaction (DoNothing,"")
resolvingTriggeractions ((name,[(OnResolve,action)]):tas) = do
    parseaction (action,name)
    resolvingTriggeractions tas
resolvingTriggeractions ((name,[(OnCast,_)]):tas) = do
    parseaction (DoNothing,name) 
    resolvingTriggeractions tas
resolvingTriggeractions _ = parseaction (DoNothing,"")

-- iterateTriggeractions [e,[(trigger,action)]:tas] = if trigger = onresolve: pass action into a parsing function which carries it out, then iterateTriggeractions tas
--i THINK i need to separate oncast and onresolve again, because they're triggered in entirely different circumstances, and it makes a generic parsing function really rough
--do this recursively? go through, try and resolve their onResolves

--destroyPermanent :: Entity->System'()
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

-- FIGURE OUT HOW TO EQUALIZE THEZONE AND ZONE HERE!! More generally how do we set up Eq-able things


-- gameChecks :: System' ()
-- gameChecks = do



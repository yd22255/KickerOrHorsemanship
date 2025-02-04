{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedRecordDot    #-}
{-# LANGUAGE TemplateHaskell #-}
module SimpleCard where

import Apecs
-- import SimpleCard (initWorld)

data CardType = Instant
        | Sorcery
        | Creature
        | Artifact
        | Enchantment
        | Land
        deriving Eq


data Card =
    Card
    {   cardName :: String,
        cardType :: CardType,
        manaCost::Int,
        onActivate :: [(Cost,Action)],
        onEvent :: [(Trigger, Action)],
        attributes :: [Attribute],
        currentZone :: Zone,
        cardID :: Int
    }

appendcard :: Int->[Int]->[Int]
appendcard thisid ids = foldr (:) [thisid] ids
--i think this blue line is wrong but correct me if thats not the case

removeCard :: Eq a => a -> [a] -> [a]
removeCard _ []                 = []
removeCard cardid (anid:ids) | anid == cardid    = removeCard anid ids
                    | otherwise = anid : removeCard anid ids

data Zone = Hand | Battlefield | Graveyard | Library | Exile deriving Show

data Action = ChooseTarget TargetType| CounterTarget | AddMana Int| DrawCards Int

data Cost = Tap

data Trigger = OnCast | OnResolve

data TargetType = Spell

data Attribute =
    BasePower Int
    | BaseToughness Int

basicLand :: Card
basicLand = Card {
    cardName = "BasicLand",
    cardType = Land,
    manaCost=0,
    onActivate = [(Tap,AddMana 1)],
    onEvent = [],
    attributes = [],
    currentZone = Library,
    cardID = 0
}

counterspell :: Card
counterspell = Card {
    cardName = "Counterspell",
    cardType = Instant,
    manaCost=2,
    onActivate = [],
    onEvent = [(OnCast, ChooseTarget Spell), (OnResolve, CounterTarget)],
    attributes = [],
    currentZone = Library,
    cardID = 1
}

divination :: Card
divination = Card {
    cardName = "Divination",
    cardType = Sorcery,
    manaCost=3,
    onActivate = [],
    onEvent = [(OnResolve, DrawCards 2)],
    attributes = [],
    currentZone = Library,
    cardID = 2
}

basicGuy :: Card
basicGuy = Card {
    cardName = "Memnite",
    cardType = Creature,
    manaCost=0,
    onActivate = [],
    onEvent = [],
    attributes = [BasePower 1, BaseToughness 1],
    currentZone = Library,
    cardID = 3
}

solRing :: Card
solRing = Card {
    cardName = "Sol Ring",
    cardType = Artifact,
    manaCost=1,
    onActivate = [(Tap, AddMana 2)],
    onEvent = [],
    attributes = [],
    currentZone = Library,
    cardID = 4
}

--use apecs stuff, look at apecs github and jess boidemic project
--generate the land as an entity
--put it in a zone called Hand
--put it on the battlefield
--if possible, tap it for mana





data IsCard = IsCard deriving Show
instance Component IsCard where type Storage IsCard = Map IsCard
--should the zones be globals instead??
data TheLibrary = TheLibrary deriving Show
instance Component TheLibrary where type Storage TheLibrary = Unique TheLibrary

data TheExile = TheExile deriving Show
instance Component TheExile where type Storage TheExile = Unique TheExile

data TheHand = TheHand deriving Show
instance Component TheHand where type Storage TheHand = Unique TheHand

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

newtype AnId = AnId Int
instance Component AnId where type Storage AnId = Map AnId

data IsZone = IsZone deriving Show
instance Component IsZone where type Storage IsZone = Map IsZone

newtype WhichZone = WhichZone Zone
instance Component WhichZone where type Storage WhichZone = Map WhichZone

newtype CardsinZone = CardsinZone [Int]
instance Component CardsinZone where type Storage CardsinZone = Map CardsinZone

makeWorld "World" [

    ''IsCard, ''ACardName, ''ACardType, ''AManaCost, ''AnOnActivate, ''AnOnEvent, ''AnAttributes, ''AZone, ''AnId,
    ''IsZone, ''TheHand, ''TheGraveyard, ''TheBattlefield, ''TheLibrary, ''TheExile, ''WhichZone, ''CardsinZone
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
    newcard basicLand
    putCardInPlace basicLand.cardID Library Hand
    c <- cardsonbattlefield
    debug c

debug :: Show a => a -> System' ()
debug x = liftIO (print x) 

cardsonbattlefield :: System' [(Entity, Zone)]
cardsonbattlefield = collect $ \ (AZone zone, Entity x) -> Just (Entity x, zone)

createZones :: System'()
createZones= do
    newEntity_
        (IsZone,
        TheHand,
        WhichZone Hand,
        CardsinZone []
        )
    newEntity_
        (IsZone,
        TheGraveyard,
        WhichZone Graveyard,
        CardsinZone []
        )
    newEntity_
        (IsZone,
        TheLibrary,
        WhichZone Library,
        CardsinZone []
        )
    newEntity_
        (IsZone,
        TheBattlefield,
        WhichZone Battlefield,
        CardsinZone []
        )
    newEntity_
        (IsZone,
        TheExile,
        WhichZone Exile,
        CardsinZone []
        )
newcard :: Card-> System' ()
newcard card= do
    newEntity_
        (IsCard,
        (ACardName card.cardName,AnId card.cardID),
        ACardType card.cardType,
        AManaCost card.manaCost,
        AnOnActivate card.onActivate,
        AnOnEvent card.onEvent,
        AnAttributes card.attributes,
        AZone card.currentZone
        )

playLand :: Card->System'()
playLand land = do
    putCardInPlace land.cardID Hand Battlefield
    addCardToPlace land.cardID Battlefield
--how do i access an entity? because i dont think this is going to work right now -- but the thesis statement is there?

--the only place entities exist is stack/battlefield, gy doesnt exist. library/hand just store Cards. 
--resolveStack !!

--all three of these effectively should function in sequence to update a card's position, wasn't sure how cmap works 
--(ie if i could resolve these in sequence with one cmap) 
--because the documentation seems shockingly vacant
putCardInPlace :: Int->Zone->Zone-> System'()
putCardInPlace thisid zone newzone = cmap $ \(AnId thisid, AZone zone) -> AZone newzone

addCardToPlace :: Int->Zone->System'()
addCardToPlace thisid zone = cmap $ \(WhichZone zone, CardsinZone ids) -> CardsinZone (appendcard thisid ids)

removeCardFromPlace :: Int->Zone->System'()
removeCardFromPlace thisid zone = cmap $ \(WhichZone zone, CardsinZone ids) -> CardsinZone (appendcard thisid ids)
--not sure why 'thisid' and 'zone' are shadowing binding here, i want to make sure the card is of that id and then move it 
-- not sure how
-- equalise zone and 


-- gameChecks :: System' ()
-- gameChecks = do



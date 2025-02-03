{-# LANGUAGE OverloadedRecordDot #-}

module Card
    (Card (Card),
    Entity(Entity),
    CardType (..),
    ManaCost (..),
    Keyword (..),
    Colour (..),
    Action (..),
    Trigger(..),
    Zones(..),
    cardName,
    cardType,
    permanentType,
    manaCost,
    text,
    keywords,
    colour,
    targetZones,
    owner,
    attributes,
    onEvent,
    tapped,
    card 
    )where 

import CreatureType
--import Keyword

data CardType = Instant
        | Sorcery
        | Creature
        | Artifact
        | Enchantment
        | Land 
        | Planeswalker
        | ArtifactCreature
        | EnchantmentCreature
        | LandCreature
        deriving Eq

--reworked this into one card type, removing Permanent

--datatypes inside datatypes -> i can do it

data Card = 
    Card
    {   cardName :: String,
        cardType :: CardType,
        manaCost::ManaCost,
        text::String,
        keywords::[Keyword], 
        onEvent :: [(Trigger, Action)],
        targetZones::[Zones],
        attributes :: [Attribute],
        owner:: Player
    }



-- class Permanent CardType where 
--     instance Permanent Creature where
--     instance Permanent Enchantment where
--     instance Permanent Artifact where
--     instance Permanent Land where 
--     instance Permanent Planeswalker where
--adapted from Tappable
-- maybe look into smart constructor instead??

data Zones = Graveyard | Battlefield | Stack | Hand | Library
data Player = Player1 | Player2
data ManaCost = W Int Phyrexian| U Int Phyrexian| B Int Phyrexian| R Int Phyrexian| G Int Phyrexian| Colourless Int | Generic Int| 
  And ManaCost ManaCost | Or ManaCost ManaCost
--technically speaking ManaCost should maybe be split into a type for each the colours and the combinators
--because you can't have triple hybrid mana (yet?). didn't do this because it seems like arbitrary complication
type Phyrexian = Bool

---------------do more of these
data Action = DrawCards Int | Tap Target | Counter Target |Exile Target| Destroy Target | 
Add ManaCost | Mill Int | MoveTo Card Zone | Discard Target | 
 Choice Action Action | Then Action Action | ChooseTarget [Attributes]

data TargetType = Permanent | TappedCreature | NonCreatureSpell | AnySpell |
 ArtifactPermanent | EnchantmentPermanent | CreaturePermanent| LandPermanent| 
 ArtifactSpell | EnchantmentSpell | CreatureSpell | InstantSpell | SorcerySpell | CardInHand Player | 
 |And TargetType TargetType | Or TargetType TargetType |

data Trigger = OnCast TargetType| OnResolve | OnETB | OnDies | OnDrawn| OnActivate| OnTap|  And Trigger Trigger | Or Trigger Trigger | 

data Attribute CardType where 
    Colour :: Colour-> Attribute CardType
    Tapped :: Permanent Creature->Bool -> Attribute Creature 
    Power :: Int -> Attribute Creature
    Toughness :: Int -> Attribute Creature 
    CreatureType :: CreatureType -> Attribute Creature
    Activateable :: Permanent CardType->Bool->Attribute CardType --i am not sure if this syntax works, but it gets the message across

-- idea here is to make Activatable an Attribute that we can use, not sure if there's a better version of this though
---------------

data Keyword = FirstStrike | DoubleStrike | Menace | Defender | Vigilance | Trample |
 Lifelink | Deathtouch | Haste | Flash | Flashback | Flying | Reach |
  Hexproof | Ward Int| Kicker| Indestructible

data Colour = White | Blue | Black | Red | Green | And Colour Colour
--i think i need to remove Colourless for the sake of multicoloured cards? 

-- playCard :: Card -> System ()
-- playCard 

data Entity =
  Entity --the idea is to use Entity as 'the card when in play' i think?
    {   eID       :: Int,
        cardName :: String,
        cardType :: String,
        keywords :: [String],
        colour :: String,
        permanentType::[String],
        tapped :: Int,

        --update    :: GameState -> Entity
  } -- should this contain card values? if so update would make sense
    --that would probably mean falling Card into GameState -- maybe this is ok though?
    --oh, or we just keep Entity in card and have GameState import Card instead?
    
-- instance Show Entity where
--     show Entity{eID} = show eID

type Targets = Int

card:: Card->Int->Entity
card c i = 
    Entity
  { eID       = i,
    
    
--   , update    = 
  }

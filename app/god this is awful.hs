--module Main where

import Apecs


makeWorld "World" []
world<-initWorld

type System' a = System World a

data CardType = Permanent
        | Instant
        | Sorcery
        deriving Eq

data PermanentType = Creature
        | Artifact
        | Enchantment
        | Land 
        | Planeswalker
        | None
        deriving Eq

data GameAction = CastSpell
        | PlayLand
        | ActivateAbility
        deriving Eq

data Player = Player1 | Player2

newtype CardName= CardName String deriving Eq
instance Component CardName where type Storage CardName = String
newtype Text= Text String deriving Eq-- this should probably be a data somehow?
newtype Targets= Targets ()
type Resolves=String--(?)
type Controller = Player
--newtype Card = CardName CardType (Maybe PermanentType) Text (Maybe Targets) Controller Zone

--class Zone a where

        

-- validcast:: Card -> Zone->Zone
-- validcast (Card cn ct t tgts) = 

--i'm pretty sure i just need apecs installed to make this work,
--message e+j about it weds/thurs depending when you see this

--the current idea is to use Zone as a class and battlefield, gy, hand etc as instances
--and to change the position of a given card i think i need ECS active

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

--cast :: Card->GameState -> Maybe GameState
--cast c = Add card to Stack in GameState?
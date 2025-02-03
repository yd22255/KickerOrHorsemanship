
--import Apecs

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

--this should probably have a Maybe included on PermanentType but i couldnt figure out how to do it
data Zone = End
        | Card CardName CardType PermanentType Controller Zone

--i'm pretty sure i just need apecs installed to make this work,
--message e+j about it weds/thurs depending when you see this

--the current idea is to use Zone as a class and battlefield, gy, hand etc as instances
--and to change the position of a given card i think i need ECS active

--data Graveyard = End
    --    | Card CardType Maybe PermanentType Graveyard

type Permanent = Int
type Instant = Int
type Sorcery = Int
type CardName = String
type Controller = String

--this is more like a shallow embedding, try to change it to deep
card :: CardName -> CardType->PermanentType->(Permanent, Instant, Sorcery)->(Permanent, Instant, Sorcery)
card "Grizzly Bear" Permanent Creature(p, i, s) = ((p+1), i, s)
card "Counterspell" Instant None (p,i,s)=(p,(i+1),s)
card "Divination" Sorcery None (p,i,s)=(p,i,(s+1))
card "Sol Ring" Permanent Artifact (p, i, s) = ((p+1), i, s)
card "Counterbalance" Permanent Enchantment (p, i, s) = ((p+1), i, s)
card "Wastes" Permanent Land (p, i, s) = ((p+1), i, s)



--cast :: Card->GameState -> Maybe GameState
--cast c = Add card to Stack in GameState?
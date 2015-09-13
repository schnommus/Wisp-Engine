import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Lens (over)
import qualified Control.Lens as Lens
import Control.Concurrent

type ComponentType = String
type EntityID = Int
type SystemType = String

data ComponentData = 
    NumData Float | 
    StringData String deriving (Show)

data Component = Component {
    cType :: ComponentType,
    cData :: Map String ComponentData
} deriving (Show)

instance Eq Component where {
    (==) a b = cType a == cType b
}

data Entity = Entity {
    eId :: EntityID,
    components :: Vector Component
} deriving (Show)

instance Eq Entity where {
    (==) a b = eId a == eId b
}

data EntitySystem = EntitySystem {
    sType :: SystemType,
    sMask :: [ComponentType],
    processEntities :: World -> [Entity] -> World
}

instance Show EntitySystem where
    show es = sType es

data World = World {
    nextId :: EntityID,
    entities :: Vector Entity,
    systems :: [EntitySystem]
} deriving (Show)

-- Pull a type of data out of component data, returning the default if not found.
getComponentData :: Component -> String -> ComponentData -> ComponentData
getComponentData component dataId defaultData = 
    Map.findWithDefault defaultData dataId (cData component)

-- Gets component data with check for unavailability
getComponentDataUnsafe :: Component -> String -> ComponentData
getComponentDataUnsafe component dataId = 
    case Map.lookup dataId (cData component) of
        Just d -> d
        Nothing -> error "Attempted to get nonexistent component data"

-- Replace OR add new component data to a component
setComponentData :: Component -> String -> ComponentData -> Component
setComponentData inComponent dataId dataIn = 
    inComponent {cData = Map.insert dataId dataIn (cData inComponent)}

-- Add a new component to an entity
addComponent :: Entity -> Component -> Entity
addComponent entity component = case hasComponent entity (cType component) of
    False -> entity { components = Vector.cons component $ components entity } 
    True -> error "Can't have more than one of the same component"

-- Get a new component, with data
newComponent :: String -> [(String, ComponentData)] -> Component
newComponent componentType dataList = Component componentType (Map.fromList dataList)

-- Check if an entity has a component (JUST USE getComponent)
hasComponent :: Entity -> String -> Bool
hasComponent entity componentType = any (\comp -> cType comp == componentType) (components entity)

-- Attempt to get a component from an entity
getComponent :: Entity -> ComponentType -> Maybe Component
getComponent entity componentType = 
    find (\comp -> cType comp == componentType) (components entity) 

-- Attempt to get a component from an entity (Unsafe version, for systems)
fetchComponent :: Entity -> ComponentType -> Component
fetchComponent entity componentType = case 
    find (\comp -> cType comp == componentType) (components entity) of
        Just c -> c
        Nothing -> error "Attempted to fetch nonexistant component"

-- Component setter, replaces existing component
setComponent :: Entity -> Component -> Entity
setComponent entity component = 
     entity {components = (Vector.//) (components entity) [(target, component)] } 
     where target = case Vector.findIndex (==component) (components entity) of
              Just c -> c
              Nothing -> error "Attempted to set nonexistant component!"


-- Get all entities matching all component types
getAllWithComponents :: World -> [ComponentType] -> [Entity]
getAllWithComponents world componentTypes = Vector.toList $ 
    Vector.filter (\ent -> all (==True) [hasComponent ent c | c <- componentTypes] ) (entities world) 

-- Add a system to the world
addSystem :: World -> EntitySystem -> World
addSystem world system = world {systems = system:(systems world)}

-- Do processing functions of a single system
stepSystem :: World -> EntitySystem -> World
stepSystem world system = 
    (processEntities system) world (getAllWithComponents world (sMask system))

-- Get a new entity processing system
newSystem :: SystemType -> [ComponentType] -> (World -> [Entity] -> World) -> EntitySystem
newSystem systemType systemMask processingFunction = EntitySystem systemType systemMask processingFunction

-- Step all systems
stepWorld :: World -> World
stepWorld world = foldl' stepSystem world (systems world)

-- Get a blank world
newWorld :: World
newWorld = World 0 Vector.empty []

-- Get a blank entity
newEntity :: [Component] -> Entity
newEntity components = Vector.foldl' addComponent (Entity (-1) Vector.empty) (Vector.fromList components)

-- Add an entity to the world, give it an ID
addEntity :: World -> Entity -> World
addEntity world ent =
    world {entities = (ent {eId = (nextId world)} `Vector.cons` entities world), nextId = succ (nextId world)}

-- Given a function that updates an entity and returns a new one, return a function
-- that will update a world using this function.
updateEntity :: (Entity -> World -> Entity) -> World -> Entity -> World
updateEntity f world ent =
    world {entities = (Vector.//) (entities world) [(target, processed)] } 
    where processed = f ent world
          target = case Vector.findIndex (==ent) (entities world) of
              Just e -> e
              Nothing -> error "Attempted to update nonexistant entity!"

componentL componentType = Lens.lens ((flip fetchComponent) componentType) setComponent
dataL dataId = Lens.lens  ((flip getComponentDataUnsafe) dataId) ((flip setComponentData) dataId)
componentDataL componentType dataId = (componentL componentType . dataL dataId)

position :: Component
position = newComponent "pos" [
        ("x", NumData 0),
        ("y", NumData 0),
        ("vx", NumData 1),
        ("vy", NumData (-1)), 
        ("angle", NumData 0)
    ]

canShoot :: Component
canShoot = newComponent "can_shoot" [
        ("rate_of_fire", NumData 3.5),
        ("weapon_name", StringData "Obliterator")
    ]

spaceship :: Entity
spaceship = newEntity [position, canShoot]

applyVelocity :: Entity -> World -> Entity
applyVelocity ent world = 
    modNum ent "pos" "angle" (\angle -> atan2 vy vx) $
    modNum ent "pos" "x" (\x -> x + vx) $ 
    modNum ent "pos" "y" (\y -> y + vy) ent
    where vx = getNum ent "pos" "vx"
          vy = getNum ent "pos" "vy"

getNum ent com att = case Lens.view (componentDataL com att) ent of
    (NumData ret) -> ret
    _ -> error "Unexpected component data"

modNum ent com att f = 
    Lens.over (componentDataL com att) (\(NumData x) -> 
        NumData $ f x
    )


processVelocity :: EntitySystem
processVelocity = newSystem "processVelocity" ["pos"] (\world ents ->
    foldl' (updateEntity applyVelocity) world ents
    )


loop w = do
    let w' = stepWorld w
    putStrLn $ show w'
    threadDelay 100000
    loop w'

main = do
    let w = addSystem (addEntity newWorld spaceship) processVelocity
    loop w

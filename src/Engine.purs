module Engine(run, Action(..), Command(..)) where

import Prelude
import Data.Array ((:), singleton)
import Data.Tuple
import Data.Traversable (class Traversable, sequence)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import FRP.Behavior
import FRP.Event
import FRP.Event.AnimationFrame as FRP
import FRP.Event.Keyboard as FRP
import Debug.Trace (spy)
import Vector

data Action =
    KeyDown String
  | KeyUp String
  | Frame

data Command =
    Render
  | Noop

derive instance genericCommand :: Generic Command _
instance eqCommand :: Eq Command where
  eq = genericEq
instance showCommand :: Show Command where
  show = genericShow

doCommand :: Command -> Effect Unit
doCommand = logShow

combineEvents :: forall m a. Traversable m => m (Event a) -> Event a
combineEvents events = makeEvent \k->
  sequence (map (flip subscribe k) events) >>= pure <<< void <<< sequence

actionEvent :: Event Action
actionEvent = combineEvents
  [ map KeyDown FRP.down
  , map KeyUp FRP.up
  , map (const Frame) FRP.animationFrame
  ]

run :: forall a. (Action -> a -> Tuple a Command) -> a -> Effect Unit
run f init = void $ subscribe tick (doCommand <<< snd)
  where
    comb x (Tuple s _) = f x s
    tick = fold comb actionEvent (Tuple init Noop) 

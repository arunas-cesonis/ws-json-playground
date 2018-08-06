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
import Graphics.Drawing (Drawing)
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

doCommand :: forall a. Tuple a Command -> Effect Unit
doCommand = logShow <<< snd

combineEvents :: forall m a. Traversable m => m (Event a) -> Event a
combineEvents events = makeEvent \k->
  sequence (map (flip subscribe k) events) >>= pure <<< void <<< sequence

actionEvent :: Event Action
actionEvent = combineEvents
  [ map KeyDown FRP.down
  , map KeyUp FRP.up
  , map (const Frame) FRP.animationFrame
  ]

type Run a =
  { update :: Action -> a -> Tuple a Command
  , draw :: a -> Drawing
  }

run :: forall a. Run a -> a -> Effect Unit
run {update, draw} init = void $ subscribe tick doCommand
  where
    f x (Tuple s _) = update x s
    tick = fold f actionEvent (Tuple init Noop) 

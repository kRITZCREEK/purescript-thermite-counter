module Main where

import Button (ButtonState, initButton, ButtonAction, buttonSpec)
import Thermite as T
import React.DOM as R
import DOM (DOM)

import Data.Function (($))
import Data.Unit (Unit, unit)
import Data.Semigroup ((<>))
import Data.Ring ((-))
import Data.Semiring ((+))
import Data.Maybe (Maybe (..))
import Data.Show (show)
import Data.Lens.Lens (Lens', lens)
import Data.Lens.Prism (Prism', prism')
import Data.Lens.Getter ((^.))
import Data.Lens.Setter (over)
import Control.Monad (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)



type CounterState =
  { _count :: Int
  , _incButton :: ButtonState
  , _decButton :: ButtonState
  }

count :: Lens' CounterState Int
count = lens (\x -> x._count) (\x q -> x { _count = q })

incButton :: Lens' CounterState ButtonState
incButton = lens (\x -> x._incButton) (\x q -> x { _incButton = q })

decButton :: Lens' CounterState ButtonState
decButton = lens (\x -> x._decButton) (\x q -> x { _decButton = q })


init :: CounterState
init = { _count: 0, _incButton: initButton, _decButton: initButton }

data CounterAction
  = Increment
  | Decrement
  | IncButton (ButtonAction CounterAction)
  | DecButton (ButtonAction CounterAction)

_IncButton :: Prism' CounterAction (ButtonAction CounterAction)
_IncButton = prism' IncButton $ \x -> case x of
                                        IncButton y -> Just y
                                        _           -> Nothing

_DecButton :: Prism' CounterAction (ButtonAction CounterAction)
_DecButton = prism' DecButton $ \x -> case x of
                                        DecButton y -> Just y
                                        _           -> Nothing

counterSpec :: forall eff props
             . T.Spec (console :: CONSOLE | eff) CounterState props CounterAction
counterSpec = T.simpleSpec performAction render
  where
    inc = T.focus incButton _IncButton $ buttonSpec { _onClick: Increment
                                                    , _value: "Increment"
                                                    }
    dec = T.focus decButton _DecButton $ buttonSpec { _onClick: Decrement
                                                    , _value: "Decrement"
                                                    }

    render :: T.Render CounterState props CounterAction
    render dispatch props state children =
      [ R.text $ "Count: " <> show (state ^. count)
      ] <> (inc ^. T._render) dispatch props state children
        <> (dec ^. T._render) dispatch props state children

    performAction :: T.PerformAction (console :: CONSOLE | eff) CounterState props CounterAction
    performAction Increment _ state = do
      T.writeState $ over count (\x -> x+1) state
      lift $ liftEff $ log "incremented"
    performAction Decrement _ state = do
      T.writeState $ over count (\x -> x-1) state
      lift $ liftEff $ log "decremented"
    performAction action@(IncButton _) props state = do
      lift $ liftEff $ log "inc button action being performed"
      (inc ^. T._performAction) action props state
    performAction action@(DecButton _) props state = do
      lift $ liftEff $ log "dec button action being performed"
      (dec ^. T._performAction) action props state


main :: forall eff. Eff ( console :: CONSOLE
                        , dom :: DOM
                        | eff) Unit
main = do
  T.defaultMain counterSpec init unit

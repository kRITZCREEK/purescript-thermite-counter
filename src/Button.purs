module Button where

import Data.Unit (Unit, unit)
import Data.Function (($))
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)

import Thermite as T
import React.DOM as R
import React.DOM.Props as RP


type ButtonState = Unit

initButton :: Unit
initButton = unit

data ButtonAction = Clicked


buttonSpec :: forall eff props
            . { _value   :: String
              }
           -> T.Spec (console :: CONSOLE | eff) ButtonState props ButtonAction
buttonSpec details = T.simpleSpec performAction render
  where
    render :: T.Render ButtonState props ButtonAction
    render dispatch _ state _ =
      [ R.button [ RP.onClick $ \_ -> dispatch Clicked ]
          [R.text details._value]
      ]

    performAction :: T.PerformAction (console :: CONSOLE | eff)
                       ButtonState props ButtonAction
    performAction Clicked _ _ = lift $ liftEff $ log "button clicked!"

module Button where

import Data.Unit (Unit, unit)
import Data.Function (($))
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans.Class (lift)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP


type ButtonState = Unit

initButton = unit

data ButtonAction parentAction
  = Clicked parentAction


buttonSpec :: forall eff props parentAction
            . { _onClick :: parentAction
              , _value   :: String
              }
           -> T.Spec (console :: CONSOLE | eff) ButtonState props (ButtonAction parentAction)
buttonSpec details = T.simpleSpec performAction render
  where
    render :: T.Render ButtonState props (ButtonAction parentAction)
    render dispatch _ state _ =
      [ R.button [ RP.onClick $ \_ -> dispatch $ Clicked details._onClick ]
          [R.text details._value]
      ]

    performAction :: T.PerformAction (console :: CONSOLE | eff)
                       ButtonState props (ButtonAction parentAction)
    performAction (Clicked _) _ _ = lift $ liftEff $ log "button clicked!"

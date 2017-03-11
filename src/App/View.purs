module App.View where

import Pux.Html.Events
import App.Types as App
import Login.View as Login
import App.Routes (Route(..))
import Pux.Html (Html, div, forwardTo, li)
import Pux.Html.Attributes (className)
import Prelude hiding (div)
import App.NotFound

view :: App.State -> Html App.Action
view state =
  div
    []
    [ h1 [] [ link "/" [] [ text "P2H" ] ]
    , case state.route of
        Home -> map LoginAction $ Login.view state.login
        NotFound -> NotFound.view state
    ]

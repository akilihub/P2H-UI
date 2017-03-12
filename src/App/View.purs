module App.View where

import App.NotFound as AppNotFound
import Login.View as Login
import App.Routes (Route(..))
import App.Types (Action(..), State)
import Pux.Html (Html, div, h1, text)
import Pux.Html.Attributes (className)
import Pux.Router (link)
import Prelude hiding (div)

view :: State -> Html Action
view state =
  div
    [className "container"]
    [ h1 [] [ link "/" [] [ text "P2H" ] ]
    , case state.route of
        Home -> map LoginAction $ Login.view state.login
        NotFound -> AppNotFound.view state
    ]

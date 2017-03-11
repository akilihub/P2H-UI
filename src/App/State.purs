module App.State where

import App.Types
import Prelude
import Login.Types as Login
import Login.State (update, init) as Login
import App.Routes (Route(..))
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

init :: State
init =
  { login : Login.init
  , route : Home
  , status : "App started"
  }

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)

update (LoginAction action) state =
  Login.update action state.post
  # mapState (state { login = _ })
  # mapEffects LoginAction

-- update (PageView route) state = routeEffects route (state { route = route })

-- routeEffects :: Route -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)
-- routeEffects Home state = { state: state
--                           , effects: [ pure PL.RequestPosts ] }
--                           # mapEffects PostL

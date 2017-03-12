module App.State where

import App.Types (Action(..), State)
import App.Routes (Route(..))
import DOM (DOM)
import Login.State (update, init) as Login
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, mapEffects, mapState, noEffects)
import Prelude((#), ($))


init :: State
init =
  { login : Login.init
  , route : Home
  , status : "App started"
  }

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)

update (LoginAction action) state =
  Login.update action state.login
  # mapState (state { login = _ })
  # mapEffects LoginAction

update (PageView route) state = noEffects $ state

-- routeEffects :: Route -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)
--
-- routeEffects Home state = { state: state
--                           , effects: [ pure PL.RequestPosts ] }
--                           # mapEffects LoginAction

module App.Types where
import Prelude
import Login.Types as Login
import App.Routes (Route)


data Action
  = LoginAction Login.Action
   | PageView Route

type State =
  { loginState :: Login.State
  , route :: Route
  , status :: String
  }

module App.Types where
import Login.Types as Login
import App.Routes (Route)


data Action
  = LoginAction Login.Action
   | PageView Route

type State =
  { login :: Login.State
  , route :: Route
  , status :: String
  }

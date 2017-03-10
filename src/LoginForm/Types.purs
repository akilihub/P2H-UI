module LoginForm.Types where
import Data.Either (Either)
import Data.Maybe (Maybe)
import Pux.Html.Events (FormEvent)
import Data.Argonaut (jsonEmptyObject, class EncodeJson, encodeJson, (:=), (~>))

data Action = Submit (Maybe User) | Validate (Either String User) | AddUsername FormEvent | AddPassword FormEvent


newtype User = User
        { password :: String
        , username :: String
        }

type State =
    { user :: User
    , status :: String
    }

init :: State
init = { user: User { password : ""
                    , username : ""
                    }
       , status : ".."
       }

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User user)
    = "username" := user.username
    ~> "password" := user.password
    ~> jsonEmptyObject

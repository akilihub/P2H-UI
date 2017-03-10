module LoginForm.Types where
import Data.Argonaut (jsonEmptyObject, class EncodeJson, encodeJson, (:=), (~>))
import Data.Boolean (otherwise)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Pux.Html.Events (FormEvent)

-- Note that the returned payload that shows whether a user is authenticated or not is not handled here

data Action
  = SubmitInput (Either String User)
  | ValidateInput (Either String User)
  | AddUsername FormEvent
  | AddPassword FormEvent

newtype User = User
  { password :: String
  , username :: String
  }

type State =
    { user :: User
    , status :: String
    }

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User user)
    = "username" := user.username
    ~> "password" := user.password
    ~> jsonEmptyObject

validateInput :: User -> ValidateInput
validate User ( user@{password, username} )
  | length username < 2 = ValidateInput (Left "Missing username")
  | length password < 2 = ValidateInput (Left "Missing password")
  | length password < 2 && length password < 2 = ValidateInput (Left "Invalid Input")
  | otherwise           = ValidateInpit (Right user)


submitFormData :: ValidateInput -> SubmitInput
submitFormData = either (SubmitInput Left) (SubmitInput Right)

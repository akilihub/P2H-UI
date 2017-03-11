module Login.Types where
import Prelude
import Data.Argonaut (jsonEmptyObject, class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), (.?))
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.String (length)
import Pux.Html.Events (FormEvent)

-- Note that the returned payload that shows whether a user is authenticated or not is not handled here

data Action
  = SignIn User
  | ValidateForm
  | DisplayError String
  | UserNameChange FormEvent
  | PasswordChange FormEvent
  | ReceiveUserSession Session

newtype User = User
  { password :: String
  , username :: String
  }

newtype Session = Session
  { sessionId :: String
  , userType :: String
  , userId :: String
  }



type State =
  { user :: User
  , error :: String
  , status :: String
  , session :: Session
  }

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User user)
    = "username" := user.username
    ~> "password" := user.password
    ~> jsonEmptyObject

instance decodeJsonSession :: DecodeJson Session where
  decodeJson json = do
    obj <- decodeJson json
    sessionId <- obj .? "sessionId"
    userType <- obj .? "userType"
    userId <- obj .? "userId"
    pure $ Session { sessionId, userType, userId }

inputValidation :: User -> Either String User
inputValidation (User user)
  | length user.password < 4 && length user.username < 2  = Left "Invalid username and password"
  | length user.username < 2 = Left "Invalid username"
  | length user.password < 4 = Left "Invalid password"
  | otherwise           = Right (User user)

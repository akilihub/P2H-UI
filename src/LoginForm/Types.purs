module LoginForm.Types where
import Data.Argonaut (jsonEmptyObject, class EncodeJson, encodeJson, (:=), (~>))
import Data.Boolean (otherwise)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Pux.Html.Events (FormEvent)

-- Note that the returned payload that shows whether a user is authenticated or not is not handled here

data Action
  = SignIn
  | ValidateForm
  | DisplayError
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

type Error = String

type State =
  { user :: User
  , error :: Error
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

inputValidation :: User -> Either (Error User)
inputValidation User ( user@{ password, username } )
  | length password < 4 && length username < 2  = Left "Invalid username and password"
  | length username < 2 = Left "Invalid username"
  | length password < 4 = Left "Invalid password"
  | otherwise           = Right (User user)

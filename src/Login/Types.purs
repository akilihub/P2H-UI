module Login.Types where
import Data.Argonaut (jsonEmptyObject, class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), (.?))
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Generic (class Generic, gShow)
import Data.Show (class Show)
import Data.String (length)
import Prelude (bind, (<), ($), pure, (&&))
import Pux.Html.Events (FormEvent)


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

derive instance genericUser :: Generic User

instance showUser :: Show User where
  show = gShow

derive instance genericSession :: Generic Session

instance showSession :: Show Session where
  show = gShow

-- reading and writing Json http://www.purescript.org/learn/generic/

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

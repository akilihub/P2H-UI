module Login.Types where
import Data.Argonaut (jsonEmptyObject, class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), (.?))
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
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

derive instance genericUser :: Generic User _

instance showUser :: Show User where
  show = genericShow

derive instance genericSession :: Generic Session _

instance showSession :: Show Session where
  show = genericShow

-- reading and writing Json http://www.purescript.org/learn/generic/

-- myOptions :: Options
-- myOptions = defaultOptions

instance isForeignSession :: IsForeign Session where
  read = readGeneric (defaultOptions { unwrapSingleConstructors = true })

instance asForeignRecordSession :: AsForeign Session where
  write = toForeignGeneric defaultOptions

instance isForeignUser :: IsForeign User where
  read = readGeneric defaultOptions

instance asForeignRecordUser :: AsForeign User where
  write = toForeignGeneric defaultOptions

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

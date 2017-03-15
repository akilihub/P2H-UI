module NavSideBar.Types where

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Array (find)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Prelude (bind, ($), pure, (==))

newtype Document = Document
  { name :: Maybe String
  , id :: Maybe String
  , publicationStatus :: Maybe String
  }

type Documents = Array Document
type DocumentId = String

type State =
  { status :: String
  , userId :: String
  , activeDocument :: Maybe DocumentId
  , error :: String
  , documents :: Documents
  }

data Action
  = GetDocuments
  | RecieveDocuments  (Either String Documents)
  | SetActiveDocument DocumentId
  | DisplayError String

instance decodeJsonDocument :: DecodeJson Document where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    id <- obj .? "id" -- id forms url
    publicationStatus <- obj .? "publicationStatus"
    pure $ Document { name, id, publicationStatus }

getDocumentById :: DocumentId -> Documents -> Maybe Document
getDocumentById id = find (\(Document doc) -> doc.id == Just id )

module Stage.Types where

import Data.Argonaut (jsonEmptyObject, class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), (.?))
import Data.Array (find)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Prelude (bind, ($), pure, (==))


newtype HtmlSnippet = HtmlSnippet
  { id :: Maybe String
  , text :: Maybe HtmlText
  , classes :: Maybe String
  , styles :: Maybe String
  }

newtype Document = Document
  { name :: Maybe String
  , url :: Maybe String
  , id :: Maybe String
  , publicationStatus :: Maybe String
  }

type Documents = Array Document
type HtmlElementId = String
type HtmlText = String
type HtmlSnippets =  Array HtmlSnippet
type Pages = Array HtmlElementId

type State =
  { status :: String
  , userId :: String
  , activeDocument :: Maybe Document
  , error :: String
  , documents :: Documents
  , activeSnippet :: HtmlSnippet
  , snippets :: HtmlSnippets
  , activeDocumentSections :: Pages
  }

data Action
  = EditSelection HtmlSnippet
  | CommitSnippetEdit HtmlSnippet
  | SaveDocumentStatus (Either String String)
  | SaveDocumentChanges
  | GetDocuments
  | RecieveDocuments  (Either String Documents)
  | DisplayError String

instance encodeJsonHtmlSnippet :: EncodeJson HtmlSnippet where
  encodeJson (HtmlSnippet snippet)
    = "id" := snippet.id
    ~> "styles" := snippet.styles
    ~> "text" := snippet.text
    ~> "classes" := snippet.classes
    ~> jsonEmptyObject

instance decodeJsonDocument :: DecodeJson Document where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    url <- obj .? "url"
    id <- obj .? "id"
    publicationStatus <- obj .? "publicationStatus"
    pure $ Document { name, url, id, publicationStatus }

getDocumentById :: String -> Documents -> Maybe Document
getDocumentById id = find (\(Document doc) -> doc.id == Just id )

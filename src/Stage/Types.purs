module Stage.Types where

import Data.Argonaut (jsonEmptyObject, class EncodeJson, (:=), (~>))
import Data.Either (Either)
import Data.Maybe (Maybe)


newtype HtmlSnippet = HtmlSnippet
  { id :: Maybe String
  , text :: Maybe HtmlText
  , classes :: Maybe String
  , styles :: Maybe String
  }

type DocumentId = String
type HtmlText = String
type HtmlSnippets =  Array HtmlSnippet

type State =
  { status :: String
  , activeDocument :: Maybe DocumentId
  , error :: String
  , activeSnippet :: HtmlSnippet
  , snippets :: HtmlSnippets
  }

data Action
  = GetDocumentHtml (Maybe DocumentId)
  | EditSelection HtmlSnippet
  | CommitSnippetEdit HtmlSnippet
  | SaveDocumentStatus (Either String String)
  | SaveDocumentChanges
  | DisplayError String
  | NoOp -- means no operation / effect

instance encodeJsonHtmlSnippet :: EncodeJson HtmlSnippet where
  encodeJson (HtmlSnippet snippet)
    = "id" := snippet.id
    ~> "styles" := snippet.styles
    ~> "text" := snippet.text
    ~> "classes" := snippet.classes
    ~> jsonEmptyObject

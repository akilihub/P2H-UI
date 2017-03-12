module Stage.Types where

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

newtype HtmlSnippet = HtmlSnipet (Tuple HtmlElementId HtmlText)

newtype Document = Document
  { name :: String
  , url :: String
  , publicationStatus :: String
  }

type DocumentList a = Maybe (List a)

type HtmlElementId = String
type HtmlText = String
type HtmlSnippets =  List HtmlSnippet
type Pages = List HtmlElementId

type State =
  { status :: String
  , hasActiveDocument :: Boolean
  , userId :: String
  , activeDocument :: Document
  , documents :: List Document
  , sectionsInEditMode :: HtmlSnippets
  , activeDocumentSections :: Pages
  }

data Action
  = EditSnippets HtmlSnippets
  | CommitSnippets HtmlSnippets
  | GetDocuments (DocumentList Document)
  | GetCurrentDocument (Document)

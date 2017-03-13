module Stage.State where

import Control.Monad.Aff (attempt)
import DOM (DOM)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array (snoc)
import Data.Either (Either(..), either)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get, post)
import Prelude (bind, ($), (<>), pure, (<<<), show)
import Pux (EffModel, noEffects)
import Stage.Types (Action(..), Document(..), Documents, HtmlSnippet(..), State)

initDoc :: Document
initDoc = Document
  { name    : Nothing
  , url     : Nothing
  , id      : Nothing
  , publicationStatus : Nothing
  }

initSnippet :: HtmlSnippet
initSnippet = HtmlSnippet
  { id : Nothing
  , text : Nothing
  , classes : Nothing
  , styles : Nothing
  }

init :: State
init =
  { status : ".."
  , userId : ""
  , activeDocument : Just initDoc
  , error : ""
  , documents : []
  , activeSnippet : initSnippet
  , snippets : []
  , activeDocumentSections : []
  }

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)
update GetDocuments state =
  { state   : state { status = "fetching documents" }
  , effects : [ do
      res <- attempt $ get ("/api/docs/" <> state.userId)
      let decode r = decodeJson r.response :: Either String Documents
      let documents = either (Left <<< show) decode res
      pure $ RecieveDocuments documents
    ]
  }

update (RecieveDocuments (Left err)) state =
  { state : state { status = newStatus
            , error = err }
  , effects : [ do
      pure $ DisplayError (newStatus <> " : " <> err)
    ]
  } where
  newStatus =  "error getting documents check you are connected to the internet"

update (RecieveDocuments (Right documents)) state =
  noEffects $ state { status = "receieved documents", documents = documents }

update (EditSelection snippet) state =
  noEffects $ state { activeSnippet = snippet, status = "editting snipet"}

update (CommitSnippetEdit snippet) state =
  noEffects $ state { snippets = newsnippets, status = "commited snipt edits"} where
    newsnippets = snoc state.snippets state.activeSnippet


-- | Html snippets / selected sections are passed by a document on select handler in the view
update SaveDocumentChanges state@{ snippets : snippets } =
  { state : state { status = "saving doc "}
  , effects : [ do
      res <- attempt $ post ("/api/docs/") (encodeJson $ (map encodeJson snippets)) -- to test
      let decode r = decodeJson r.response :: Either String String
      let saveStatus = either (Left <<< show) decode res
      pure $ SaveDocumentStatus saveStatus
    ]
  }

update (SaveDocumentStatus (Left err)) state =
  { state : state { status = newStatus
            , error = err }
  , effects : [ do
      pure $ DisplayError (newStatus <> " : " <> err)
    ]
  } where newStatus =  "Document failed to save, check network connection"

update (SaveDocumentStatus (Right status)) state = noEffects $ state { status = "saved document", error = ""}

update (DisplayError err) state = noEffects $ state { error = err }

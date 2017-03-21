module Stage.State where

import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (message)
import DOM (DOM)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array (snoc)
import Data.Either (Either(..), either)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get, post)
import Prelude (bind, pure, show, ($), (<<<), (<>))
import Pux (EffModel, noEffects, onlyEffects)
import Stage.Util(dangerouslyInsertHml)
import Stage.Types (Action(..), DocumentId, HtmlSnippet(..), State)


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
  , activeDocument : Nothing
  , error : ""
  , activeSnippet : initSnippet
  , snippets : []
  }


-- insertHtml:: DomNode -> Eff (dom :: DOM) unit

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)

update (GetDocumentHtml (Just id)) state =
  noEffects $ state { state : state { activeDocument = Just id }

update (GetDocumentHtml (Nothing)) state =
  noEffects $ state { error = newError, status = newStatus} where
    newStatus = "couldnt retrieve document Id check internet connectedion"
    newError  = "Null document Id"

update (EditSelection snippet) state =
  noEffects $ state { activeSnippet = snippet, status = "editting snipet"}

update (CommitSnippetEdit snippet) state =
  noEffects $ state { snippets = newsnippets, status = "commited snipt edits"} where
    newsnippets = snoc state.snippets state.activeSnippet

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

update NoOp s = noEffects $ s

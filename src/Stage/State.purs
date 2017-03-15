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
import Pux (EffModel, noEffects)
import Stage.Types (Action(..), HtmlSnippet(..), State, DocumentId)


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

getDocHtml :: Action -> Either String String
getDocHtml (GetDocumentHtml (Just id)) = do
  res <- attempt $ get ("/api/docs/" <> id)
  case res of
    (Left err)  -> Left message err
    (Right r)   ->  Right r.response

getDocHtml (GetDocumentHtml (Nothing)) = Left "No document id provided"

getDocHtml _  = Left "used wrong Action type"

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)

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

update (GetDocumentHtml id) state = noEffects $ state { activeDocument = Just id }

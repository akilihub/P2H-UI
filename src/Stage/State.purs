module Stage.State where

import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (Error, message)
import DOM (DOM)
import Data.Argonaut (decodeJson)
import Data.Array (snoc)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)
import Prelude (bind, ($), (<>), pure, (<<<), map, show)
import Pux (EffModel, noEffects)
import Stage.Types (Action(..), Document(..), Documents, State, getDocumentById)

initDoc :: Document
initDoc = Document
  { name    : Nothing
  , url     : Nothing
  , id      : Nothing
  , publicationStatus : Nothing
  }

init :: State
init =
  { status : ".."
  , userId : ""
  , activeDocument : Just initDoc
  , error : ""
  , documents : []
  , sectionsInEditMode : []
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
  newStatus =  "error getting documents checking your connected to the internet"

update (RecieveDocuments (Right documents)) state =
  noEffects $ state { status = "receieved documents", documents = documents }

update (SetActiveDocument id) state@{ documents : documents} = noEffects $ state
  -- noEffects $ state { activeDocument = (getDocumentById id), status = "set active Document: " <> id }

update (CommitSnippets snippets) state = noEffects $ state

update (EditSnippets snippets) state = noEffects $ state

update (DisplayError err) state = noEffects $ state

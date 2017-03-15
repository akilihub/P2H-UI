module NavSideBar.State where

import Control.Monad.Aff (attempt)
import DOM (DOM)
import Data.Argonaut (decodeJson)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)
import Prelude (bind, ($), (<>), pure, (<<<), show)
import Pux (EffModel, noEffects)
import NavSideBar.Types (Action(..), Document(..), Documents, State)

initDoc :: Document
initDoc = Document
  { name    : Nothing
  , id      : Nothing
  , publicationStatus : Nothing
  }

init :: State
init =
  { status : ".."
  , userId : ""
  , activeDocument : Nothing
  , error : ""
  , documents : []
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

update (SetActiveDocument id) state =
  noEffects $ state { status = "started doc editing", activeDocument = Just id }

update (DisplayError err) state = noEffects $ state { error = err }

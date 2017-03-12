module Stage.State where

import Control.Monad.Aff (attempt)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel)
import Stage.Types (Action(..), State)



update :: Action -> State ->  EffModel State Action (dom :: DOM, ajax :: AJAX)

update (GetDocuments docs) state =
  { state : state {status = "fetching documents"}
  , effects : do [
      -- res <- attempt $ get "/api/docs/" <> "userId"  
    ]
  }

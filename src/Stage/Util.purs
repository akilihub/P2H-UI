module Stage.Util where

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (message)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)
import Prelude (bind, ($), (<>), pure)
import Stage.Types (DocumentId)

addDocHtml :: Maybe DocumentId -> Aff (ajax :: AJAX) String
addDocHtml (Just id) = do
    res <- attempt $ get ("/api/docs/" <> id)
    case res of
      (Left err)  -> pure $ message err
      (Right r)   -> pure $ r.response

addDocHtml Nothing = pure $ "Couldnt retrieve document"

module Stage.View where
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (runPure)
import Control.Monad.Eff.Class (liftEff)
import DOM.Event.KeyboardEvent (KeyLocation(..))
import DOM.Node.Document (documentElement)
import Data.Maybe (Maybe)
import Prelude (bind, pure, ($))
import Pux.Html (Html, ul, li, form, input, div, text)
import Pux.Html.Attributes (className, dangerouslySetInnerHTML, name, type_, value)
import Pux.Html.Events (onChange, onSubmit)
import Stage.State (update)
import Stage.Types (Action(..), State, DocumentId)
import Stage.Util (addDocHtml)
import Prelude hiding (div)


view :: State -> Html Action
view state = div
  [className "stage"]
  [ -- dangerouslySetInnerHTML addDocHtml state.activeDocument]

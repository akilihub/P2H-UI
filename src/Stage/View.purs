module Stage.View where
import DOM.Event.KeyboardEvent (KeyLocation(..))
import Pux.Html (Html, ul, li, form, input, div, text)
import Pux.Html.Attributes (className, dangerouslySetInnerHTML, name, type_, value)
import Pux.Html.Events (onChange, onSubmit)
import Stage.State (update)
import Stage.Types (Action(..), State)
import Prelude hiding (div)


view :: State -> Html Action
view state = div
  [className "stage"]
  [

  ]

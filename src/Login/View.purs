module Login.View where
import Data.Tuple (Tuple(..))
import Login.Types
import Pux.Html (Html, button, div, form, input, label, p, style, text)
import Pux.Html.Attributes (id_, name, checked, type_, value, className)
import Pux.Html.Events (onBlur, onChange, onClick, onDoubleClick, onKeyDown, onSubmit)
import Prelude hiding (div)


view :: State -> Html Action
view { user: (User user), status: error} = form
  [ name "Signin Form"
  , onSubmit (const ValidateForm)
  ]
  [ input [ value user.username , type_ "text", onChange UserNameChange ] []
  , input [ value user.password , type_ "password", onChange PasswordChange ] []
  , button [ className "submit-form", type_ "submit"][text "Submit"]
  , p [ style [Tuple "display" (if error == "" then "none" else "inherit")]
      , className "form-error"
      ] [text error]
  ]

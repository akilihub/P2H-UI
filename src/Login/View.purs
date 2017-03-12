module Login.View where
import Login.Types(Action(..), State)
import Pux.Html (Html, button,form, input, p, text)
import Pux.Html.Attributes (id_, name, checked, type_, value, className)
import Pux.Html.Events (onBlur, onChange, onClick, onDoubleClick, onKeyDown, onSubmit)
import Prelude hiding (div)


view :: State -> Html Action
view { user: (User user), error: error} = form
  [ name "Signin Form"
  , onSubmit (const ValidateForm)
  ]
  [ input [ value user.username , type_ "text", onChange UserNameChange ] []
  , input [ value user.password , type_ "password", onChange PasswordChange ] []
  , button [ className "submit-form", type_ "submit"][text "Submit"]
  , p [ className "form-error" ] [text error]
  ]

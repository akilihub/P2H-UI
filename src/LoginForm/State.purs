module LoginForm.State where

import LoginForm.Types
import DOM (DOM)
import Pux (EffModel, noEffects)
import Network.HTTP.Affjax (AJAX, post)
import Pux.Html.Events (FormEvent)

init :: State
init = { user: User { password : ""
                    , username : ""
                    }
       , status : ".."
       }

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)

update (AddUsername ev) { status: status, user: (User user) } =
  let newUser = User $ user { username = ev.target.value } in
  noEffects $ {status: "Entered user name", user: newUser}

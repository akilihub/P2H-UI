module LoginForm.State where

import LoginForm.Types
import App.Counter (update)
import CSS (Display(..), input)
import Control.Monad.Aff.Console (CONSOLE, log)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX, post)
import Pux (EffModel, noEffects)
import Pux.Html.Events (FormEvent)

init :: State
init =
  { user:       User  { password : ""
                      , username : ""
                      }
  , status : ".."
  , error : ""
  , session : Session {
                        sessionId : ""
                      , userType  : ""
                      , userId    : ""
                      }
  }

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX, console :: CONSOLE)

update (UserNameChange ev) state@{ status: status, user: (User user) } =
  let newUser = User $ user { username = ev.target.value } in
  noEffects $ {status: "Entering user name", user: newUser, error: ""}

update (PasswordChange ev) state@{status: status, user: (User user)} =
    let newUser = User $ user { password = ev.target.value } in
    noEffects $ state {status: "Entered password", user: newUser, error: ""}

update ValidateForm state@{ user: user } =
  { state: state { status = "form validation"}
  , effects: [ do
       pure $ either DisplayError SignIn (inputValidation user)
    ]
  }

update (DisplayError e) state = noEffects $ state { error: e }

update (SignIn user) state =
    { state: state { status = "input form submission " <> show user <> "..." }
    , effects: [ do
        res <- attempt $ post $ "http://localhost:3001/api/posts/" (encodeJson user)
        let decode r = decodeJson r.response :: Either String Session
        let session = either (Left <<< show) decode res
        pure $ ReceiveUserSession sessionData
      ]
    }

update (ReceiveUserSession session) state =
  noEffects $ state {status: "started new session for user:  " <> show user, session: session}

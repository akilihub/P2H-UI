module Login.State where

import Login.Types
import Prelude
import CSS (Display(..), input)
import Control.Monad.Aff (attempt)
import Control.MonadPlus (guard)
import DOM (DOM)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..), either)
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

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)

update (UserNameChange ev) { user: (User user), session } =
  let newUser = User $ user { username = ev.target.value } in
  noEffects $ { user: newUser, status: "Entering user name", error: "", session: session }

update (PasswordChange ev) { status: status, user: (User user), session} =
    let newUser = User $ user { password = ev.target.value } in
    noEffects $ { user: newUser, status: "Entering password", error: "", session: session }

update ValidateForm state@{ user: user } =
  { state: state { status = "form validation"}
  , effects: [ do
      let validation = inputValidation user
      case validation of
          (Left err)   -> pure $ DisplayError err
          (Right user) -> pure $ SignIn user
    ]
  }

update (DisplayError err) state = noEffects $ state { error: err }

update (SignIn user) state =
    { state: state { status = "input form submission " <> show user <> "..." }
    , effects: [ do
        res <- attempt $ post $ "http://localhost:3001/api/posts/" (encodeJson user)
        let decode r = decodeJson r.response :: Either String Session
        let response = either (Left <<< show) decode res
        case response of
          (Left err)      -> pure $ DisplayError err
          (Right session) -> pure $ ReceiveUserSession session
      ]
    }

update (ReceiveUserSession session) state =
  noEffects $ state {status: "started new session for user:  " <> show state.user, session: session}

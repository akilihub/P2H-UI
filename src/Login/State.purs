module Login.State where
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (message)
import DOM (DOM)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Show (show)
import Login.Types (Action(..), State, User(..), Session(..), inputValidation)
import Network.HTTP.Affjax (AJAX, post)
import Prelude (bind, pure, (<>), ($))
import Pux (EffModel, noEffects)


init :: State
init =
  { user: User { password : ""
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

update ValidateForm state =
  { state: state { status = "form validation"}
  , effects: [ do
      let validation = inputValidation state.user
      case validation of
          (Left err)   -> pure $ DisplayError err
          (Right user) -> pure $ SignIn user
    ]
  }

update (DisplayError err) state = noEffects $
  { user: state.user, status: "Error in form", error: err, session: state.session }

update (SignIn user) state =
    { state: state { status = "input form submission " <> show user <> "..." }
    , effects: [ do
        res <- attempt $ post "http://localhost:3001/api/login/" (encodeJson user)
        case res of
          (Left err)  -> pure $ DisplayError (message err)
          (Right json) -> case decodeJson json.response of
                          Right session -> pure $ ReceiveUserSession session
                          Left err -> pure $ DisplayError err
      ]
    }

update (ReceiveUserSession session) state =
  noEffects $ { user: state.user
              , status: "started new session for user:  " <> show state.user
              , error: state.error
              , session: state.session }

module Login.State where
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (message)
import DOM (DOM)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Login.Types (Action(..), Session(..), State, User(..), inputValidation, authenticateSession)
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
                        sessionId : Nothing
                      , userId    : Nothing
                      }
  }


update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)

update (UserNameChange ev) state@{ user: (User user), session } =
  let newUser = User $ user { username = ev.target.value } in
  noEffects $ state { user = newUser, status = "Entering user name" }

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

update (DisplayError err) state =
  noEffects $ state { status = "Error in form", error = err }

update (SignIn user) state =
    { state: state { status = "input form submission " <> show user <> "..." }
    , effects: [ do
        res <- attempt $ post "http://localhost:3001/api/login" (encodeJson user)
        case res of
          (Left err)  -> pure $ DisplayError (message err)
          (Right r) -> case decodeJson r.response of
                          Right session -> pure $ AuthenticateUser session
                          Left err -> pure $ DisplayError err
      ]
    }

update (AuthenticateUser session) state =
  { state: state { status = "authenticating user login response"}
  , effects: [ do
    pure $ (authenticateSession session)
    ]
  }

update (ReceiveUserSession session) state@{user: User {username, password } } =
  noEffects $ state
    { status = "started new session for: " <> username
    , error = ""
    , session = session }

import Linear.V2 (V2(V2))

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Cmd as Cmd
import qualified Helm.Sub as Sub
import qualified Helm.Mouse as Mouse
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Graphics2D.Text as Text

import Data.Text as T

data Action = Idle | ChangePosition (V2 Double) | PrintText T.Text
data Model = Model (V2 Double) String

initial :: (Model, Cmd SDLEngine Action)
initial = (Model (V2 0 0) "", Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update (Model _ txt) ChangePosition pos) = (Model pos txt, Cmd.none)
update (Model pos oldTxt) (PrintText txt) = (Model pos (oldTxt ++ T.unpack txt), Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
    [ Mouse.moves (\(V2 x y) -> ChangePosition $ V2 (fromIntegral x) (fromIntegral y))
    , Keyboard.typing PrintText
    ]

view :: Model -> Graphics SDLEngine
view (Model pos txt) = Graphics2D $ collage
    [ move pos $ filled (rgb 1 0 0) $ square 10
    , move pos $ text $ Text.height 12 $ Text.color (rgb 1 1 1) $ Text.toText txt
    ]

main :: IO ()
main = do
  engine <- SDL.startup

  run engine defaultConfig GameLifecycle
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }

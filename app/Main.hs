module Main where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Graphics.ImageMagick.MagickWand (withMagickWandGenesis)
import Interpreters.ControlProgram as CP (run)
import Network.Wai (Application)
import Network.Wai.Handler.Warp as Warp (run)
import Scripts.Program (program)

main :: IO ()
main = withMagickWandGenesis (liftIO $ Warp.run 7777 app)

app :: Application
app request respond = CP.run $ program request respond

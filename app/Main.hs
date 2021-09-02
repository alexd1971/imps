module Main where

import Interpreters.ControlProgram as CP (run)
import Network.Wai (Application)
import Network.Wai.Handler.Warp as Warp (run)
import Scripts.Program (program)

main :: IO ()
main = Warp.run 7777 app

app :: Application
app request respond = CP.run $ program request respond

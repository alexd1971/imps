module Interpreters.ControlProgram
  ( run
  ) where

import DSL.ControlProgram
  ( ControlProgram
  , Interpreter(..)
  , Script(IOScriptDef, ImpScriptDef)
  , interpret
  )
import qualified Interpreters.IOLang.Http as IOLang (run)
import qualified Interpreters.ImpLang.ImageMagick as ImpLang (run)

runScript (IOScriptDef src) = IOLang.run src
runScript (ImpScriptDef src) = ImpLang.run src

instance Interpreter IO where
  onEval = runScript

run :: ControlProgram a -> IO a
run = interpret

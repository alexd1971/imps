module Interpreters.ControlProgram
  ( run
  ) where

import DSL.ControlProgram
  ( ControlProgram
  , Interpreter(..)
  , Script(IOScriptDef, ImpScriptDef)
  , interpret
  )
import qualified DSL.IOLang as IOLang (interpret)
import qualified DSL.ImpLang as ImpLang (interpret)
import Interpreters.IOLang.Http
import Interpreters.ImpLang.GD

interpretScript (IOScriptDef src) = IOLang.interpret src
interpretScript (ImpScriptDef src) = ImpLang.interpret src

instance Interpreter IO where
  onEval = interpretScript

run :: ControlProgram a -> IO a
run = interpret

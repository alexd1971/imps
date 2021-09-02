{-# LANGUAGE ExistentialQuantification #-}

module DSL.ControlProgram where

import Control.Monad.Free
import DSL.IOLang (IOScript)
import DSL.ImpLang (ImpScript)

data Script a
  = IOScriptDef (IOScript a)
  | ImpScriptDef (ImpScript a)

ioScript :: IOScript a -> Script a
ioScript = IOScriptDef

impScript :: ImpScript a -> Script a
impScript = ImpScriptDef

data ControlLang a =
  forall b. Eval (Script b) (b -> a)

instance Functor ControlLang where
  fmap f (Eval src g) = Eval src (f . g)

type ControlProgram a = Free ControlLang a

eval :: Script a -> ControlProgram a
eval script = Free (Eval script Pure)

class Monad m =>
      Interpreter m
  where
  onEval :: Script a -> m a

interpret ::
     Monad m
  => Interpreter m =>
       ControlProgram a -> m a
interpret (Pure a) = return a
interpret (Free (Eval script next)) = do
  v <- onEval script
  interpret (next v)

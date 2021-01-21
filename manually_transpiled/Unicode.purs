module Erlang.Unicode where

import Erlang.Type
import Erlang.Builtins as BIF
import Erlang.Exception as EXC

import Prelude


-- TODO: this is actually a placeholder...
erlps__characters_to_binary__1 :: ErlangFun
erlps__characters_to_binary__1 [arg] =
  erlps__characters_to_binary__3 [arg, ErlangAtom "unicode", ErlangAtom "unicode"]
erlps__characters_to_binary__1 [_, _, _] = EXC.badarg unit
erlps__characters_to_binary__1 args =
  EXC.badarity (ErlangFun 1 erlps__characters_to_binary__1) args

erlps__characters_to_binary__2 :: ErlangFun
erlps__characters_to_binary__2 [arg1, arg2] =
  erlps__characters_to_binary__3 [arg1, arg2, ErlangAtom "unicode"]
erlps__characters_to_binary__2 [_, _, _] = EXC.badarg unit
erlps__characters_to_binary__2 args =
  EXC.badarity (ErlangFun 2 erlps__characters_to_binary__2) args

erlps__characters_to_binary__3 :: ErlangFun
erlps__characters_to_binary__3 [str, ErlangAtom inEnc, ErlangAtom outEnc]
  = BIF.erlang__iolist_to_binary__1 [str]
erlps__characters_to_binary__3 [_, _, _] = EXC.badarg unit
erlps__characters_to_binary__3 args =
  EXC.badarity (ErlangFun 3 erlps__characters_to_binary__3) args

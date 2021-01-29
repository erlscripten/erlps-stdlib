module Os where

import Erlang.Type
import Erlang.Exception as EXC

erlps__type__0 :: ErlangFun
erlps__type__0 [] = ErlangTuple [ErlangAtom "unix", ErlangAtom "linux"]
erlps__type__0 args = EXC.badarity (ErlangFun 0 erlps__type__0) args

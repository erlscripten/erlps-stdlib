module Crypto where

import Data.BigInt as DBI
import Erlang.Exception as EXC
import Erlang.Type
import Erlang.Binary as BIN
import Erlang.Builtins as BIF
import Erlang.Helpers as H
import Prelude
import Node.Buffer

foreign import sha256Impl :: Buffer -> Buffer

erlps__hash__2 :: ErlangFun
erlps__hash__2 [ErlangAtom "sha256", ErlangBinary buf]
  = ErlangBinary $ sha256Impl buf
erlps__hash__2 [alg, iolist] =
  erlps__hash__2 [alg, BIF.erlang__iolist_to_binary__1 [iolist]]
erlps__hash__2 _ = EXC.badarg unit

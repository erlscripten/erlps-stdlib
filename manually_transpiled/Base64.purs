module Base64 where

import Data.Binary.Base64
import Data.BigInt as DBI
import Erlang.Exception as EXC
import Erlang.Type
import Erlang.Builtins
import Erlang.Binary as BIN
import Erlang.Helpers as H
import Data.Maybe as DM
import Data.Either as DE
import Prelude
import Partial.Unsafe
import Node.Buffer
import Unsafe.Coerce
import Data.ArrayBuffer.Types

foreign import u8ToBuf :: Uint8Array -> Buffer
foreign import bufToU8 :: Buffer -> Uint8Array

erlps__decode__1 :: ErlangFun
erlps__decode__1 [ErlangBinary buf] =
  erlps__decode__1 [BIN.to_erlang_list buf]
erlps__decode__1 [el] | DM.Just str <- H.erlangListToString el
  = case decode str of
      DE.Left _ -> EXC.badarg unit
      DE.Right u -> ErlangBinary $ u8ToBuf u
erlps__decode__1 [_] = EXC.badarg unit
erlps__decode__1 args = EXC.badarity (ErlangFun 1 erlps__decode__1) args

erlps__decode_to_string__1 :: ErlangFun
erlps__decode_to_string__1 [a] =
  erlang__binary_to_list__1 [erlps__decode__1 [a]]
erlps__decode_to_string__1 args = EXC.badarity (ErlangFun 1 erlps__decode_to_string__1) args

erlps__encode__1 :: ErlangFun
erlps__encode__1 [a] =
  erlang__list_to_binary__1 [erlps__encode_to_string__1 [a]]
erlps__encode__1 args = EXC.badarity (ErlangFun 1 erlps__encode__1) args

erlps__encode_to_string__1 :: ErlangFun
erlps__encode_to_string__1 [ErlangBinary buf] =
  H.make_string $ encode $ bufToU8 buf
erlps__encode_to_string__1 [el] =
  erlps__encode_to_string__1 [erlang__list_to_binary__1 [el]]
erlps__encode_to_string__1 args = EXC.badarity (ErlangFun 1 erlps__encode_to_string__1) args

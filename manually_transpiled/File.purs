module File where

import Erlang.Type
import Node.Buffer
import Prelude

import Data.Maybe as DM
import Erlang.Binary as BIN
import Erlang.Builtins as BIF
import Erlang.Exception as EXC
import Erlang.Helpers as H

foreign import readFileImpl :: (Buffer -> ErlangTerm) -> ErlangTerm -> String -> ErlangTerm

erlps__read_file__1 [ErlangBinary buf] =
  erlps__read_file__1 [BIN.toErlangList buf]
erlps__read_file__1 [estr] | DM.Just str <- fromErl estr =
  readFileImpl (\buf -> ErlangTuple [ErlangAtom "ok", ErlangBinary buf])
  (ErlangTuple [ErlangAtom "error", ErlangAtom "enoent"]) str
erlps__read_file__1 [_] = EXC.badarg unit
erlps__read_file__1 args = EXC.badarity (ErlangFun 1 erlps__read_file__1) args

erlps__native_name_encoding__0 :: ErlangFun
erlps__native_name_encoding__0 [] = ErlangAtom "utf8"
erlps__native_name_encoding__0 args = EXC.badarity (ErlangFun 0 erlps__native_name_encoding__0) args

erlps__get_cwd__0 :: ErlangFun
erlps__get_cwd__0 [] = ErlangTuple [ErlangAtom "ok", toErl "."]  -- XD
erlps__get_cwd__0 args = EXC.badarity (ErlangFun 0 erlps__get_cwd__0) args

erlps__format_error__1 :: ErlangFun
erlps__format_error__1 [ErlangTuple [_, ErlangAtom "file", ErlangAtom "undefined_script"]] =
  toErl "no value returned from script"
erlps__format_error__1 [ErlangTuple [line, ErlangAtom "file", ErlangTuple [clas, reason, stack]]] =
  toErl $ show line <> ": evaluation failed with reason " <> show clas <> ":" <> show reason <> " and stacktrace " <> show stack
erlps__format_error__1 [ErlangTuple [line, ErlangAtom "file", ErlangTuple [reason, stack]]] =
  toErl $ show line <> ": evaluation failed with reason " <> show reason <> " and stacktrace " <> show stack
erlps__format_error__1 [ErlangTuple [line, ErlangAtom modul, reason]] =
  BIF.erlang__append__2 [toErl $ show line <> ": ", BIF.do_remote_fun_call modul "erlps__format_error__1" [reason]]
erlps__format_error__1 [ErlangAtom "badarg"] =
  toErl "bad argument"
erlps__format_error__1 [ErlangAtom "system_limit"] =
  toErl "a system limit was hit, probably not enough ports"
erlps__format_error__1 [ErlangAtom "terminated"] =
  toErl "the file server process is terminated"
erlps__format_error__1 [e] =
  BIF.do_remote_fun_call "Erl.Posix.Msg" "erlps__message__1" [e]
erlps__format_error__1 args = EXC.badarity (ErlangFun 0 erlps__format_error__1) args

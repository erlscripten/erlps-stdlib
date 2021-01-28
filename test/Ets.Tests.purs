module Ets.Tests(erlps__test_set__0, erlps__test_bag__0) where
{-
This file has been autogenerated
DO NOT EDIT - Your changes WILL be overwritten
Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote
Erlscripten v0.2.0
-}

import Prelude
import Data.BigInt as DBI
import Data.Array as DA
import Data.Maybe as DM
import Data.Map as Map
import Data.Tuple as DT
import Erlang.Builtins as BIF
import Erlang.Binary as BIN
import Erlang.Helpers
import Erlang.Exception as EXC
import Erlang.Type
import Partial.Unsafe (unsafePartial)


erlps__test_set__0 :: ErlangFun
erlps__test_set__0 [] =
  let   
    t_4 =
      BIF.do_remote_fun_call "Ets" "erlps__new__2"
        [ErlangAtom "t", ErlangCons (ErlangAtom "set") ErlangEmptyList]
  in let tup_el_7 = toErl 1
  in let arg_6 = ErlangTuple [tup_el_7, ErlangAtom "a"]
  in let
    matchExpr_9 =
      BIF.do_remote_fun_call "Ets" "erlps__insert__2" [t_4, arg_6]
  in
    case matchExpr_9 of
      (ErlangAtom "true") ->
        let    tup_el_12 = toErl 1
        in let arg_11 = ErlangTuple [tup_el_12, ErlangAtom "b"]
        in let
          matchExpr_14 =
            BIF.do_remote_fun_call "Ets" "erlps__insert__2" [t_4, arg_11]
        in
          case matchExpr_14 of
            (ErlangAtom "true") ->
              let    tup_el_17 = toErl 1
              in let arg_16 = ErlangTuple [tup_el_17, ErlangAtom "b"]
              in let
                matchExpr_19 =
                  BIF.do_remote_fun_call "Ets" "erlps__insert__2" [t_4, arg_16]
              in
                case matchExpr_19 of
                  (ErlangAtom "true") ->
                    let    tup_el_22 = ErlangFloat 1.00000000000000000000e+0
                    in let arg_21 = ErlangTuple [tup_el_22, ErlangAtom "c"]
                    in let
                      matchExpr_24 =
                        BIF.do_remote_fun_call "Ets" "erlps__insert__2"
                          [t_4, arg_21]
                    in
                      case matchExpr_24 of
                        (ErlangAtom "true") ->
                          let    arg_26 = toErl 1
                          in let
                            matchExpr_28 =
                              BIF.do_remote_fun_call "Ets" "erlps__lookup__2"
                                [t_4, arg_26]
                          in
                            case matchExpr_28 of
                              (ErlangCons (ErlangTuple [(ErlangInt num_27),
                                                        (ErlangAtom "b")]) (ErlangEmptyList)) | ((ErlangInt
                                                                                                    num_27) ==
                                                                                                   (toErl
                                                                                                      1)) ->
                                let
                                  matchExpr_31 =
                                    BIF.do_remote_fun_call "Ets"
                                      "erlps__tab2list__1" [t_4]
                                in
                                  case matchExpr_31 of
                                    (ErlangCons (ErlangTuple [(ErlangFloat 1.00000000000000000000e+0),
                                                              (ErlangAtom "c")]) (ErlangCons (ErlangTuple [(ErlangInt num_30),
                                                                                                           (ErlangAtom "b")]) (ErlangEmptyList))) | ((ErlangInt
                                                                                                                                                        num_30) ==
                                                                                                                                                       (toErl
                                                                                                                                                          1)) ->
                                      let
                                        matchExpr_33 =
                                          BIF.do_remote_fun_call "Ets"
                                            "erlps__delete__1" [t_4]
                                      in
                                        case matchExpr_33 of
                                          (ErlangAtom "true") -> ErlangAtom "ok"
                                          _ -> EXC.badmatch matchExpr_33
                                    _ -> EXC.badmatch matchExpr_31
                              _ -> EXC.badmatch matchExpr_28
                        _ -> EXC.badmatch matchExpr_24
                  _ -> EXC.badmatch matchExpr_19
            _ -> EXC.badmatch matchExpr_14
      _ -> EXC.badmatch matchExpr_9
erlps__test_set__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__test_bag__0 :: ErlangFun
erlps__test_bag__0 [] =
  let   
    t_4 =
      BIF.do_remote_fun_call "Ets" "erlps__new__2"
        [ErlangAtom "t", ErlangCons (ErlangAtom "bag") ErlangEmptyList]
  in let tup_el_7 = toErl 1
  in let arg_6 = ErlangTuple [tup_el_7, ErlangAtom "a"]
  in let
    matchExpr_9 =
      BIF.do_remote_fun_call "Ets" "erlps__insert__2" [t_4, arg_6]
  in
    case matchExpr_9 of
      (ErlangAtom "true") ->
        let    tup_el_12 = toErl 1
        in let arg_11 = ErlangTuple [tup_el_12, ErlangAtom "b"]
        in let
          matchExpr_14 =
            BIF.do_remote_fun_call "Ets" "erlps__insert__2" [t_4, arg_11]
        in
          case matchExpr_14 of
            (ErlangAtom "true") ->
              let    tup_el_17 = toErl 1
              in let arg_16 = ErlangTuple [tup_el_17, ErlangAtom "b"]
              in let
                matchExpr_19 =
                  BIF.do_remote_fun_call "Ets" "erlps__insert__2" [t_4, arg_16]
              in
                case matchExpr_19 of
                  (ErlangAtom "true") ->
                    let    tup_el_22 = ErlangFloat 1.00000000000000000000e+0
                    in let arg_21 = ErlangTuple [tup_el_22, ErlangAtom "c"]
                    in let
                      matchExpr_24 =
                        BIF.do_remote_fun_call "Ets" "erlps__insert__2"
                          [t_4, arg_21]
                    in
                      case matchExpr_24 of
                        (ErlangAtom "true") ->
                          let    arg_26 = toErl 1
                          in let
                            matchExpr_29 =
                              BIF.do_remote_fun_call "Ets" "erlps__lookup__2"
                                [t_4, arg_26]
                          in
                            case matchExpr_29 of
                              (ErlangCons (ErlangTuple [(ErlangInt num_27),
                                                        (ErlangAtom "a")]) (ErlangCons (ErlangTuple [(ErlangInt num_28),
                                                                                                     (ErlangAtom "b")]) (ErlangEmptyList))) | ((ErlangInt
                                                                                                                                                  num_27) ==
                                                                                                                                                 (toErl
                                                                                                                                                    1))
                                                                                                                                            , ((ErlangInt
                                                                                                                                                  num_28) ==
                                                                                                                                                 (toErl
                                                                                                                                                    1)) ->
                                let
                                  matchExpr_33 =
                                    BIF.do_remote_fun_call "Ets"
                                      "erlps__tab2list__1" [t_4]
                                in
                                  case matchExpr_33 of
                                    (ErlangCons (ErlangTuple [(ErlangFloat 1.00000000000000000000e+0),
                                                              (ErlangAtom "c")]) (ErlangCons (ErlangTuple [(ErlangInt num_31),
                                                                                                           (ErlangAtom "b")]) (ErlangCons (ErlangTuple [(ErlangInt num_32),
                                                                                                                                                        (ErlangAtom "a")]) (ErlangEmptyList)))) | ((ErlangInt
                                                                                                                                                                                                      num_31) ==
                                                                                                                                                                                                     (toErl
                                                                                                                                                                                                        1))
                                                                                                                                                                                                , ((ErlangInt
                                                                                                                                                                                                      num_32) ==
                                                                                                                                                                                                     (toErl
                                                                                                                                                                                                        1)) ->
                                      let
                                        matchExpr_35 =
                                          BIF.do_remote_fun_call "Ets"
                                            "erlps__delete__1" [t_4]
                                      in
                                        case matchExpr_35 of
                                          (ErlangAtom "true") -> ErlangAtom "ok"
                                          _ -> EXC.badmatch matchExpr_35
                                    _ -> EXC.badmatch matchExpr_33
                              _ -> EXC.badmatch matchExpr_29
                        _ -> EXC.badmatch matchExpr_24
                  _ -> EXC.badmatch matchExpr_19
            _ -> EXC.badmatch matchExpr_14
      _ -> EXC.badmatch matchExpr_9
erlps__test_bag__0 args =
  EXC.badarity (ErlangFun 0 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args
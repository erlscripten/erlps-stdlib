module Proplists(erlps__property__1, erlps__property__2,
                 erlps__unfold__1, erlps__compact__1, erlps__lookup__2,
                 erlps__lookup_all__2, erlps__is_defined__2,
                 erlps__get_value__2, erlps__get_value__3,
                 erlps__get_all_values__2, erlps__append_values__2,
                 erlps__get_bool__2, erlps__get_keys__1, erlps__delete__2,
                 erlps__substitute_aliases__2, erlps__substitute_negations__2,
                 erlps__expand__2, erlps__normalize__2, erlps__split__2) where
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


erlps__property__1 :: ErlangFun
erlps__property__1 [(ErlangTuple [key_0, (ErlangAtom "true")])]
  | isEAtom key_0 =
  key_0
erlps__property__1 [property_0] = property_0
erlps__property__1 [arg_1] = EXC.function_clause unit
erlps__property__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__property__2 :: ErlangFun
erlps__property__2 [key_0, (ErlangAtom "true")] | isEAtom key_0 =
  key_0
erlps__property__2 [key_0, value_1] =
  ErlangTuple [key_0, value_1]
erlps__property__2 [arg_4, arg_5] = EXC.function_clause unit
erlps__property__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__unfold__1 :: ErlangFun
erlps__unfold__1 [(ErlangCons p_0 ps_1)] =
  case ErlangAtom "true" of
    _ | isEAtom p_0 ->
      let    head_2 = ErlangTuple [p_0, ErlangAtom "true"]
      in let tail_5 = erlps__unfold__1 [ps_1]
      in ErlangCons head_2 tail_5
    _ ->
      let tail_8 = erlps__unfold__1 [ps_1]
      in ErlangCons p_0 tail_8
erlps__unfold__1 [(ErlangEmptyList)] = ErlangEmptyList
erlps__unfold__1 [arg_0] = EXC.function_clause unit
erlps__unfold__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__compact__1 :: ErlangFun
erlps__compact__1 [listin_0] =
  flmap
    (\ lc_3 ->
       let lcRet_4 = erlps__property__1 [lc_3]
       in ErlangCons lcRet_4 ErlangEmptyList)
    listin_0
erlps__compact__1 [arg_6] = EXC.function_clause unit
erlps__compact__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__lookup__2 :: ErlangFun
erlps__lookup__2 [key_0, (ErlangCons p_1 ps_2)] =
  case ErlangAtom "true" of
    _ | ((isEAtom p_1) && ((==) p_1 key_0)) ->
      ErlangTuple [key_0, ErlangAtom "true"]
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_6 = BIF.erlang__tuple_size__1 [p_1]
                 in let rop_8 = toErl 1
                 in let lop_5 = BIF.erlang__op_greaterEq [lop_6, rop_8]
                 in
                   case lop_5 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_10 = toErl 1
                       in let lop_9 = BIF.erlang__element__2 [arg_10, p_1]
                       in BIF.erlang__op_exactEq [lop_9, key_0]
                     _ -> EXC.badarg1 lop_5))) ->
      p_1
    _ -> erlps__lookup__2 [key_0, ps_2]
erlps__lookup__2 [_key_0, (ErlangEmptyList)] = ErlangAtom "none"
erlps__lookup__2 [arg_1, arg_2] = EXC.function_clause unit
erlps__lookup__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__lookup_all__2 :: ErlangFun
erlps__lookup_all__2 [key_0, (ErlangCons p_1 ps_2)] =
  case ErlangAtom "true" of
    _ | ((isEAtom p_1) && ((==) p_1 key_0)) ->
      let    head_3 = ErlangTuple [key_0, ErlangAtom "true"]
      in let tail_6 = erlps__lookup_all__2 [key_0, ps_2]
      in ErlangCons head_3 tail_6
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_10 = BIF.erlang__tuple_size__1 [p_1]
                 in let rop_12 = toErl 1
                 in let lop_9 = BIF.erlang__op_greaterEq [lop_10, rop_12]
                 in
                   case lop_9 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_14 = toErl 1
                       in let lop_13 = BIF.erlang__element__2 [arg_14, p_1]
                       in BIF.erlang__op_exactEq [lop_13, key_0]
                     _ -> EXC.badarg1 lop_9))) ->
      let tail_18 = erlps__lookup_all__2 [key_0, ps_2]
      in ErlangCons p_1 tail_18
    _ -> erlps__lookup_all__2 [key_0, ps_2]
erlps__lookup_all__2 [_key_0, (ErlangEmptyList)] =
  ErlangEmptyList
erlps__lookup_all__2 [arg_1, arg_2] = EXC.function_clause unit
erlps__lookup_all__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__is_defined__2 :: ErlangFun
erlps__is_defined__2 [key_0, (ErlangCons p_1 ps_2)] =
  case ErlangAtom "true" of
    _ | ((isEAtom p_1) && ((==) p_1 key_0)) -> ErlangAtom "true"
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_4 = BIF.erlang__tuple_size__1 [p_1]
                 in let rop_6 = toErl 1
                 in let lop_3 = BIF.erlang__op_greaterEq [lop_4, rop_6]
                 in
                   case lop_3 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_8 = toErl 1
                       in let lop_7 = BIF.erlang__element__2 [arg_8, p_1]
                       in BIF.erlang__op_exactEq [lop_7, key_0]
                     _ -> EXC.badarg1 lop_3))) ->
      ErlangAtom "true"
    _ -> erlps__is_defined__2 [key_0, ps_2]
erlps__is_defined__2 [_key_0, (ErlangEmptyList)] =
  ErlangAtom "false"
erlps__is_defined__2 [arg_1, arg_2] = EXC.function_clause unit
erlps__is_defined__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__get_value__2 :: ErlangFun
erlps__get_value__2 [key_0, list_1] =
  erlps__get_value__3 [key_0, list_1, ErlangAtom "undefined"]
erlps__get_value__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__get_value__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__get_value__3 :: ErlangFun
erlps__get_value__3 [key_0, (ErlangCons p_1 ps_2), default_3] =
  case ErlangAtom "true" of
    _ | ((isEAtom p_1) && ((==) p_1 key_0)) -> ErlangAtom "true"
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_5 = BIF.erlang__tuple_size__1 [p_1]
                 in let rop_7 = toErl 1
                 in let lop_4 = BIF.erlang__op_greaterEq [lop_5, rop_7]
                 in
                   case lop_4 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_9 = toErl 1
                       in let lop_8 = BIF.erlang__element__2 [arg_9, p_1]
                       in BIF.erlang__op_exactEq [lop_8, key_0]
                     _ -> EXC.badarg1 lop_4))) ->
      case p_1 of
        (ErlangTuple [_, value_13]) -> value_13
        _ -> default_3
    _ -> erlps__get_value__3 [key_0, ps_2, default_3]
erlps__get_value__3 [_key_0, (ErlangEmptyList), default_1] =
  default_1
erlps__get_value__3 [arg_2, arg_3, arg_4] =
  EXC.function_clause unit
erlps__get_value__3 args =
  EXC.badarity (ErlangFun 3 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__get_all_values__2 :: ErlangFun
erlps__get_all_values__2 [key_0, (ErlangCons p_1 ps_2)] =
  case ErlangAtom "true" of
    _ | ((isEAtom p_1) && ((==) p_1 key_0)) ->
      let tail_4 = erlps__get_all_values__2 [key_0, ps_2]
      in ErlangCons (ErlangAtom "true") tail_4
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_8 = BIF.erlang__tuple_size__1 [p_1]
                 in let rop_10 = toErl 1
                 in let lop_7 = BIF.erlang__op_greaterEq [lop_8, rop_10]
                 in
                   case lop_7 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_12 = toErl 1
                       in let lop_11 = BIF.erlang__element__2 [arg_12, p_1]
                       in BIF.erlang__op_exactEq [lop_11, key_0]
                     _ -> EXC.badarg1 lop_7))) ->
      case p_1 of
        (ErlangTuple [_, value_16]) ->
          let tail_18 = erlps__get_all_values__2 [key_0, ps_2]
          in ErlangCons value_16 tail_18
        _ -> erlps__get_all_values__2 [key_0, ps_2]
    _ -> erlps__get_all_values__2 [key_0, ps_2]
erlps__get_all_values__2 [_key_0, (ErlangEmptyList)] =
  ErlangEmptyList
erlps__get_all_values__2 [arg_1, arg_2] =
  EXC.function_clause unit
erlps__get_all_values__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__append_values__2 :: ErlangFun
erlps__append_values__2 [key_0, (ErlangCons p_1 ps_2)] =
  case ErlangAtom "true" of
    _ | ((isEAtom p_1) && ((==) p_1 key_0)) ->
      let tail_4 = erlps__append_values__2 [key_0, ps_2]
      in ErlangCons (ErlangAtom "true") tail_4
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_8 = BIF.erlang__tuple_size__1 [p_1]
                 in let rop_10 = toErl 1
                 in let lop_7 = BIF.erlang__op_greaterEq [lop_8, rop_10]
                 in
                   case lop_7 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_12 = toErl 1
                       in let lop_11 = BIF.erlang__element__2 [arg_12, p_1]
                       in BIF.erlang__op_exactEq [lop_11, key_0]
                     _ -> EXC.badarg1 lop_7))) ->
      case p_1 of
        (ErlangTuple [_, value_16]) | isEList value_16 ->
          let rop_18 = erlps__append_values__2 [key_0, ps_2]
          in BIF.erlang__op_append [value_16, rop_18]
        (ErlangTuple [_, value_21]) ->
          let tail_23 = erlps__append_values__2 [key_0, ps_2]
          in ErlangCons value_21 tail_23
        _ -> erlps__append_values__2 [key_0, ps_2]
    _ -> erlps__append_values__2 [key_0, ps_2]
erlps__append_values__2 [_key_0, (ErlangEmptyList)] =
  ErlangEmptyList
erlps__append_values__2 [arg_1, arg_2] = EXC.function_clause unit
erlps__append_values__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__get_bool__2 :: ErlangFun
erlps__get_bool__2 [key_0, (ErlangCons p_1 ps_2)] =
  case ErlangAtom "true" of
    _ | ((isEAtom p_1) && ((==) p_1 key_0)) -> ErlangAtom "true"
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_4 = BIF.erlang__tuple_size__1 [p_1]
                 in let rop_6 = toErl 1
                 in let lop_3 = BIF.erlang__op_greaterEq [lop_4, rop_6]
                 in
                   case lop_3 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_8 = toErl 1
                       in let lop_7 = BIF.erlang__element__2 [arg_8, p_1]
                       in BIF.erlang__op_exactEq [lop_7, key_0]
                     _ -> EXC.badarg1 lop_3))) ->
      case p_1 of
        (ErlangTuple [_, (ErlangAtom "true")]) -> ErlangAtom "true"
        _ -> ErlangAtom "false"
    _ -> erlps__get_bool__2 [key_0, ps_2]
erlps__get_bool__2 [_key_0, (ErlangEmptyList)] =
  ErlangAtom "false"
erlps__get_bool__2 [arg_1, arg_2] = EXC.function_clause unit
erlps__get_bool__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__get_keys__1 :: ErlangFun
erlps__get_keys__1 [ps_0] =
  let    arg_3 = BIF.do_remote_fun_call "Sets" "erlps__new__0" []
  in let arg_1 = erlps__get_keys__2 [ps_0, arg_3]
  in BIF.do_remote_fun_call "Sets" "erlps__to_list__1" [arg_1]
erlps__get_keys__1 [arg_4] = EXC.function_clause unit
erlps__get_keys__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__get_keys__2 :: ErlangFun
erlps__get_keys__2 [(ErlangCons p_0 ps_1), keys_2] =
  case ErlangAtom "true" of
    _ | isEAtom p_0 ->
      let
        arg_4 =
          BIF.do_remote_fun_call "Sets" "erlps__add_element__2"
            [p_0, keys_2]
      in erlps__get_keys__2 [ps_1, arg_4]
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_7 = BIF.erlang__tuple_size__1 [p_0]
                 in let rop_9 = toErl 1
                 in BIF.erlang__op_greaterEq [lop_7, rop_9]))) ->
      let    arg_13 = toErl 1
      in let arg_12 = BIF.erlang__element__2 [arg_13, p_0]
      in let
        arg_11 =
          BIF.do_remote_fun_call "Sets" "erlps__add_element__2"
            [arg_12, keys_2]
      in erlps__get_keys__2 [ps_1, arg_11]
    _ -> erlps__get_keys__2 [ps_1, keys_2]
erlps__get_keys__2 [(ErlangEmptyList), keys_0] = keys_0
erlps__get_keys__2 [arg_1, arg_2] = EXC.function_clause unit
erlps__get_keys__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__delete__2 :: ErlangFun
erlps__delete__2 [key_0, (ErlangCons p_1 ps_2)] =
  case ErlangAtom "true" of
    _ | ((isEAtom p_1) && ((==) p_1 key_0)) ->
      erlps__delete__2 [key_0, ps_2]
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_6 = BIF.erlang__tuple_size__1 [p_1]
                 in let rop_8 = toErl 1
                 in let lop_5 = BIF.erlang__op_greaterEq [lop_6, rop_8]
                 in
                   case lop_5 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_10 = toErl 1
                       in let lop_9 = BIF.erlang__element__2 [arg_10, p_1]
                       in BIF.erlang__op_exactEq [lop_9, key_0]
                     _ -> EXC.badarg1 lop_5))) ->
      erlps__delete__2 [key_0, ps_2]
    _ ->
      let tail_16 = erlps__delete__2 [key_0, ps_2]
      in ErlangCons p_1 tail_16
erlps__delete__2 [_, (ErlangEmptyList)] = ErlangEmptyList
erlps__delete__2 [arg_0, arg_1] = EXC.function_clause unit
erlps__delete__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__substitute_aliases__2 :: ErlangFun
erlps__substitute_aliases__2 [as_0, props_1] =
  flmap
    (\ lc_4 ->
       let lcRet_5 = erlps__substitute_aliases_1__2 [as_0, lc_4]
       in ErlangCons lcRet_5 ErlangEmptyList)
    props_1
erlps__substitute_aliases__2 [arg_8, arg_9] =
  EXC.function_clause unit
erlps__substitute_aliases__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__substitute_aliases_1__2 :: ErlangFun
erlps__substitute_aliases_1__2 [(ErlangCons (ErlangTuple [key_0,
                                                          key1_1]) as_2),
                                p_3]
  =
  case ErlangAtom "true" of
    _ | ((isEAtom p_3) && ((==) p_3 key_0)) ->
      erlps__property__2 [key1_1, ErlangAtom "true"]
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_7 = BIF.erlang__tuple_size__1 [p_3]
                 in let rop_9 = toErl 1
                 in let lop_6 = BIF.erlang__op_greaterEq [lop_7, rop_9]
                 in
                   case lop_6 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_11 = toErl 1
                       in let lop_10 = BIF.erlang__element__2 [arg_11, p_3]
                       in BIF.erlang__op_exactEq [lop_10, key_0]
                     _ -> EXC.badarg1 lop_6))) ->
      let    arg_15 = toErl 1
      in let arg_14 = BIF.erlang__setelement__3 [arg_15, p_3, key1_1]
      in erlps__property__1 [arg_14]
    _ -> erlps__substitute_aliases_1__2 [as_2, p_3]
erlps__substitute_aliases_1__2 [(ErlangEmptyList), p_0] = p_0
erlps__substitute_aliases_1__2 [arg_1, arg_2] =
  EXC.function_clause unit
erlps__substitute_aliases_1__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__substitute_negations__2 :: ErlangFun
erlps__substitute_negations__2 [as_0, props_1] =
  flmap
    (\ lc_4 ->
       let lcRet_5 = erlps__substitute_negations_1__2 [as_0, lc_4]
       in ErlangCons lcRet_5 ErlangEmptyList)
    props_1
erlps__substitute_negations__2 [arg_8, arg_9] =
  EXC.function_clause unit
erlps__substitute_negations__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__substitute_negations_1__2 :: ErlangFun
erlps__substitute_negations_1__2 [(ErlangCons (ErlangTuple [key_0,
                                                            key1_1]) as_2),
                                  p_3]
  =
  case ErlangAtom "true" of
    _ | ((isEAtom p_3) && ((==) p_3 key_0)) ->
      erlps__property__2 [key1_1, ErlangAtom "false"]
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_7 = BIF.erlang__tuple_size__1 [p_3]
                 in let rop_9 = toErl 1
                 in let lop_6 = BIF.erlang__op_greaterEq [lop_7, rop_9]
                 in
                   case lop_6 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_11 = toErl 1
                       in let lop_10 = BIF.erlang__element__2 [arg_11, p_3]
                       in BIF.erlang__op_exactEq [lop_10, key_0]
                     _ -> EXC.badarg1 lop_6))) ->
      case p_3 of
        (ErlangTuple [_, (ErlangAtom "true")]) ->
          erlps__property__2 [key1_1, ErlangAtom "false"]
        (ErlangTuple [_, (ErlangAtom "false")]) ->
          erlps__property__2 [key1_1, ErlangAtom "true"]
        _ -> erlps__property__2 [key1_1, ErlangAtom "true"]
    _ -> erlps__substitute_negations_1__2 [as_2, p_3]
erlps__substitute_negations_1__2 [(ErlangEmptyList), p_0] = p_0
erlps__substitute_negations_1__2 [arg_1, arg_2] =
  EXC.function_clause unit
erlps__substitute_negations_1__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__expand__2 :: ErlangFun
erlps__expand__2 [es_0, ps_1] | isEList ps_1 =
  let   
    es1_10 =
      flmap
        (\ lc_5 ->
           case lc_5 of
             (ErlangTuple [p_3, v_4]) ->
               let    tup_el_7 = erlps__property__1 [p_3]
               in let lcRet_6 = ErlangTuple [tup_el_7, v_4]
               in ErlangCons lcRet_6 ErlangEmptyList
             _ -> ErlangEmptyList)
        es_0
  in let arg_12 = erlps__key_uniq__1 [es1_10]
  in let arg_11 = erlps__expand_0__2 [arg_12, ps_1]
  in erlps__flatten__1 [arg_11]
erlps__expand__2 [arg_15, arg_16] = EXC.function_clause unit
erlps__expand__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__expand_0__2 :: ErlangFun
erlps__expand_0__2 [(ErlangCons (ErlangTuple [p_0, l_1]) es_2),
                    ps_3]
  =
  let arg_5 = erlps__expand_1__3 [p_0, l_1, ps_3]
  in erlps__expand_0__2 [es_2, arg_5]
erlps__expand_0__2 [(ErlangEmptyList), ps_0] = ps_0
erlps__expand_0__2 [arg_1, arg_2] = EXC.function_clause unit
erlps__expand_0__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__expand_1__3 :: ErlangFun
erlps__expand_1__3 [p_0, l_1, ps_2] =
  case ErlangAtom "true" of
    _ | isEAtom p_0 -> erlps__expand_2__4 [p_0, p_0, l_1, ps_2]
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_7 = BIF.erlang__tuple_size__1 [p_0]
                 in let rop_9 = toErl 1
                 in BIF.erlang__op_greaterEq [lop_7, rop_9]))) ->
      let    arg_11 = toErl 1
      in let arg_10 = BIF.erlang__element__2 [arg_11, p_0]
      in erlps__expand_2__4 [arg_10, p_0, l_1, ps_2]
    _ -> ps_2
erlps__expand_1__3 [arg_16, arg_17, arg_18] =
  EXC.function_clause unit
erlps__expand_1__3 args =
  EXC.badarity (ErlangFun 3 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__expand_2__4 :: ErlangFun
erlps__expand_2__4 [key_0, p1_1, l_2, (ErlangCons p_3 ps_4)] =
  case ErlangAtom "true" of
    _ | ((isEAtom p_3) && ((==) p_3 key_0)) ->
      erlps__expand_3__5 [key_0, p1_1, p_3, l_2, ps_4]
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_11 = BIF.erlang__tuple_size__1 [p_3]
                 in let rop_13 = toErl 1
                 in let lop_10 = BIF.erlang__op_greaterEq [lop_11, rop_13]
                 in
                   case lop_10 of
                     (ErlangAtom "false") -> ErlangAtom "false"
                     (ErlangAtom "true") ->
                       let    arg_15 = toErl 1
                       in let lop_14 = BIF.erlang__element__2 [arg_15, p_3]
                       in BIF.erlang__op_exactEq [lop_14, key_0]
                     _ -> EXC.badarg1 lop_10))) ->
      let arg_20 = erlps__property__1 [p_3]
      in erlps__expand_3__5 [key_0, p1_1, arg_20, l_2, ps_4]
    _ ->
      let tail_25 = erlps__expand_2__4 [key_0, p1_1, l_2, ps_4]
      in ErlangCons p_3 tail_25
erlps__expand_2__4 [_, _, _, (ErlangEmptyList)] = ErlangEmptyList
erlps__expand_2__4 [arg_0, arg_1, arg_2, arg_3] =
  EXC.function_clause unit
erlps__expand_2__4 args =
  EXC.badarity (ErlangFun 4 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__expand_3__5 :: ErlangFun
erlps__expand_3__5 [key_0, p1_1, p_2, l_3, ps_4] =
  case ErlangAtom "true" of
    _ | (==) p1_1 p_2 ->
      let tail_6 = erlps__delete__2 [key_0, ps_4]
      in ErlangCons l_3 tail_6
    _ -> ErlangCons p_2 ps_4
erlps__expand_3__5 [arg_11, arg_12, arg_13, arg_14, arg_15] =
  EXC.function_clause unit
erlps__expand_3__5 args =
  EXC.badarity (ErlangFun 5 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__key_uniq__1 :: ErlangFun
erlps__key_uniq__1 [(ErlangCons (ErlangTuple [k_0, v_1]) ps_2)] =
  let    head_3 = ErlangTuple [k_0, v_1]
  in let tail_6 = erlps__key_uniq_1__2 [k_0, ps_2]
  in ErlangCons head_3 tail_6
erlps__key_uniq__1 [(ErlangEmptyList)] = ErlangEmptyList
erlps__key_uniq__1 [arg_0] = EXC.function_clause unit
erlps__key_uniq__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__key_uniq_1__2 :: ErlangFun
erlps__key_uniq_1__2 [k_0,
                      (ErlangCons (ErlangTuple [k1_1, v_2]) ps_3)]
  =
  case ErlangAtom "true" of
    _ | (==) k_0 k1_1 -> erlps__key_uniq_1__2 [k_0, ps_3]
    _ ->
      let    head_6 = ErlangTuple [k1_1, v_2]
      in let tail_9 = erlps__key_uniq_1__2 [k1_1, ps_3]
      in ErlangCons head_6 tail_9
erlps__key_uniq_1__2 [_, (ErlangEmptyList)] = ErlangEmptyList
erlps__key_uniq_1__2 [arg_0, arg_1] = EXC.function_clause unit
erlps__key_uniq_1__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__flatten__1 :: ErlangFun
erlps__flatten__1 [(ErlangCons e_0 es_1)] | isEList e_0 =
  let rop_3 = erlps__flatten__1 [es_1]
  in BIF.erlang__op_append [e_0, rop_3]
erlps__flatten__1 [(ErlangCons e_0 es_1)] =
  let tail_3 = erlps__flatten__1 [es_1]
  in ErlangCons e_0 tail_3
erlps__flatten__1 [(ErlangEmptyList)] = ErlangEmptyList
erlps__flatten__1 [arg_0] = EXC.function_clause unit
erlps__flatten__1 args =
  EXC.badarity (ErlangFun 1 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__normalize__2 :: ErlangFun
erlps__normalize__2 [l_0,
                     (ErlangCons (ErlangTuple [(ErlangAtom "aliases"),
                                               as_1]) xs_2)]
  =
  let arg_3 = erlps__substitute_aliases__2 [as_1, l_0]
  in erlps__normalize__2 [arg_3, xs_2]
erlps__normalize__2 [l_0,
                     (ErlangCons (ErlangTuple [(ErlangAtom "expand"),
                                               es_1]) xs_2)]
  =
  let arg_3 = erlps__expand__2 [es_1, l_0]
  in erlps__normalize__2 [arg_3, xs_2]
erlps__normalize__2 [l_0,
                     (ErlangCons (ErlangTuple [(ErlangAtom "negations"),
                                               ns_1]) xs_2)]
  =
  let arg_3 = erlps__substitute_negations__2 [ns_1, l_0]
  in erlps__normalize__2 [arg_3, xs_2]
erlps__normalize__2 [l_0, (ErlangEmptyList)] =
  erlps__compact__1 [l_0]
erlps__normalize__2 [arg_2, arg_3] = EXC.function_clause unit
erlps__normalize__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__split__2 :: ErlangFun
erlps__split__2 [list_0, keys_1] =
  let   
    arg_4 =
      flmap
        (\ lc_7 ->
           let lcRet_8 = ErlangTuple [lc_7, ErlangEmptyList]
           in ErlangCons lcRet_8 ErlangEmptyList)
        keys_1
  in let arg_3 = BIF.maps__from_list__1 [arg_4]
  in let
    matchExpr_14 = erlps__split__3 [list_0, arg_3, ErlangEmptyList]
  in
    case matchExpr_14 of
      (ErlangTuple [store_12, rest_13]) ->
        let   
          tup_el_15 =
            flmap
              (\ lc_18 ->
                 let    arg_20 = BIF.erlang__map_get__2 [lc_18, store_12]
                 in let
                   lcRet_19 =
                     BIF.do_remote_fun_call "Lists" "erlps__reverse__1" [arg_20]
                 in ErlangCons lcRet_19 ErlangEmptyList)
              keys_1
        in let
          tup_el_23 =
            BIF.do_remote_fun_call "Lists" "erlps__reverse__1" [rest_13]
        in ErlangTuple [tup_el_15, tup_el_23]
      _ -> EXC.badmatch matchExpr_14
erlps__split__2 [arg_25, arg_26] = EXC.function_clause unit
erlps__split__2 args =
  EXC.badarity (ErlangFun 2 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__split__3 :: ErlangFun
erlps__split__3 [(ErlangCons p_0 ps_1), store_2, rest_3] =
  case ErlangAtom "true" of
    _ | isEAtom p_0 ->
      let case_4 = BIF.erlang__is_map_key__2 [p_0, store_2]
      in
        case case_4 of
          (ErlangAtom "true") ->
            let arg_8 = erlps__maps_prepend__3 [p_0, p_0, store_2]
            in erlps__split__3 [ps_1, arg_8, rest_3]
          (ErlangAtom "false") ->
            erlps__split__3 [ps_1, store_2, ErlangCons p_0 rest_3]
          something_else -> EXC.case_clause something_else
    _ | ((ErlangAtom "true") ==
           (falsifyErrors
              (\ _ ->
                 let    lop_18 = BIF.erlang__tuple_size__1 [p_0]
                 in let rop_20 = toErl 1
                 in BIF.erlang__op_greaterEq [lop_18, rop_20]))) ->
      let    arg_21 = toErl 1
      in let key_23 = BIF.erlang__element__2 [arg_21, p_0]
      in let case_24 = BIF.erlang__is_map_key__2 [key_23, store_2]
      in
        case case_24 of
          (ErlangAtom "true") ->
            let arg_28 = erlps__maps_prepend__3 [key_23, p_0, store_2]
            in erlps__split__3 [ps_1, arg_28, rest_3]
          (ErlangAtom "false") ->
            erlps__split__3 [ps_1, store_2, ErlangCons p_0 rest_3]
          something_else -> EXC.case_clause something_else
    _ -> erlps__split__3 [ps_1, store_2, ErlangCons p_0 rest_3]
erlps__split__3 [(ErlangEmptyList), store_0, rest_1] =
  ErlangTuple [store_0, rest_1]
erlps__split__3 [arg_4, arg_5, arg_6] = EXC.function_clause unit
erlps__split__3 args =
  EXC.badarity (ErlangFun 3 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args

erlps__maps_prepend__3 :: ErlangFun
erlps__maps_prepend__3 [key_0, val_1, dict_2] =
  let    tail_7 = BIF.erlang__map_get__2 [key_0, dict_2]
  in let
    mapExt_10 =
      ErlangMap (Map.singleton key_0 (ErlangCons val_1 tail_7))
  in
    case findMissingKey dict_2 [key_0] of
      (DM.Nothing) -> BIF.maps__merge__2 [dict_2, mapExt_10]
      (DM.Just missing_12) -> EXC.badkey missing_12
erlps__maps_prepend__3 [arg_13, arg_14, arg_15] =
  EXC.function_clause unit
erlps__maps_prepend__3 args =
  EXC.badarity (ErlangFun 3 (\ _ -> ErlangAtom "purs_tco_sucks"))
    args
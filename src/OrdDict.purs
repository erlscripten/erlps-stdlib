module Orddict(erlps__new__0, erlps__is_key__2,
               erlps__to_list__1, erlps__from_list__1, erlps__size__1,
               erlps__is_empty__1, erlps__fetch__2, erlps__find__2,
               erlps__fetch_keys__1, erlps__erase__2, erlps__take__2,
               erlps__store__3, erlps__append__3, erlps__append_list__3,
               erlps__update__3, erlps__update__4, erlps__update_counter__3,
               erlps__fold__3, erlps__map__2, erlps__filter__2,
               erlps__merge__3) where
{-
This file has been autogenerated
DO NOT EDIT - Your changes WILL be overwritten
Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote
Erlscripten v0.1.0
-}

import Prelude
import Data.Array as DA
import Data.List as DL
import Data.Maybe as DM
import Data.Map as Map
import Data.Tuple as Tup
import Data.BigInt as DBI
import Erlang.Builtins as BIF
import Erlang.Binary as BIN
import Erlang.Helpers as H
import Erlang.Exception as EXC
import Erlang.Type (ErlangFun, ErlangTerm(..), weakCmp, weakEq,
                    weakNEq, weakLt, weakLeq, weakGeq, weakGt)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)


erlps__new__0 :: ErlangFun
erlps__new__0 [] = ErlangEmptyList
erlps__new__0 args =
  (EXC.badarity
     (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__is_key__2 :: ErlangFun
erlps__is_key__2 [key_0, (ErlangCons (ErlangTuple [k_1, _]) _)]
  | (weakLt key_0 k_1) =
  (ErlangAtom "false")
erlps__is_key__2 [key_0,
                  (ErlangCons (ErlangTuple [k_1, _]) dict_2)]
  | (weakGt key_0 k_1) =
  (erlps__is_key__2 [key_0, dict_2])
erlps__is_key__2 [_key_0,
                  (ErlangCons (ErlangTuple [_k_1, _val_2]) _)]
  =
  (ErlangAtom "true")
erlps__is_key__2 [_, (ErlangEmptyList)] = (ErlangAtom "false")
erlps__is_key__2 [arg_0, arg_1] = (EXC.function_clause unit)
erlps__is_key__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__to_list__1 :: ErlangFun
erlps__to_list__1 [dict_0] = dict_0
erlps__to_list__1 [arg_1] = (EXC.function_clause unit)
erlps__to_list__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__from_list__1 :: ErlangFun
erlps__from_list__1 [(ErlangEmptyList)] = ErlangEmptyList
erlps__from_list__1 [pair_0@(ErlangCons (ErlangTuple [_,
                                                      _]) (ErlangEmptyList))]
  =
  pair_0
erlps__from_list__1 [pairs_0] =
  let arg_2 = (erlps__reverse_pairs__2 [pairs_0, ErlangEmptyList])
  in
    (BIF.do_remote_fun_call "Lists" "erlps__ukeysort__2"
       [(ErlangInt (DBI.fromInt 1)), arg_2])
erlps__from_list__1 [arg_5] = (EXC.function_clause unit)
erlps__from_list__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__size__1 :: ErlangFun
erlps__size__1 [d_0] = (BIF.erlang__length__1 [d_0])
erlps__size__1 [arg_2] = (EXC.function_clause unit)
erlps__size__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__is_empty__1 :: ErlangFun
erlps__is_empty__1 [(ErlangEmptyList)] = (ErlangAtom "true")
erlps__is_empty__1 [(ErlangCons _ _)] = (ErlangAtom "false")
erlps__is_empty__1 [arg_0] = (EXC.function_clause unit)
erlps__is_empty__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fetch__2 :: ErlangFun
erlps__fetch__2 [key_0, (ErlangCons (ErlangTuple [k_1, _]) d_2)]
  | (weakGt key_0 k_1) =
  (erlps__fetch__2 [key_0, d_2])
erlps__fetch__2 [key_0,
                 (ErlangCons (ErlangTuple [k_1, value_2]) _)]
  | (weakEq key_0 k_1) =
  value_2
erlps__fetch__2 [arg_3, arg_4] = (EXC.function_clause unit)
erlps__fetch__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__find__2 :: ErlangFun
erlps__find__2 [key_0, (ErlangCons (ErlangTuple [k_1, _]) _)]
  | (weakLt key_0 k_1) =
  (ErlangAtom "error")
erlps__find__2 [key_0, (ErlangCons (ErlangTuple [k_1, _]) d_2)]
  | (weakGt key_0 k_1) =
  (erlps__find__2 [key_0, d_2])
erlps__find__2 [_key_0,
                (ErlangCons (ErlangTuple [_k_1, value_2]) _)]
  =
  (ErlangTuple [(ErlangAtom "ok"), value_2])
erlps__find__2 [_, (ErlangEmptyList)] = (ErlangAtom "error")
erlps__find__2 [arg_0, arg_1] = (EXC.function_clause unit)
erlps__find__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fetch_keys__1 :: ErlangFun
erlps__fetch_keys__1 [(ErlangCons (ErlangTuple [key_0,
                                                _]) dict_1)]
  =
  let tail_3 = (erlps__fetch_keys__1 [dict_1])
  in (ErlangCons key_0 tail_3)
erlps__fetch_keys__1 [(ErlangEmptyList)] = ErlangEmptyList
erlps__fetch_keys__1 [arg_0] = (EXC.function_clause unit)
erlps__fetch_keys__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__erase__2 :: ErlangFun
erlps__erase__2 [key_0,
                 (ErlangCons e_2@(ErlangTuple [k_1, _]) dict_3)]
  | (weakLt key_0 k_1) =
  (ErlangCons e_2 dict_3)
erlps__erase__2 [key_0,
                 (ErlangCons e_2@(ErlangTuple [k_1, _]) dict_3)]
  | (weakGt key_0 k_1) =
  let tail_5 = (erlps__erase__2 [key_0, dict_3])
  in (ErlangCons e_2 tail_5)
erlps__erase__2 [_key_0,
                 (ErlangCons (ErlangTuple [_k_1, _val_2]) dict_3)]
  =
  dict_3
erlps__erase__2 [_, (ErlangEmptyList)] = ErlangEmptyList
erlps__erase__2 [arg_0, arg_1] = (EXC.function_clause unit)
erlps__erase__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__take__2 :: ErlangFun
erlps__take__2 [key_0, dict_1] =
  (erlps__take_1__3 [key_0, dict_1, ErlangEmptyList])
erlps__take__2 [arg_5, arg_6] = (EXC.function_clause unit)
erlps__take__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__take_1__3 :: ErlangFun
erlps__take_1__3 [key_0, (ErlangCons (ErlangTuple [k_1, _]) _),
                  _acc_2]
  | (weakLt key_0 k_1) =
  (ErlangAtom "error")
erlps__take_1__3 [key_0,
                  (ErlangCons p_2@(ErlangTuple [k_1, _]) d_3), acc_4]
  | (weakGt key_0 k_1) =
  (erlps__take_1__3 [key_0, d_3, (ErlangCons p_2 acc_4)])
erlps__take_1__3 [_key_0,
                  (ErlangCons (ErlangTuple [_k_1, value_2]) d_3), acc_4]
  =
  let tup_el_6 = (BIF.lists__reverse__2 [acc_4, d_3])
  in (ErlangTuple [value_2, tup_el_6])
erlps__take_1__3 [_, (ErlangEmptyList), _] = (ErlangAtom "error")
erlps__take_1__3 [arg_0, arg_1, arg_2] =
  (EXC.function_clause unit)
erlps__take_1__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__store__3 :: ErlangFun
erlps__store__3 [key_0, new_1,
                 dict_3@(ErlangCons (ErlangTuple [k_2, _]) _)]
  | (weakLt key_0 k_2) =
  let head_4 = (ErlangTuple [key_0, new_1])
  in (ErlangCons head_4 dict_3)
erlps__store__3 [key_0, new_1,
                 (ErlangCons e_3@(ErlangTuple [k_2, _]) dict_4)]
  | (weakGt key_0 k_2) =
  let tail_6 = (erlps__store__3 [key_0, new_1, dict_4])
  in (ErlangCons e_3 tail_6)
erlps__store__3 [key_0, new_1,
                 (ErlangCons (ErlangTuple [_k_2, _old_3]) dict_4)]
  =
  let head_5 = (ErlangTuple [key_0, new_1])
  in (ErlangCons head_5 dict_4)
erlps__store__3 [key_0, new_1, (ErlangEmptyList)] =
  let head_2 = (ErlangTuple [key_0, new_1])
  in (ErlangCons head_2 ErlangEmptyList)
erlps__store__3 [arg_6, arg_7, arg_8] =
  (EXC.function_clause unit)
erlps__store__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__append__3 :: ErlangFun
erlps__append__3 [key_0, new_1,
                  dict_3@(ErlangCons (ErlangTuple [k_2, _]) _)]
  | (weakLt key_0 k_2) =
  let
    head_4 =
      (ErlangTuple [key_0, (ErlangCons new_1 ErlangEmptyList)])
  in (ErlangCons head_4 dict_3)
erlps__append__3 [key_0, new_1,
                  (ErlangCons e_3@(ErlangTuple [k_2, _]) dict_4)]
  | (weakGt key_0 k_2) =
  let tail_6 = (erlps__append__3 [key_0, new_1, dict_4])
  in (ErlangCons e_3 tail_6)
erlps__append__3 [key_0, new_1,
                  (ErlangCons (ErlangTuple [_k_2, old_3]) dict_4)]
  =
  let   
    tup_el_7 =
      (BIF.erlang__op_append
         [old_3, (ErlangCons new_1 ErlangEmptyList)])
  in let head_5 = (ErlangTuple [key_0, tup_el_7])
  in (ErlangCons head_5 dict_4)
erlps__append__3 [key_0, new_1, (ErlangEmptyList)] =
  let
    head_2 =
      (ErlangTuple [key_0, (ErlangCons new_1 ErlangEmptyList)])
  in (ErlangCons head_2 ErlangEmptyList)
erlps__append__3 [arg_8, arg_9, arg_10] =
  (EXC.function_clause unit)
erlps__append__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__append_list__3 :: ErlangFun
erlps__append_list__3 [key_0, newlist_1,
                       dict_3@(ErlangCons (ErlangTuple [k_2, _]) _)]
  | (weakLt key_0 k_2) =
  let head_4 = (ErlangTuple [key_0, newlist_1])
  in (ErlangCons head_4 dict_3)
erlps__append_list__3 [key_0, newlist_1,
                       (ErlangCons e_3@(ErlangTuple [k_2, _]) dict_4)]
  | (weakGt key_0 k_2) =
  let tail_6 = (erlps__append_list__3 [key_0, newlist_1, dict_4])
  in (ErlangCons e_3 tail_6)
erlps__append_list__3 [key_0, newlist_1,
                       (ErlangCons (ErlangTuple [_k_2, old_3]) dict_4)]
  =
  let    tup_el_7 = (BIF.erlang__op_append [old_3, newlist_1])
  in let head_5 = (ErlangTuple [key_0, tup_el_7])
  in (ErlangCons head_5 dict_4)
erlps__append_list__3 [key_0, newlist_1, (ErlangEmptyList)] =
  let head_2 = (ErlangTuple [key_0, newlist_1])
  in (ErlangCons head_2 ErlangEmptyList)
erlps__append_list__3 [arg_6, arg_7, arg_8] =
  (EXC.function_clause unit)
erlps__append_list__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__update__3 :: ErlangFun
erlps__update__3 [key_0, fun_1,
                  (ErlangCons e_3@(ErlangTuple [k_2, _]) dict_4)]
  | (weakGt key_0 k_2) =
  let tail_6 = (erlps__update__3 [key_0, fun_1, dict_4])
  in (ErlangCons e_3 tail_6)
erlps__update__3 [key_0, fun_1,
                  (ErlangCons (ErlangTuple [k_2, val_3]) dict_4)]
  | (weakEq key_0 k_2) =
  let   
    tup_el_7 =
      (BIF.erlang__apply__2
         [fun_1, (ErlangCons val_3 ErlangEmptyList)])
  in let head_5 = (ErlangTuple [key_0, tup_el_7])
  in (ErlangCons head_5 dict_4)
erlps__update__3 [arg_11, arg_12, arg_13] =
  (EXC.function_clause unit)
erlps__update__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__update__4 :: ErlangFun
erlps__update__4 [key_0, _, init_1,
                  dict_3@(ErlangCons (ErlangTuple [k_2, _]) _)]
  | (weakLt key_0 k_2) =
  let head_4 = (ErlangTuple [key_0, init_1])
  in (ErlangCons head_4 dict_3)
erlps__update__4 [key_0, fun_1, init_2,
                  (ErlangCons e_4@(ErlangTuple [k_3, _]) dict_5)]
  | (weakGt key_0 k_3) =
  let tail_7 = (erlps__update__4 [key_0, fun_1, init_2, dict_5])
  in (ErlangCons e_4 tail_7)
erlps__update__4 [key_0, fun_1, _init_2,
                  (ErlangCons (ErlangTuple [_k_3, val_4]) dict_5)]
  =
  let   
    tup_el_8 =
      (BIF.erlang__apply__2
         [fun_1, (ErlangCons val_4 ErlangEmptyList)])
  in let head_6 = (ErlangTuple [key_0, tup_el_8])
  in (ErlangCons head_6 dict_5)
erlps__update__4 [key_0, _, init_1, (ErlangEmptyList)] =
  let head_2 = (ErlangTuple [key_0, init_1])
  in (ErlangCons head_2 ErlangEmptyList)
erlps__update__4 [arg_6, arg_7, arg_8, arg_9] =
  (EXC.function_clause unit)
erlps__update__4 args =
  (EXC.badarity
     (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__update_counter__3 :: ErlangFun
erlps__update_counter__3 [key_0, incr_1,
                          dict_3@(ErlangCons (ErlangTuple [k_2, _]) _)]
  | (weakLt key_0 k_2) =
  let head_4 = (ErlangTuple [key_0, incr_1])
  in (ErlangCons head_4 dict_3)
erlps__update_counter__3 [key_0, incr_1,
                          (ErlangCons e_3@(ErlangTuple [k_2, _]) dict_4)]
  | (weakGt key_0 k_2) =
  let tail_6 = (erlps__update_counter__3 [key_0, incr_1, dict_4])
  in (ErlangCons e_3 tail_6)
erlps__update_counter__3 [key_0, incr_1,
                          (ErlangCons (ErlangTuple [_k_2, val_3]) dict_4)]
  =
  let    tup_el_7 = (BIF.erlang__op_plus [val_3, incr_1])
  in let head_5 = (ErlangTuple [key_0, tup_el_7])
  in (ErlangCons head_5 dict_4)
erlps__update_counter__3 [key_0, incr_1, (ErlangEmptyList)] =
  let head_2 = (ErlangTuple [key_0, incr_1])
  in (ErlangCons head_2 ErlangEmptyList)
erlps__update_counter__3 [arg_6, arg_7, arg_8] =
  (EXC.function_clause unit)
erlps__update_counter__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fold__3 :: ErlangFun
erlps__fold__3 [f_0, acc_1,
                (ErlangCons (ErlangTuple [key_2, val_3]) d_4)]
  =
  let
    arg_6 =
      (BIF.erlang__apply__2
         [f_0,
          (ErlangCons key_2
             (ErlangCons val_3 (ErlangCons acc_1 ErlangEmptyList)))])
  in (erlps__fold__3 [f_0, arg_6, d_4])
erlps__fold__3 [f_0, acc_1, (ErlangEmptyList)]
  | (H.isEFunA f_0 (ErlangInt (DBI.fromInt 3))) =
  acc_1
erlps__fold__3 [arg_2, arg_3, arg_4] = (EXC.function_clause unit)
erlps__fold__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__map__2 :: ErlangFun
erlps__map__2 [f_0,
               (ErlangCons (ErlangTuple [key_1, val_2]) d_3)]
  =
  let   
    tup_el_6 =
      (BIF.erlang__apply__2
         [f_0, (ErlangCons key_1 (ErlangCons val_2 ErlangEmptyList))])
  in let head_4 = (ErlangTuple [key_1, tup_el_6])
  in let tail_10 = (erlps__map__2 [f_0, d_3])
  in (ErlangCons head_4 tail_10)
erlps__map__2 [f_0, (ErlangEmptyList)]
  | (H.isEFunA f_0 (ErlangInt (DBI.fromInt 2))) =
  ErlangEmptyList
erlps__map__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__map__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__filter__2 :: ErlangFun
erlps__filter__2 [f_0,
                  (ErlangCons e_3@(ErlangTuple [key_1, val_2]) d_4)]
  =
  let
    case_5 =
      (BIF.erlang__apply__2
         [f_0, (ErlangCons key_1 (ErlangCons val_2 ErlangEmptyList))])
  in
    case case_5 of
      (ErlangAtom "true") ->
        let tail_10 = (erlps__filter__2 [f_0, d_4])
        in (ErlangCons e_3 tail_10)
      (ErlangAtom "false") -> (erlps__filter__2 [f_0, d_4])
      something_else -> (EXC.case_clause something_else)
erlps__filter__2 [f_0, (ErlangEmptyList)]
  | (H.isEFunA f_0 (ErlangInt (DBI.fromInt 2))) =
  ErlangEmptyList
erlps__filter__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__filter__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__merge__3 :: ErlangFun
erlps__merge__3 [f_0,
                 (ErlangCons e1_2@(ErlangTuple [k1_1, _]) d1_3),
                 (ErlangCons e2_5@(ErlangTuple [k2_4, _]) d2_6)]
  | (weakLt k1_1 k2_4) =
  let
    tail_8 = (erlps__merge__3 [f_0, d1_3, (ErlangCons e2_5 d2_6)])
  in (ErlangCons e1_2 tail_8)
erlps__merge__3 [f_0,
                 (ErlangCons e1_2@(ErlangTuple [k1_1, _]) d1_3),
                 (ErlangCons e2_5@(ErlangTuple [k2_4, _]) d2_6)]
  | (weakGt k1_1 k2_4) =
  let
    tail_8 = (erlps__merge__3 [f_0, (ErlangCons e1_2 d1_3), d2_6])
  in (ErlangCons e2_5 tail_8)
erlps__merge__3 [f_0,
                 (ErlangCons (ErlangTuple [k1_1, v1_2]) d1_3),
                 (ErlangCons (ErlangTuple [_k2_4, v2_5]) d2_6)]
  =
  let   
    tup_el_9 =
      (BIF.erlang__apply__2
         [f_0,
          (ErlangCons k1_1
             (ErlangCons v1_2 (ErlangCons v2_5 ErlangEmptyList)))])
  in let head_7 = (ErlangTuple [k1_1, tup_el_9])
  in let tail_14 = (erlps__merge__3 [f_0, d1_3, d2_6])
  in (ErlangCons head_7 tail_14)
erlps__merge__3 [f_0, (ErlangEmptyList), d2_1]
  | (H.isEFunA f_0 (ErlangInt (DBI.fromInt 3))) =
  d2_1
erlps__merge__3 [f_0, d1_1, (ErlangEmptyList)]
  | (H.isEFunA f_0 (ErlangInt (DBI.fromInt 3))) =
  d1_1
erlps__merge__3 [arg_2, arg_3, arg_4] =
  (EXC.function_clause unit)
erlps__merge__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__reverse_pairs__2 :: ErlangFun
erlps__reverse_pairs__2 [(ErlangCons h_0@(ErlangTuple [_,
                                                       _]) t_1),
                         acc_2]
  =
  (erlps__reverse_pairs__2 [t_1, (ErlangCons h_0 acc_2)])
erlps__reverse_pairs__2 [(ErlangEmptyList), acc_0] = acc_0
erlps__reverse_pairs__2 [arg_1, arg_2] =
  (EXC.function_clause unit)
erlps__reverse_pairs__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)
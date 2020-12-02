module Maps(erlps__get__3, erlps__filter__2, erlps__fold__3,
            erlps__map__2, erlps__size__1, erlps__new__0,
            erlps__update_with__3, erlps__update_with__4, erlps__without__2,
            erlps__with__2, erlps__iterator__1, erlps__next__1,
            erlps__get__2, erlps__find__2, erlps__from_list__1,
            erlps__is_key__2, erlps__keys__1, erlps__merge__2, erlps__put__3,
            erlps__remove__2, erlps__take__2, erlps__to_list__1,
            erlps__update__3, erlps__values__1) where
{-
This file has been autogenerated
DO NOT EDIT - Your changes WILL be overwritten
Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote
Erlscripten v0.0.2
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
import Erlang.Helpers
import Erlang.Exception as EXC
import Erlang.Type (ErlangFun, ErlangTerm(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)


erlps__get__2 :: ErlangFun
erlps__get__2 [_, _] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__get__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__get__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__find__2 :: ErlangFun
erlps__find__2 [_, _] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__find__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__find__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__from_list__1 :: ErlangFun
erlps__from_list__1 [_] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__from_list__1 [arg_1] = (EXC.function_clause unit)
erlps__from_list__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__is_key__2 :: ErlangFun
erlps__is_key__2 [_, _] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__is_key__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__is_key__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__keys__1 :: ErlangFun
erlps__keys__1 [_] = (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__keys__1 [arg_1] = (EXC.function_clause unit)
erlps__keys__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__merge__2 :: ErlangFun
erlps__merge__2 [_, _] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__merge__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__merge__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__put__3 :: ErlangFun
erlps__put__3 [_, _, _] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__put__3 [arg_1, arg_2, arg_3] = (EXC.function_clause unit)
erlps__put__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__remove__2 :: ErlangFun
erlps__remove__2 [_, _] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__remove__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__remove__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__take__2 :: ErlangFun
erlps__take__2 [_, _] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__take__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__take__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__to_list__1 :: ErlangFun
erlps__to_list__1 [map_0] | (isEMap map_0) =
  let
    arg_1 =
      (BIF.erts_internal__map_next__3
         [(ErlangInt (DBI.fromInt 0)), map_0, ErlangEmptyList])
  in (erlps__to_list_internal__1 [arg_1])
erlps__to_list__1 [map_0] =
  let arg_1 = (ErlangTuple [(ErlangAtom "badmap"), map_0])
  in
    (BIF.erlang__error__2
       [arg_1, (ErlangCons map_0 ErlangEmptyList)])
erlps__to_list__1 [arg_7] = (EXC.function_clause unit)
erlps__to_list__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__to_list_internal__1 :: ErlangFun
erlps__to_list_internal__1 [(ErlangCons iter_0 (ErlangCons map_1 acc_2))]
  | (isENum iter_0) =
  let
    arg_3 = (BIF.erts_internal__map_next__3 [iter_0, map_1, acc_2])
  in (erlps__to_list_internal__1 [arg_3])
erlps__to_list_internal__1 [acc_0] = acc_0
erlps__to_list_internal__1 [arg_1] = (EXC.function_clause unit)
erlps__to_list_internal__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__update__3 :: ErlangFun
erlps__update__3 [_, _, _] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__update__3 [arg_1, arg_2, arg_3] =
  (EXC.function_clause unit)
erlps__update__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__values__1 :: ErlangFun
erlps__values__1 [_] =
  (BIF.erlang__nif_error__1 [(ErlangAtom "undef")])
erlps__values__1 [arg_1] = (EXC.function_clause unit)
erlps__values__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__new__0 :: ErlangFun
erlps__new__0 [] = (ErlangMap Map.empty)
erlps__new__0 args =
  (EXC.badarity (ErlangFun 0 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__update_with__3 :: ErlangFun
erlps__update_with__3 [key_0, fun_1, map_2]
  | ((isEFunA fun_1 (ErlangInt (DBI.fromInt 1))) &&
       (isEMap map_2)) =
  case map_2 of
    (ErlangMap map_4) | (DM.Just value_5) <-
                          ((Map.lookup key_0 map_4)) ->
      let map_ext_7 = (ErlangMap Map.empty)
      in (BIF.maps__merge__2 [map_2, map_ext_7])
    (ErlangMap map_8) ->
      let arg_9 = (ErlangTuple [(ErlangAtom "badkey"), key_0])
      in
        (BIF.erlang__error__2
           [arg_9,
            (ErlangCons key_0
               (ErlangCons fun_1 (ErlangCons map_2 ErlangEmptyList)))])
    something_else -> (EXC.case_clause something_else)
erlps__update_with__3 [key_0, fun_1, map_2] =
  let arg_3 = (erlps__error_type__1 [map_2])
  in
    (BIF.erlang__error__2
       [arg_3,
        (ErlangCons key_0
           (ErlangCons fun_1 (ErlangCons map_2 ErlangEmptyList)))])
erlps__update_with__3 [arg_12, arg_13, arg_14] =
  (EXC.function_clause unit)
erlps__update_with__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__update_with__4 :: ErlangFun
erlps__update_with__4 [key_0, fun_1, init_2, map_3]
  | ((isEFunA fun_1 (ErlangInt (DBI.fromInt 1))) &&
       (isEMap map_3)) =
  case map_3 of
    (ErlangMap map_5) | (DM.Just value_6) <-
                          ((Map.lookup key_0 map_5)) ->
      let map_ext_8 = (ErlangMap Map.empty)
      in (BIF.maps__merge__2 [map_3, map_ext_8])
    (ErlangMap map_9) ->
      let map_ext_13 = (ErlangMap (Map.singleton key_0 init_2))
      in (BIF.maps__merge__2 [map_3, map_ext_13])
    something_else -> (EXC.case_clause something_else)
erlps__update_with__4 [key_0, fun_1, init_2, map_3] =
  let arg_4 = (erlps__error_type__1 [map_3])
  in
    (BIF.erlang__error__2
       [arg_4,
        (ErlangCons key_0
           (ErlangCons fun_1
              (ErlangCons init_2 (ErlangCons map_3 ErlangEmptyList))))])
erlps__update_with__4 [arg_15, arg_16, arg_17, arg_18] =
  (EXC.function_clause unit)
erlps__update_with__4 args =
  (EXC.badarity (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__get__3 :: ErlangFun
erlps__get__3 [key_0, map_1, default_2] | (isEMap map_1) =
  case map_1 of
    (ErlangMap map_4) | (DM.Just value_5) <-
                          ((Map.lookup key_0 map_4)) ->
      value_5
    (ErlangMap map_6) -> default_2
    something_else -> (EXC.case_clause something_else)
erlps__get__3 [key_0, map_1, default_2] =
  let arg_3 = (ErlangTuple [(ErlangAtom "badmap"), map_1])
  in
    (BIF.erlang__error__2
       [arg_3,
        (ErlangCons key_0
           (ErlangCons map_1 (ErlangCons default_2 ErlangEmptyList)))])
erlps__get__3 [arg_13, arg_14, arg_15] =
  (EXC.function_clause unit)
erlps__get__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__filter__2 :: ErlangFun
erlps__filter__2 [pred_0, map_1]
  | ((isEFunA pred_0 (ErlangInt (DBI.fromInt 2))) &&
       (isEMap map_1)) =
  let    arg_4 = (erlps__iterator__1 [map_1])
  in let arg_2 = (erlps__filter_1__2 [pred_0, arg_4])
  in (BIF.maps__from_list__1 [arg_2])
erlps__filter__2 [pred_0, iterator_1]
  | (ErlangAtom "true") <-
      (let   
         lop_7 =
           (BIF.erlang__is_function__2
              [pred_0, (ErlangInt (DBI.fromInt 2))])
       in let lop_11 = (BIF.erlang__is_tuple__1 [iterator_1])
       in let lop_14 = (BIF.erlang__tuple_size__1 [iterator_1])
       in let
         rop_13 =
           (BIF.erlang__op_eq [lop_14, (ErlangInt (DBI.fromInt 3))])
       in let rop_10 = (BIF.erlang__op_andalso [lop_11, rop_13])
       in let lop_6 = (BIF.erlang__op_andalso [lop_7, rop_10])
       in let rop_17 = (BIF.erlang__op_eq [iterator_1, (ErlangAtom "none")])
       in let lop_5 = (BIF.erlang__op_orelse [lop_6, rop_17])
       in let arg_22 = (BIF.erlang__hd__1 [iterator_1])
       in let lop_21 = (BIF.erlang__is_integer__1 [arg_22])
       in let arg_25 = (BIF.erlang__tl__1 [iterator_1])
       in let rop_24 = (BIF.erlang__is_map__1 [arg_25])
       in let rop_20 = (BIF.erlang__op_andalso [lop_21, rop_24])
       in (BIF.erlang__op_orelse [lop_5, rop_20])) =
  let arg_2 = (erlps__filter_1__2 [pred_0, iterator_1])
  in (BIF.maps__from_list__1 [arg_2])
erlps__filter__2 [pred_0, map_1] =
  let arg_2 = (erlps__error_type__1 [map_1])
  in
    (BIF.erlang__error__2
       [arg_2, (ErlangCons pred_0 (ErlangCons map_1 ErlangEmptyList))])
erlps__filter__2 [arg_9, arg_10] = (EXC.function_clause unit)
erlps__filter__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__filter_1__2 :: ErlangFun
erlps__filter_1__2 [pred_0, iter_1] =
  let case_2 = (erlps__next__1 [iter_1])
  in
    case case_2 of
      (ErlangTuple [k_4, v_5, nextiter_6]) ->
        let
          case_7 =
            (BIF.erlang__apply__2
               [pred_0, (ErlangCons k_4 (ErlangCons v_5 ErlangEmptyList))])
        in
          case case_7 of
            (ErlangAtom "true") ->
              let    tail_11 = (ErlangTuple [k_4, v_5])
              in let head_14 = (erlps__filter_1__2 [pred_0, nextiter_6])
              in (ErlangCons tail_11 head_14)
            (ErlangAtom "false") -> (erlps__filter_1__2 [pred_0, nextiter_6])
            something_else -> (EXC.case_clause something_else)
      (ErlangAtom "none") -> ErlangEmptyList
      something_else -> (EXC.case_clause something_else)
erlps__filter_1__2 [arg_19, arg_20] = (EXC.function_clause unit)
erlps__filter_1__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fold__3 :: ErlangFun
erlps__fold__3 [fun_0, init_1, map_2]
  | ((isEFunA fun_0 (ErlangInt (DBI.fromInt 3))) &&
       (isEMap map_2)) =
  let arg_5 = (erlps__iterator__1 [map_2])
  in (erlps__fold_1__3 [fun_0, init_1, arg_5])
erlps__fold__3 [fun_0, init_1, iterator_2]
  | (ErlangAtom "true") <-
      (let   
         lop_8 =
           (BIF.erlang__is_function__2 [fun_0, (ErlangInt (DBI.fromInt 3))])
       in let lop_12 = (BIF.erlang__is_tuple__1 [iterator_2])
       in let lop_15 = (BIF.erlang__tuple_size__1 [iterator_2])
       in let
         rop_14 =
           (BIF.erlang__op_eq [lop_15, (ErlangInt (DBI.fromInt 3))])
       in let rop_11 = (BIF.erlang__op_andalso [lop_12, rop_14])
       in let lop_7 = (BIF.erlang__op_andalso [lop_8, rop_11])
       in let rop_18 = (BIF.erlang__op_eq [iterator_2, (ErlangAtom "none")])
       in let lop_6 = (BIF.erlang__op_orelse [lop_7, rop_18])
       in let arg_23 = (BIF.erlang__hd__1 [iterator_2])
       in let lop_22 = (BIF.erlang__is_integer__1 [arg_23])
       in let arg_26 = (BIF.erlang__tl__1 [iterator_2])
       in let rop_25 = (BIF.erlang__is_map__1 [arg_26])
       in let rop_21 = (BIF.erlang__op_andalso [lop_22, rop_25])
       in (BIF.erlang__op_orelse [lop_6, rop_21])) =
  (erlps__fold_1__3 [fun_0, init_1, iterator_2])
erlps__fold__3 [fun_0, init_1, map_2] =
  let arg_3 = (erlps__error_type_iter__1 [map_2])
  in
    (BIF.erlang__error__2
       [arg_3,
        (ErlangCons fun_0
           (ErlangCons init_1 (ErlangCons map_2 ErlangEmptyList)))])
erlps__fold__3 [arg_12, arg_13, arg_14] =
  (EXC.function_clause unit)
erlps__fold__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fold_1__3 :: ErlangFun
erlps__fold_1__3 [fun_0, acc_1, iter_2] =
  let case_3 = (erlps__next__1 [iter_2])
  in
    case case_3 of
      (ErlangTuple [k_5, v_6, nextiter_7]) ->
        let
          arg_9 =
            (BIF.erlang__apply__2
               [fun_0,
                (ErlangCons k_5
                   (ErlangCons v_6 (ErlangCons acc_1 ErlangEmptyList)))])
        in (erlps__fold_1__3 [fun_0, arg_9, nextiter_7])
      (ErlangAtom "none") -> acc_1
      something_else -> (EXC.case_clause something_else)
erlps__fold_1__3 [arg_15, arg_16, arg_17] =
  (EXC.function_clause unit)
erlps__fold_1__3 args =
  (EXC.badarity (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__map__2 :: ErlangFun
erlps__map__2 [fun_0, map_1]
  | ((isEFunA fun_0 (ErlangInt (DBI.fromInt 2))) &&
       (isEMap map_1)) =
  let    arg_4 = (erlps__iterator__1 [map_1])
  in let arg_2 = (erlps__map_1__2 [fun_0, arg_4])
  in (BIF.maps__from_list__1 [arg_2])
erlps__map__2 [fun_0, iterator_1]
  | (ErlangAtom "true") <-
      (let   
         lop_7 =
           (BIF.erlang__is_function__2 [fun_0, (ErlangInt (DBI.fromInt 2))])
       in let lop_11 = (BIF.erlang__is_tuple__1 [iterator_1])
       in let lop_14 = (BIF.erlang__tuple_size__1 [iterator_1])
       in let
         rop_13 =
           (BIF.erlang__op_eq [lop_14, (ErlangInt (DBI.fromInt 3))])
       in let rop_10 = (BIF.erlang__op_andalso [lop_11, rop_13])
       in let lop_6 = (BIF.erlang__op_andalso [lop_7, rop_10])
       in let rop_17 = (BIF.erlang__op_eq [iterator_1, (ErlangAtom "none")])
       in let lop_5 = (BIF.erlang__op_orelse [lop_6, rop_17])
       in let arg_22 = (BIF.erlang__hd__1 [iterator_1])
       in let lop_21 = (BIF.erlang__is_integer__1 [arg_22])
       in let arg_25 = (BIF.erlang__tl__1 [iterator_1])
       in let rop_24 = (BIF.erlang__is_map__1 [arg_25])
       in let rop_20 = (BIF.erlang__op_andalso [lop_21, rop_24])
       in (BIF.erlang__op_orelse [lop_5, rop_20])) =
  let arg_2 = (erlps__map_1__2 [fun_0, iterator_1])
  in (BIF.maps__from_list__1 [arg_2])
erlps__map__2 [fun_0, map_1] =
  let arg_2 = (erlps__error_type_iter__1 [map_1])
  in
    (BIF.erlang__error__2
       [arg_2, (ErlangCons fun_0 (ErlangCons map_1 ErlangEmptyList))])
erlps__map__2 [arg_9, arg_10] = (EXC.function_clause unit)
erlps__map__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__map_1__2 :: ErlangFun
erlps__map_1__2 [fun_0, iter_1] =
  let case_2 = (erlps__next__1 [iter_1])
  in
    case case_2 of
      (ErlangTuple [k_4, v_5, nextiter_6]) ->
        let   
          tup_el_9 =
            (BIF.erlang__apply__2
               [fun_0, (ErlangCons k_4 (ErlangCons v_5 ErlangEmptyList))])
        in let tail_7 = (ErlangTuple [k_4, tup_el_9])
        in let head_13 = (erlps__map_1__2 [fun_0, nextiter_6])
        in (ErlangCons tail_7 head_13)
      (ErlangAtom "none") -> ErlangEmptyList
      something_else -> (EXC.case_clause something_else)
erlps__map_1__2 [arg_16, arg_17] = (EXC.function_clause unit)
erlps__map_1__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__size__1 :: ErlangFun
erlps__size__1 [map_0] | (isEMap map_0) =
  (BIF.erlang__map_size__1 [map_0])
erlps__size__1 [val_0] =
  let arg_1 = (ErlangTuple [(ErlangAtom "badmap"), val_0])
  in
    (BIF.erlang__error__2
       [arg_1, (ErlangCons val_0 ErlangEmptyList)])
erlps__size__1 [arg_7] = (EXC.function_clause unit)
erlps__size__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__iterator__1 :: ErlangFun
erlps__iterator__1 [m_0] | (isEMap m_0) =
  (ErlangCons (ErlangInt (DBI.fromInt 0)) m_0)
erlps__iterator__1 [m_0] =
  let arg_1 = (ErlangTuple [(ErlangAtom "badmap"), m_0])
  in
    (BIF.erlang__error__2 [arg_1, (ErlangCons m_0 ErlangEmptyList)])
erlps__iterator__1 [arg_7] = (EXC.function_clause unit)
erlps__iterator__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__next__1 :: ErlangFun
erlps__next__1 [(ErlangTuple [k_0, v_1, i_2])] =
  (ErlangTuple [k_0, v_1, i_2])
erlps__next__1 [(ErlangCons path_0 map_1)]
  | ((isENum path_0) && (isEMap map_1)) =
  (BIF.erts_internal__map_next__3 [path_0, map_1, (ErlangAtom "iterator")])
erlps__next__1 [(ErlangAtom "none")] = (ErlangAtom "none")
erlps__next__1 [iter_0] =
  (BIF.erlang__error__2
     [(ErlangAtom "badarg"), (ErlangCons iter_0 ErlangEmptyList)])
erlps__next__1 [arg_5] = (EXC.function_clause unit)
erlps__next__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__without__2 :: ErlangFun
erlps__without__2 [ks_0, m_1]
  | ((isEList ks_0) && (isEMap m_1)) =
  let
    arg_2 =
      (BIF.erlang__make_fun__3
         [(ErlangAtom "maps"), (ErlangAtom "remove"), (ErlangInt (DBI.fromInt 2))])
  in (BIF.do_remote_fun_call "Lists" "erlps__foldl__3" [arg_2, m_1, ks_0])
erlps__without__2 [ks_0, m_1] =
  let arg_2 = (erlps__error_type__1 [m_1])
  in
    (BIF.erlang__error__2
       [arg_2, (ErlangCons ks_0 (ErlangCons m_1 ErlangEmptyList))])
erlps__without__2 [arg_9, arg_10] = (EXC.function_clause unit)
erlps__without__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__with__2 :: ErlangFun
erlps__with__2 [ks_0, map1_1]
  | ((isEList ks_0) && (isEMap map1_1)) =
  let arg_2 = (erlps__with_1__2 [ks_0, map1_1])
  in (BIF.maps__from_list__1 [arg_2])
erlps__with__2 [ks_0, m_1] =
  let arg_2 = (erlps__error_type__1 [m_1])
  in
    (BIF.erlang__error__2
       [arg_2, (ErlangCons ks_0 (ErlangCons m_1 ErlangEmptyList))])
erlps__with__2 [arg_9, arg_10] = (EXC.function_clause unit)
erlps__with__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__with_1__2 :: ErlangFun
erlps__with_1__2 [(ErlangCons k_0 ks_1), map_2] =
  case map_2 of
    (ErlangMap map_4) | (DM.Just v_5) <- ((Map.lookup k_0 map_4)) ->
      let    tail_6 = (ErlangTuple [k_0, v_5])
      in let head_9 = (erlps__with_1__2 [ks_1, map_2])
      in (ErlangCons tail_6 head_9)
    (ErlangMap map_12) -> (erlps__with_1__2 [ks_1, map_2])
    something_else -> (EXC.case_clause something_else)
erlps__with_1__2 [ErlangEmptyList, _map_0] = ErlangEmptyList
erlps__with_1__2 [arg_1, arg_2] = (EXC.function_clause unit)
erlps__with_1__2 args =
  (EXC.badarity (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__error_type__1 :: ErlangFun
erlps__error_type__1 [m_0] | (isEMap m_0) = (ErlangAtom "badarg")
erlps__error_type__1 [v_0] = (ErlangTuple [(ErlangAtom "badmap"), v_0])
erlps__error_type__1 [arg_3] = (EXC.function_clause unit)
erlps__error_type__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__error_type_iter__1 :: ErlangFun
erlps__error_type_iter__1 [m_0]
  | (ErlangAtom "true") <-
      (let    lop_3 = (BIF.erlang__is_map__1 [m_0])
       in let lop_6 = (BIF.erlang__is_tuple__1 [m_0])
       in let lop_9 = (BIF.erlang__tuple_size__1 [m_0])
       in let
         rop_8 = (BIF.erlang__op_eq [lop_9, (ErlangInt (DBI.fromInt 3))])
       in let rop_5 = (BIF.erlang__op_andalso [lop_6, rop_8])
       in let lop_2 = (BIF.erlang__op_orelse [lop_3, rop_5])
       in let rop_12 = (BIF.erlang__op_eq [m_0, (ErlangAtom "none")])
       in let lop_1 = (BIF.erlang__op_orelse [lop_2, rop_12])
       in let arg_17 = (BIF.erlang__hd__1 [m_0])
       in let lop_16 = (BIF.erlang__is_integer__1 [arg_17])
       in let arg_20 = (BIF.erlang__tl__1 [m_0])
       in let rop_19 = (BIF.erlang__is_map__1 [arg_20])
       in let rop_15 = (BIF.erlang__op_andalso [lop_16, rop_19])
       in (BIF.erlang__op_orelse [lop_1, rop_15])) =
  (ErlangAtom "badarg")
erlps__error_type_iter__1 [v_0] =
  (ErlangTuple [(ErlangAtom "badmap"), v_0])
erlps__error_type_iter__1 [arg_3] = (EXC.function_clause unit)
erlps__error_type_iter__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)
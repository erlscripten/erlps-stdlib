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
  | (isEInt iter_0) =
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
      let   
        val_8 =
          (BIF.erlang__apply__2
             [fun_1, (ErlangCons value_5 ErlangEmptyList)])
      in let map_ext_11 = (ErlangMap (Map.singleton key_0 val_8))
      in
        case (findMissingKey map_2 [key_0]) of
          (DM.Nothing) -> (BIF.maps__merge__2 [map_2, map_ext_11])
          (DM.Just missing_13) -> (EXC.badkey missing_13)
    (ErlangMap map_14) ->
      let arg_15 = (ErlangTuple [(ErlangAtom "badkey"), key_0])
      in
        (BIF.erlang__error__2
           [arg_15,
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
      let   
        val_9 =
          (BIF.erlang__apply__2
             [fun_1, (ErlangCons value_6 ErlangEmptyList)])
      in let map_ext_12 = (ErlangMap (Map.singleton key_0 val_9))
      in
        case (findMissingKey map_3 [key_0]) of
          (DM.Nothing) -> (BIF.maps__merge__2 [map_3, map_ext_12])
          (DM.Just missing_14) -> (EXC.badkey missing_14)
    (ErlangMap map_15) ->
      let map_ext_19 = (ErlangMap (Map.singleton key_0 init_2))
      in (BIF.maps__merge__2 [map_3, map_ext_19])
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
      ((falsifyErrors
          (\ _ ->
             let   
               lop_7 =
                 (BIF.erlang__is_function__2
                    [pred_0, (ErlangInt (DBI.fromInt 2))])
             in let
               lop_6 =
                 case lop_7 of
                   (ErlangAtom "false") -> (ErlangAtom "false")
                   (ErlangAtom "true") ->
                     let lop_10 = (BIF.erlang__is_tuple__1 [iterator_1])
                     in
                       case lop_10 of
                         (ErlangAtom "false") -> (ErlangAtom "false")
                         (ErlangAtom "true") ->
                           let lop_12 = (BIF.erlang__tuple_size__1 [iterator_1])
                           in
                             (BIF.erlang__op_eq
                                [lop_12, (ErlangInt (DBI.fromInt 3))])
                         _ -> (EXC.badarg1 lop_10)
                   _ -> (EXC.badarg1 lop_7)
             in let
               lop_5 =
                 case lop_6 of
                   (ErlangAtom "true") -> (ErlangAtom "true")
                   (ErlangAtom "false") ->
                     (BIF.erlang__op_eq [iterator_1, (ErlangAtom "none")])
                   _ -> (EXC.badarg1 lop_6)
             in
               case lop_5 of
                 (ErlangAtom "true") -> (ErlangAtom "true")
                 (ErlangAtom "false") ->
                   let    arg_18 = (BIF.erlang__hd__1 [iterator_1])
                   in let lop_17 = (BIF.erlang__is_integer__1 [arg_18])
                   in
                     case lop_17 of
                       (ErlangAtom "false") -> (ErlangAtom "false")
                       (ErlangAtom "true") ->
                         let arg_20 = (BIF.erlang__tl__1 [iterator_1])
                         in (BIF.erlang__is_map__1 [arg_20])
                       _ -> (EXC.badarg1 lop_17)
                 _ -> (EXC.badarg1 lop_5)))) =
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
              let    head_11 = (ErlangTuple [k_4, v_5])
              in let tail_14 = (erlps__filter_1__2 [pred_0, nextiter_6])
              in (ErlangCons head_11 tail_14)
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
      ((falsifyErrors
          (\ _ ->
             let   
               lop_8 =
                 (BIF.erlang__is_function__2
                    [fun_0, (ErlangInt (DBI.fromInt 3))])
             in let
               lop_7 =
                 case lop_8 of
                   (ErlangAtom "false") -> (ErlangAtom "false")
                   (ErlangAtom "true") ->
                     let lop_11 = (BIF.erlang__is_tuple__1 [iterator_2])
                     in
                       case lop_11 of
                         (ErlangAtom "false") -> (ErlangAtom "false")
                         (ErlangAtom "true") ->
                           let lop_13 = (BIF.erlang__tuple_size__1 [iterator_2])
                           in
                             (BIF.erlang__op_eq
                                [lop_13, (ErlangInt (DBI.fromInt 3))])
                         _ -> (EXC.badarg1 lop_11)
                   _ -> (EXC.badarg1 lop_8)
             in let
               lop_6 =
                 case lop_7 of
                   (ErlangAtom "true") -> (ErlangAtom "true")
                   (ErlangAtom "false") ->
                     (BIF.erlang__op_eq [iterator_2, (ErlangAtom "none")])
                   _ -> (EXC.badarg1 lop_7)
             in
               case lop_6 of
                 (ErlangAtom "true") -> (ErlangAtom "true")
                 (ErlangAtom "false") ->
                   let    arg_19 = (BIF.erlang__hd__1 [iterator_2])
                   in let lop_18 = (BIF.erlang__is_integer__1 [arg_19])
                   in
                     case lop_18 of
                       (ErlangAtom "false") -> (ErlangAtom "false")
                       (ErlangAtom "true") ->
                         let arg_21 = (BIF.erlang__tl__1 [iterator_2])
                         in (BIF.erlang__is_map__1 [arg_21])
                       _ -> (EXC.badarg1 lop_18)
                 _ -> (EXC.badarg1 lop_6)))) =
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
      ((falsifyErrors
          (\ _ ->
             let   
               lop_7 =
                 (BIF.erlang__is_function__2
                    [fun_0, (ErlangInt (DBI.fromInt 2))])
             in let
               lop_6 =
                 case lop_7 of
                   (ErlangAtom "false") -> (ErlangAtom "false")
                   (ErlangAtom "true") ->
                     let lop_10 = (BIF.erlang__is_tuple__1 [iterator_1])
                     in
                       case lop_10 of
                         (ErlangAtom "false") -> (ErlangAtom "false")
                         (ErlangAtom "true") ->
                           let lop_12 = (BIF.erlang__tuple_size__1 [iterator_1])
                           in
                             (BIF.erlang__op_eq
                                [lop_12, (ErlangInt (DBI.fromInt 3))])
                         _ -> (EXC.badarg1 lop_10)
                   _ -> (EXC.badarg1 lop_7)
             in let
               lop_5 =
                 case lop_6 of
                   (ErlangAtom "true") -> (ErlangAtom "true")
                   (ErlangAtom "false") ->
                     (BIF.erlang__op_eq [iterator_1, (ErlangAtom "none")])
                   _ -> (EXC.badarg1 lop_6)
             in
               case lop_5 of
                 (ErlangAtom "true") -> (ErlangAtom "true")
                 (ErlangAtom "false") ->
                   let    arg_18 = (BIF.erlang__hd__1 [iterator_1])
                   in let lop_17 = (BIF.erlang__is_integer__1 [arg_18])
                   in
                     case lop_17 of
                       (ErlangAtom "false") -> (ErlangAtom "false")
                       (ErlangAtom "true") ->
                         let arg_20 = (BIF.erlang__tl__1 [iterator_1])
                         in (BIF.erlang__is_map__1 [arg_20])
                       _ -> (EXC.badarg1 lop_17)
                 _ -> (EXC.badarg1 lop_5)))) =
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
        in let head_7 = (ErlangTuple [k_4, tup_el_9])
        in let tail_13 = (erlps__map_1__2 [fun_0, nextiter_6])
        in (ErlangCons head_7 tail_13)
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
  | ((isEInt path_0) && (isEMap map_1)) =
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
      let    head_6 = (ErlangTuple [k_0, v_5])
      in let tail_9 = (erlps__with_1__2 [ks_1, map_2])
      in (ErlangCons head_6 tail_9)
    (ErlangMap map_12) -> (erlps__with_1__2 [ks_1, map_2])
    something_else -> (EXC.case_clause something_else)
erlps__with_1__2 [(ErlangEmptyList), _map_0] = ErlangEmptyList
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
      ((falsifyErrors
          (\ _ ->
             let    lop_3 = (BIF.erlang__is_map__1 [m_0])
             in let
               lop_2 =
                 case lop_3 of
                   (ErlangAtom "true") -> (ErlangAtom "true")
                   (ErlangAtom "false") ->
                     let lop_5 = (BIF.erlang__is_tuple__1 [m_0])
                     in
                       case lop_5 of
                         (ErlangAtom "false") -> (ErlangAtom "false")
                         (ErlangAtom "true") ->
                           let lop_7 = (BIF.erlang__tuple_size__1 [m_0])
                           in
                             (BIF.erlang__op_eq
                                [lop_7, (ErlangInt (DBI.fromInt 3))])
                         _ -> (EXC.badarg1 lop_5)
                   _ -> (EXC.badarg1 lop_3)
             in let
               lop_1 =
                 case lop_2 of
                   (ErlangAtom "true") -> (ErlangAtom "true")
                   (ErlangAtom "false") -> (BIF.erlang__op_eq [m_0, (ErlangAtom "none")])
                   _ -> (EXC.badarg1 lop_2)
             in
               case lop_1 of
                 (ErlangAtom "true") -> (ErlangAtom "true")
                 (ErlangAtom "false") ->
                   let    arg_13 = (BIF.erlang__hd__1 [m_0])
                   in let lop_12 = (BIF.erlang__is_integer__1 [arg_13])
                   in
                     case lop_12 of
                       (ErlangAtom "false") -> (ErlangAtom "false")
                       (ErlangAtom "true") ->
                         let arg_15 = (BIF.erlang__tl__1 [m_0])
                         in (BIF.erlang__is_map__1 [arg_15])
                       _ -> (EXC.badarg1 lop_12)
                 _ -> (EXC.badarg1 lop_1)))) =
  (ErlangAtom "badarg")
erlps__error_type_iter__1 [v_0] =
  (ErlangTuple [(ErlangAtom "badmap"), v_0])
erlps__error_type_iter__1 [arg_3] = (EXC.function_clause unit)
erlps__error_type_iter__1 args =
  (EXC.badarity (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)
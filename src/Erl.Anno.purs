module Erl.Anno(erlps__new__1, erlps__is_anno__1,
                erlps__column__1, erlps__end_location__1, erlps__file__1,
                erlps__generated__1, erlps__line__1, erlps__location__1,
                erlps__record__1, erlps__text__1, erlps__set_file__2,
                erlps__set_generated__2, erlps__set_line__2,
                erlps__set_location__2, erlps__set_record__2,
                erlps__set_text__2, erlps__to_term__1,
                erlps__from_term__1) where
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


erlps__to_term__1 :: ErlangFun
erlps__to_term__1 [anno_0] = anno_0
erlps__to_term__1 [arg_1] = EXC.function_clause unit
erlps__to_term__1 args =
  EXC.badarity (ErlangFun 1 erlps__to_term__1) args

erlps__from_term__1 :: ErlangFun
erlps__from_term__1 [line_0]
  | (isEInt line_0) && (weakLt line_0 (toErl 0)) =
  let    arg_3 = BIF.erlang__op_neg [line_0]
  in let arg_2 = erlps__new__1 [arg_3]
  in erlps__set_generated__2 [ErlangAtom "true", arg_2]
erlps__from_term__1 [term_0] = term_0
erlps__from_term__1 [arg_1] = EXC.function_clause unit
erlps__from_term__1 args =
  EXC.badarity (ErlangFun 1 erlps__from_term__1) args

erlps__new__1 :: ErlangFun
erlps__new__1 [line_0]
  | (isEInt line_0) && (weakGeq line_0 (toErl 0)) =
  erlps__new_location__1 [line_0]
erlps__new__1 [loc_2@(ErlangTuple [line_0, column_1])]
  | (((isEInt line_0) && (weakGeq line_0 (toErl 0))) &&
       (isEInt column_1)) &&
      (weakGeq column_1 (toErl 1)) =
  erlps__new_location__1 [loc_2]
erlps__new__1 [term_0] =
  BIF.erlang__error__2
    [ErlangAtom "badarg", ErlangCons term_0 ErlangEmptyList]
erlps__new__1 [arg_5] = EXC.function_clause unit
erlps__new__1 args =
  EXC.badarity (ErlangFun 1 erlps__new__1) args

erlps__new_location__1 :: ErlangFun
erlps__new_location__1 [location_0] = location_0
erlps__new_location__1 [arg_1] = EXC.function_clause unit
erlps__new_location__1 args =
  EXC.badarity (ErlangFun 1 erlps__new_location__1) args

erlps__is_anno__1 :: ErlangFun
erlps__is_anno__1 [line_0]
  | (isEInt line_0) && (weakGeq line_0 (toErl 0)) =
  ErlangAtom "true"
erlps__is_anno__1 [(ErlangTuple [line_0, column_1])]
  | (((isEInt line_0) && (weakGeq line_0 (toErl 0))) &&
       (isEInt column_1)) &&
      (weakGeq column_1 (toErl 1)) =
  ErlangAtom "true"
erlps__is_anno__1 [anno_0] =
  let lop_1 = BIF.erlang__op_exactNeq [anno_0, ErlangEmptyList]
  in
    case lop_1 of
      (ErlangAtom "false") -> ErlangAtom "false"
      (ErlangAtom "true") ->
        let lop_4 = erlps__is_anno1__1 [anno_0]
        in
          case lop_4 of
            (ErlangAtom "false") -> ErlangAtom "false"
            (ErlangAtom "true") ->
              let arg_7 = toErl 1
              in BIF.lists__keymember__3 [ErlangAtom "location", arg_7, anno_0]
            _ -> EXC.badarg1 lop_4
      _ -> EXC.badarg1 lop_1
erlps__is_anno__1 [arg_9] = EXC.function_clause unit
erlps__is_anno__1 args =
  EXC.badarity (ErlangFun 1 erlps__is_anno__1) args

erlps__is_anno1__1 :: ErlangFun
erlps__is_anno1__1 [(ErlangCons (ErlangTuple [item_0,
                                              value_1]) anno_2)]
  =
  let lop_3 = erlps__is_anno2__2 [item_0, value_1]
  in
    case lop_3 of
      (ErlangAtom "false") -> ErlangAtom "false"
      (ErlangAtom "true") -> erlps__is_anno1__1 [anno_2]
      _ -> EXC.badarg1 lop_3
erlps__is_anno1__1 [a_0] =
  BIF.erlang__op_exactEq [a_0, ErlangEmptyList]
erlps__is_anno1__1 [arg_3] = EXC.function_clause unit
erlps__is_anno1__1 args =
  EXC.badarity (ErlangFun 1 erlps__is_anno1__1) args

erlps__is_anno2__2 :: ErlangFun
erlps__is_anno2__2 [(ErlangAtom "location"), line_0]
  | (isEInt line_0) && (weakGeq line_0 (toErl 0)) =
  ErlangAtom "true"
erlps__is_anno2__2 [(ErlangAtom "location"),
                    (ErlangTuple [line_0, column_1])]
  | (((isEInt line_0) && (weakGeq line_0 (toErl 0))) &&
       (isEInt column_1)) &&
      (weakGeq column_1 (toErl 1)) =
  ErlangAtom "true"
erlps__is_anno2__2 [(ErlangAtom "generated"),
                    (ErlangAtom "true")]
  =
  ErlangAtom "true"
erlps__is_anno2__2 [(ErlangAtom "file"), filename_0] =
  erlps__is_filename__1 [filename_0]
erlps__is_anno2__2 [(ErlangAtom "record"), (ErlangAtom "true")] =
  ErlangAtom "true"
erlps__is_anno2__2 [(ErlangAtom "text"), text_0] =
  erlps__is_string__1 [text_0]
erlps__is_anno2__2 [_, _] = ErlangAtom "false"
erlps__is_anno2__2 [arg_0, arg_1] = EXC.function_clause unit
erlps__is_anno2__2 args =
  EXC.badarity (ErlangFun 2 erlps__is_anno2__2) args

erlps__is_filename__1 :: ErlangFun
erlps__is_filename__1 [t_0] =
  let lop_1 = BIF.erlang__is_list__1 [t_0]
  in
    case lop_1 of
      (ErlangAtom "true") -> ErlangAtom "true"
      (ErlangAtom "false") -> BIF.erlang__is_binary__1 [t_0]
      _ -> EXC.badarg1 lop_1
erlps__is_filename__1 [arg_4] = EXC.function_clause unit
erlps__is_filename__1 args =
  EXC.badarity (ErlangFun 1 erlps__is_filename__1) args

erlps__is_string__1 :: ErlangFun
erlps__is_string__1 [t_0] = BIF.erlang__is_list__1 [t_0]
erlps__is_string__1 [arg_2] = EXC.function_clause unit
erlps__is_string__1 args =
  EXC.badarity (ErlangFun 1 erlps__is_string__1) args

erlps__column__1 :: ErlangFun
erlps__column__1 [(ErlangTuple [line_0, column_1])]
  | (((isEInt line_0) && (weakGeq line_0 (toErl 0))) &&
       (isEInt column_1)) &&
      (weakGeq column_1 (toErl 1)) =
  column_1
erlps__column__1 [line_0]
  | (isEInt line_0) && (weakGeq line_0 (toErl 0)) =
  ErlangAtom "undefined"
erlps__column__1 [anno_0] =
  let case_1 = erlps__location__1 [anno_0]
  in
    case case_1 of
      (ErlangTuple [_line_3, column_4]) -> column_4
      _line_5 -> ErlangAtom "undefined"
erlps__column__1 [arg_6] = EXC.function_clause unit
erlps__column__1 args =
  EXC.badarity (ErlangFun 1 erlps__column__1) args

erlps__end_location__1 :: ErlangFun
erlps__end_location__1 [anno_0] =
  let case_1 = erlps__text__1 [anno_0]
  in
    case case_1 of
      (ErlangAtom "undefined") -> ErlangAtom "undefined"
      text_3 ->
        let case_4 = erlps__location__1 [anno_0]
        in
          case case_4 of
            (ErlangTuple [line_6, column_7]) ->
              erlps__end_location__3 [text_3, line_6, column_7]
            line_11 -> erlps__end_location__2 [text_3, line_11]
erlps__end_location__1 [arg_14] = EXC.function_clause unit
erlps__end_location__1 args =
  EXC.badarity (ErlangFun 1 erlps__end_location__1) args

erlps__file__1 :: ErlangFun
erlps__file__1 [line_0]
  | (isEInt line_0) && (weakGeq line_0 (toErl 0)) =
  ErlangAtom "undefined"
erlps__file__1 [(ErlangTuple [line_0, column_1])]
  | (((isEInt line_0) && (weakGeq line_0 (toErl 0))) &&
       (isEInt column_1)) &&
      (weakGeq column_1 (toErl 1)) =
  ErlangAtom "undefined"
erlps__file__1 [anno_0] =
  erlps__anno_info__2 [anno_0, ErlangAtom "file"]
erlps__file__1 [arg_3] = EXC.function_clause unit
erlps__file__1 args =
  EXC.badarity (ErlangFun 1 erlps__file__1) args

erlps__generated__1 :: ErlangFun
erlps__generated__1 [line_0]
  | (isEInt line_0) && (weakGeq line_0 (toErl 0)) =
  ErlangAtom "false"
erlps__generated__1 [(ErlangTuple [line_0, column_1])]
  | (((isEInt line_0) && (weakGeq line_0 (toErl 0))) &&
       (isEInt column_1)) &&
      (weakGeq column_1 (toErl 1)) =
  ErlangAtom "false"
erlps__generated__1 [anno_0] =
  erlps__anno_info__3
    [anno_0, ErlangAtom "generated", ErlangAtom "false"]
erlps__generated__1 [arg_4] = EXC.function_clause unit
erlps__generated__1 args =
  EXC.badarity (ErlangFun 1 erlps__generated__1) args

erlps__line__1 :: ErlangFun
erlps__line__1 [anno_0] =
  let case_1 = erlps__location__1 [anno_0]
  in
    case case_1 of
      (ErlangTuple [line_3, _column_4]) -> line_3
      line_5 -> line_5
erlps__line__1 [arg_6] = EXC.function_clause unit
erlps__line__1 args =
  EXC.badarity (ErlangFun 1 erlps__line__1) args

erlps__location__1 :: ErlangFun
erlps__location__1 [line_0]
  | (isEInt line_0) && (weakGeq line_0 (toErl 0)) =
  line_0
erlps__location__1 [location_2@(ErlangTuple [line_0, column_1])]
  | (((isEInt line_0) && (weakGeq line_0 (toErl 0))) &&
       (isEInt column_1)) &&
      (weakGeq column_1 (toErl 1)) =
  location_2
erlps__location__1 [anno_0] =
  erlps__anno_info__2 [anno_0, ErlangAtom "location"]
erlps__location__1 [arg_3] = EXC.function_clause unit
erlps__location__1 args =
  EXC.badarity (ErlangFun 1 erlps__location__1) args

erlps__record__1 :: ErlangFun
erlps__record__1 [line_0]
  | (isEInt line_0) && (weakGeq line_0 (toErl 0)) =
  ErlangAtom "false"
erlps__record__1 [(ErlangTuple [line_0, column_1])]
  | (((isEInt line_0) && (weakGeq line_0 (toErl 0))) &&
       (isEInt column_1)) &&
      (weakGeq column_1 (toErl 1)) =
  ErlangAtom "false"
erlps__record__1 [anno_0] =
  erlps__anno_info__3
    [anno_0, ErlangAtom "record", ErlangAtom "false"]
erlps__record__1 [arg_4] = EXC.function_clause unit
erlps__record__1 args =
  EXC.badarity (ErlangFun 1 erlps__record__1) args

erlps__text__1 :: ErlangFun
erlps__text__1 [line_0]
  | (isEInt line_0) && (weakGeq line_0 (toErl 0)) =
  ErlangAtom "undefined"
erlps__text__1 [(ErlangTuple [line_0, column_1])]
  | (((isEInt line_0) && (weakGeq line_0 (toErl 0))) &&
       (isEInt column_1)) &&
      (weakGeq column_1 (toErl 1)) =
  ErlangAtom "undefined"
erlps__text__1 [anno_0] =
  erlps__anno_info__2 [anno_0, ErlangAtom "text"]
erlps__text__1 [arg_3] = EXC.function_clause unit
erlps__text__1 args =
  EXC.badarity (ErlangFun 1 erlps__text__1) args

erlps__set_file__2 :: ErlangFun
erlps__set_file__2 [file_0, anno_1] =
  erlps__set__3 [ErlangAtom "file", file_0, anno_1]
erlps__set_file__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__set_file__2 args =
  EXC.badarity (ErlangFun 2 erlps__set_file__2) args

erlps__set_generated__2 :: ErlangFun
erlps__set_generated__2 [generated_0, anno_1] =
  erlps__set__3 [ErlangAtom "generated", generated_0, anno_1]
erlps__set_generated__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__set_generated__2 args =
  EXC.badarity (ErlangFun 2 erlps__set_generated__2) args

erlps__set_line__2 :: ErlangFun
erlps__set_line__2 [line_0, anno_1] =
  let case_2 = erlps__location__1 [anno_1]
  in
    case case_2 of
      (ErlangTuple [_line_4, column_5]) ->
        let arg_6 = ErlangTuple [line_0, column_5]
        in erlps__set_location__2 [arg_6, anno_1]
      _line_10 -> erlps__set_location__2 [line_0, anno_1]
erlps__set_line__2 [arg_13, arg_14] = EXC.function_clause unit
erlps__set_line__2 args =
  EXC.badarity (ErlangFun 2 erlps__set_line__2) args

erlps__set_location__2 :: ErlangFun
erlps__set_location__2 [line_0, l_1]
  | (((isEInt l_1) && (weakGeq l_1 (toErl 0))) &&
       (isEInt line_0)) &&
      (weakGeq line_0 (toErl 0)) =
  erlps__new_location__1 [line_0]
erlps__set_location__2 [line_0, (ErlangTuple [l_1, column_2])]
  | (((((isEInt l_1) && (weakGeq l_1 (toErl 0))) &&
         (isEInt column_2)) &&
        (weakGeq column_2 (toErl 1))) &&
       (isEInt line_0)) &&
      (weakGeq line_0 (toErl 0)) =
  erlps__new_location__1 [line_0]
erlps__set_location__2 [loc_2@(ErlangTuple [l_0, c_1]), line_3]
  | (((((isEInt line_3) && (weakGeq line_3 (toErl 0))) &&
         (isEInt l_0)) &&
        (weakGeq l_0 (toErl 0))) &&
       (isEInt c_1)) &&
      (weakGeq c_1 (toErl 1)) =
  erlps__new_location__1 [loc_2]
erlps__set_location__2 [loc_2@(ErlangTuple [l_0, c_1]),
                        (ErlangTuple [line_3, column_4])]
  | (((((((isEInt line_3) && (weakGeq line_3 (toErl 0))) &&
           (isEInt column_4)) &&
          (weakGeq column_4 (toErl 1))) &&
         (isEInt l_0)) &&
        (weakGeq l_0 (toErl 0))) &&
       (isEInt c_1)) &&
      (weakGeq c_1 (toErl 1)) =
  erlps__new_location__1 [loc_2]
erlps__set_location__2 [location_0, anno_1] =
  erlps__set__3 [ErlangAtom "location", location_0, anno_1]
erlps__set_location__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__set_location__2 args =
  EXC.badarity (ErlangFun 2 erlps__set_location__2) args

erlps__set_record__2 :: ErlangFun
erlps__set_record__2 [record_0, anno_1] =
  erlps__set__3 [ErlangAtom "record", record_0, anno_1]
erlps__set_record__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__set_record__2 args =
  EXC.badarity (ErlangFun 2 erlps__set_record__2) args

erlps__set_text__2 :: ErlangFun
erlps__set_text__2 [text_0, anno_1] =
  erlps__set__3 [ErlangAtom "text", text_0, anno_1]
erlps__set_text__2 [arg_5, arg_6] = EXC.function_clause unit
erlps__set_text__2 args =
  EXC.badarity (ErlangFun 2 erlps__set_text__2) args

erlps__set__3 :: ErlangFun
erlps__set__3 [item_0, value_1, anno_2] =
  let    tup_el_4 = erlps__is_settable__2 [item_0, value_1]
  in let case_3 = ErlangTuple [tup_el_4, anno_2]
  in
    case case_3 of
      (ErlangTuple [(ErlangAtom "true"), line_8]) | (isEInt line_8) &&
                                                      (weakGeq line_8
                                                         (toErl 0)) ->
        let head_12 = ErlangTuple [ErlangAtom "location", line_8]
        in
          erlps__set_anno__3
            [item_0, value_1, ErlangCons head_12 ErlangEmptyList]
      (ErlangTuple [(ErlangAtom "true"),
                    location_18@(ErlangTuple [l_16, c_17])]) | (((isEInt
                                                                    l_16) &&
                                                                   (weakGeq l_16
                                                                      (toErl
                                                                         0))) &&
                                                                  (isEInt
                                                                     c_17)) &&
                                                                 (weakGeq c_17
                                                                    (toErl
                                                                       1)) ->
        let head_22 = ErlangTuple [ErlangAtom "location", location_18]
        in
          erlps__set_anno__3
            [item_0, value_1, ErlangCons head_22 ErlangEmptyList]
      (ErlangTuple [(ErlangAtom "true"), a_26]) | (isEList a_26) &&
                                                    ((/=) a_26
                                                       ErlangEmptyList) ->
        erlps__set_anno__3 [item_0, value_1, anno_2]
      _ ->
        BIF.erlang__error__2
          [ErlangAtom "badarg",
           ErlangCons item_0
             (ErlangCons value_1 (ErlangCons anno_2 ErlangEmptyList))]
erlps__set__3 [arg_38, arg_39, arg_40] = EXC.function_clause unit
erlps__set__3 args =
  EXC.badarity (ErlangFun 3 erlps__set__3) args

erlps__set_anno__3 :: ErlangFun
erlps__set_anno__3 [item_0, value_1, anno_2] =
  let case_3 = erlps__default__2 [item_0, value_1]
  in
    case case_3 of
      (ErlangAtom "true") -> erlps__reset__2 [anno_2, item_0]
      (ErlangAtom "false") ->
        let    case_8 = erlps__anno_info__2 [anno_2, item_0]
        in let
          r_21 =
            case case_8 of
              (ErlangAtom "undefined") ->
                let head_11 = ErlangTuple [item_0, value_1]
                in ErlangCons head_11 anno_2
              _ ->
                let    arg_16 = toErl 1
                in let arg_18 = ErlangTuple [item_0, value_1]
                in
                  BIF.do_remote_fun_call "Lists" "erlps__keyreplace__4"
                    [item_0, arg_16, anno_2, arg_18]
        in erlps__reset_simplify__1 [r_21]
      something_else -> EXC.case_clause something_else
erlps__set_anno__3 [arg_23, arg_24, arg_25] =
  EXC.function_clause unit
erlps__set_anno__3 args =
  EXC.badarity (ErlangFun 3 erlps__set_anno__3) args

erlps__reset__2 :: ErlangFun
erlps__reset__2 [anno_0, item_1] =
  let    arg_3 = toErl 1
  in let
    a_5 =
      BIF.do_remote_fun_call "Lists" "erlps__keydelete__3"
        [item_1, arg_3, anno_0]
  in erlps__reset_simplify__1 [a_5]
erlps__reset__2 [arg_7, arg_8] = EXC.function_clause unit
erlps__reset__2 args =
  EXC.badarity (ErlangFun 2 erlps__reset__2) args

erlps__reset_simplify__1 :: ErlangFun
erlps__reset_simplify__1 [a_0] = erlps__simplify__1 [a_0]
erlps__reset_simplify__1 [arg_2] = EXC.function_clause unit
erlps__reset_simplify__1 args =
  EXC.badarity (ErlangFun 1 erlps__reset_simplify__1) args

erlps__simplify__1 :: ErlangFun
erlps__simplify__1 [(ErlangCons (ErlangTuple [(ErlangAtom "location"),
                                              location_0]) (ErlangEmptyList))]
  =
  location_0
erlps__simplify__1 [anno_0] = anno_0
erlps__simplify__1 [arg_1] = EXC.function_clause unit
erlps__simplify__1 args =
  EXC.badarity (ErlangFun 1 erlps__simplify__1) args

erlps__anno_info__3 :: ErlangFun
erlps__anno_info__3 [anno_0, item_1, default_2] =
  EXC.tryOfCatch
    (\ _ ->
       let arg_4 = toErl 1
       in BIF.lists__keyfind__3 [item_1, arg_4, anno_0])
    (\ of_6 ->
       case of_6 of
         (ErlangAtom "false") -> default_2
         (ErlangTuple [item_9, value_10]) | item_9 == item_1 -> value_10
         something_else -> EXC.try_clause something_else)
    (\ ex_7 ->
       case ex_7 of
         (ErlangTuple [_, _, _]) ->
           BIF.erlang__error__2
             [ErlangAtom "badarg", ErlangCons anno_0 ErlangEmptyList]
         ex_8 -> EXC.raise ex_8)
erlps__anno_info__3 [arg_15, arg_16, arg_17] =
  EXC.function_clause unit
erlps__anno_info__3 args =
  EXC.badarity (ErlangFun 3 erlps__anno_info__3) args

erlps__anno_info__2 :: ErlangFun
erlps__anno_info__2 [anno_0, item_1] =
  EXC.tryOfCatch
    (\ _ ->
       let arg_3 = toErl 1
       in BIF.lists__keyfind__3 [item_1, arg_3, anno_0])
    (\ of_5 ->
       case of_5 of
         (ErlangTuple [item_8, value_9]) | item_8 == item_1 -> value_9
         (ErlangAtom "false") -> ErlangAtom "undefined"
         something_else -> EXC.try_clause something_else)
    (\ ex_6 ->
       case ex_6 of
         (ErlangTuple [_, _, _]) ->
           BIF.erlang__error__2
             [ErlangAtom "badarg", ErlangCons anno_0 ErlangEmptyList]
         ex_7 -> EXC.raise ex_7)
erlps__anno_info__2 [arg_14, arg_15] = EXC.function_clause unit
erlps__anno_info__2 args =
  EXC.badarity (ErlangFun 2 erlps__anno_info__2) args

erlps__end_location__3 :: ErlangFun
erlps__end_location__3 [(ErlangEmptyList), line_0, column_1] =
  ErlangTuple [line_0, column_1]
erlps__end_location__3 [(ErlangCons (ErlangInt num_0) string_1),
                        line_2, _column_3]
  | (ErlangInt num_0) == (toErl 10) =
  let    rop_7 = toErl 1
  in let arg_5 = BIF.erlang__op_plus [line_2, rop_7]
  in let arg_8 = toErl 1
  in erlps__end_location__3 [string_1, arg_5, arg_8]
erlps__end_location__3 [(ErlangCons _ string_0), line_1,
                        column_2]
  =
  let    rop_7 = toErl 1
  in let arg_5 = BIF.erlang__op_plus [column_2, rop_7]
  in erlps__end_location__3 [string_0, line_1, arg_5]
erlps__end_location__3 [arg_8, arg_9, arg_10] =
  EXC.function_clause unit
erlps__end_location__3 args =
  EXC.badarity (ErlangFun 3 erlps__end_location__3) args

erlps__end_location__2 :: ErlangFun
erlps__end_location__2 [(ErlangEmptyList), line_0] = line_0
erlps__end_location__2 [(ErlangCons (ErlangInt num_0) string_1),
                        line_2]
  | (ErlangInt num_0) == (toErl 10) =
  let    rop_6 = toErl 1
  in let arg_4 = BIF.erlang__op_plus [line_2, rop_6]
  in erlps__end_location__2 [string_1, arg_4]
erlps__end_location__2 [(ErlangCons _ string_0), line_1] =
  erlps__end_location__2 [string_0, line_1]
erlps__end_location__2 [arg_4, arg_5] = EXC.function_clause unit
erlps__end_location__2 args =
  EXC.badarity (ErlangFun 2 erlps__end_location__2) args

erlps__is_settable__2 :: ErlangFun
erlps__is_settable__2 [(ErlangAtom "file"), file_0] =
  erlps__is_filename__1 [file_0]
erlps__is_settable__2 [(ErlangAtom "generated"), boolean_0]
  | ((==) (ErlangAtom "true") boolean_0) ||
      ((ErlangAtom "true") ==
         (falsifyErrors (\ _ -> BIF.erlang__not__1 [boolean_0]))) =
  ErlangAtom "true"
erlps__is_settable__2 [(ErlangAtom "location"), line_0]
  | (isEInt line_0) && (weakGeq line_0 (toErl 0)) =
  ErlangAtom "true"
erlps__is_settable__2 [(ErlangAtom "location"),
                       (ErlangTuple [line_0, column_1])]
  | (((isEInt line_0) && (weakGeq line_0 (toErl 0))) &&
       (isEInt column_1)) &&
      (weakGeq column_1 (toErl 1)) =
  ErlangAtom "true"
erlps__is_settable__2 [(ErlangAtom "record"), boolean_0]
  | ((==) (ErlangAtom "true") boolean_0) ||
      ((ErlangAtom "true") ==
         (falsifyErrors (\ _ -> BIF.erlang__not__1 [boolean_0]))) =
  ErlangAtom "true"
erlps__is_settable__2 [(ErlangAtom "text"), text_0] =
  erlps__is_string__1 [text_0]
erlps__is_settable__2 [_, _] = ErlangAtom "false"
erlps__is_settable__2 [arg_0, arg_1] = EXC.function_clause unit
erlps__is_settable__2 args =
  EXC.badarity (ErlangFun 2 erlps__is_settable__2) args

erlps__default__2 :: ErlangFun
erlps__default__2 [(ErlangAtom "generated"),
                   (ErlangAtom "false")]
  =
  ErlangAtom "true"
erlps__default__2 [(ErlangAtom "record"), (ErlangAtom "false")] =
  ErlangAtom "true"
erlps__default__2 [_, _] = ErlangAtom "false"
erlps__default__2 [arg_0, arg_1] = EXC.function_clause unit
erlps__default__2 args =
  EXC.badarity (ErlangFun 2 erlps__default__2) args
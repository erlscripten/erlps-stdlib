module Sets.Test.Lib(erlps__new__2) where
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
import Erlang.Type (ErlangFun, ErlangTerm(..), weakCmp, weakEq,
                    weakNEq, weakLt, weakLeq, weakGeq, weakGt)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)


erlps__new__2 :: ErlangFun
erlps__new__2 [mod_0, eq_1] =
  (ErlangFun 2
     let
       lambda_2 [(ErlangAtom "add_element"), (ErlangTuple [el_5, s_6])]
         =
         (erlps__add_element__3 [mod_0, el_5, s_6])
       lambda_2 [(ErlangAtom "del_element"),
                 (ErlangTuple [el_10, s_11])]
         =
         (erlps__del_element__3 [mod_0, el_10, s_11])
       lambda_2 [(ErlangAtom "empty"), (ErlangEmptyList)] =
         (BIF.erlang__apply__3
            [mod_0, (ErlangAtom "new"), ErlangEmptyList])
       lambda_2 [(ErlangAtom "equal"), (ErlangTuple [s1_18, s2_19])] =
         (BIF.erlang__apply__2
            [eq_1, (ErlangCons s1_18 (ErlangCons s2_19 ErlangEmptyList))])
       lambda_2 [(ErlangAtom "filter"), (ErlangTuple [f_23, s_24])] =
         (erlps__filter__3 [mod_0, f_23, s_24])
       lambda_2 [(ErlangAtom "fold"), (ErlangTuple [f_28, a_29, s_30])]
         =
         (erlps__fold__4 [mod_0, f_28, a_29, s_30])
       lambda_2 [(ErlangAtom "from_list"), l_35] =
         (BIF.erlang__apply__3
            [mod_0, (ErlangAtom "from_list"),
             (ErlangCons l_35 ErlangEmptyList)])
       lambda_2 [(ErlangAtom "intersection"),
                 (ErlangTuple [s1_41, s2_42])]
         =
         (erlps__intersection__4 [mod_0, eq_1, s1_41, s2_42])
       lambda_2 [(ErlangAtom "intersection"), ss_47] =
         (erlps__intersection__3 [mod_0, eq_1, ss_47])
       lambda_2 [(ErlangAtom "is_empty"), s_51] =
         (BIF.erlang__apply__3
            [mod_0, (ErlangAtom "is_empty"),
             (ErlangCons s_51 ErlangEmptyList)])
       lambda_2 [(ErlangAtom "is_set"), s_57] =
         (BIF.erlang__apply__3
            [mod_0, (ErlangAtom "is_set"),
             (ErlangCons s_57 ErlangEmptyList)])
       lambda_2 [(ErlangAtom "is_subset"), (ErlangTuple [s_63, set_64])]
         =
         (erlps__is_subset__4 [mod_0, eq_1, s_63, set_64])
       lambda_2 [(ErlangAtom "iterator"), s_69] =
         (BIF.erlang__apply__3
            [mod_0, (ErlangAtom "iterator"),
             (ErlangCons s_69 ErlangEmptyList)])
       lambda_2 [(ErlangAtom "iterator_from"),
                 (ErlangTuple [start_75, s_76])]
         =
         (BIF.erlang__apply__3
            [mod_0, (ErlangAtom "iterator_from"),
             (ErlangCons start_75 (ErlangCons s_76 ErlangEmptyList))])
       lambda_2 [(ErlangAtom "module"), (ErlangEmptyList)] = mod_0
       lambda_2 [(ErlangAtom "next"), i_84] =
         (BIF.erlang__apply__3
            [mod_0, (ErlangAtom "next"), (ErlangCons i_84 ErlangEmptyList)])
       lambda_2 [(ErlangAtom "singleton"), e_90] =
         (erlps__singleton__2 [mod_0, e_90])
       lambda_2 [(ErlangAtom "size"), s_93] =
         (BIF.erlang__apply__3
            [mod_0, (ErlangAtom "size"), (ErlangCons s_93 ErlangEmptyList)])
       lambda_2 [(ErlangAtom "subtract"), (ErlangTuple [s1_99, s2_100])]
         =
         (erlps__subtract__3 [mod_0, s1_99, s2_100])
       lambda_2 [(ErlangAtom "to_list"), s_104] =
         (BIF.erlang__apply__3
            [mod_0, (ErlangAtom "to_list"),
             (ErlangCons s_104 ErlangEmptyList)])
       lambda_2 [(ErlangAtom "union"), (ErlangTuple [s1_110, s2_111])] =
         (erlps__union__4 [mod_0, eq_1, s1_110, s2_111])
       lambda_2 [(ErlangAtom "union"), ss_116] =
         (erlps__union__3 [mod_0, eq_1, ss_116])
       lambda_2 [arg_3, arg_4] = (EXC.function_clause unit)
       lambda_2 args = (EXC.badarity (ErlangFun 2 lambda_2) args)
     in lambda_2)
erlps__new__2 [arg_120, arg_121] = (EXC.function_clause unit)
erlps__new__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__singleton__2 :: ErlangFun
erlps__singleton__2 [mod_0, e_1] =
  let
    case_2 =
      (BIF.erlang__function_exported__3
         [mod_0, (ErlangAtom "singleton"), (ErlangInt (DBI.fromInt 1))])
  in
    case case_2 of
      (ErlangAtom "true") ->
        (BIF.erlang__apply__3
           [mod_0, (ErlangAtom "singleton"),
            (ErlangCons e_1 ErlangEmptyList)])
      (ErlangAtom "false") ->
        (BIF.erlang__apply__3
           [mod_0, (ErlangAtom "from_list"),
            (ErlangCons (ErlangCons e_1 ErlangEmptyList) ErlangEmptyList)])
      something_else -> (EXC.case_clause something_else)
erlps__singleton__2 [arg_18, arg_19] = (EXC.function_clause unit)
erlps__singleton__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__add_element__3 :: ErlangFun
erlps__add_element__3 [mod_0, el_1, s0_2] =
  let   
    s_10 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "add_element"),
          (ErlangCons el_1 (ErlangCons s0_2 ErlangEmptyList))])
  in let
    match_expr_18 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "is_element"),
          (ErlangCons el_1 (ErlangCons s_10 ErlangEmptyList))])
  in
    case match_expr_18 of
      (ErlangAtom "true") ->
        let
          match_expr_24 =
            (BIF.erlang__apply__3
               [mod_0, (ErlangAtom "is_empty"),
                (ErlangCons s_10 ErlangEmptyList)])
        in
          case match_expr_24 of
            (ErlangAtom "false") ->
              let
                match_expr_30 =
                  (BIF.erlang__apply__3
                     [mod_0, (ErlangAtom "is_set"),
                      (ErlangCons s_10 ErlangEmptyList)])
              in
                case match_expr_30 of
                  (ErlangAtom "true") -> s_10
                  _ -> (EXC.badmatch match_expr_30)
            _ -> (EXC.badmatch match_expr_24)
      _ -> (EXC.badmatch match_expr_18)
erlps__add_element__3 [arg_31, arg_32, arg_33] =
  (EXC.function_clause unit)
erlps__add_element__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__del_element__3 :: ErlangFun
erlps__del_element__3 [mod_0, el_1, s0_2] =
  let   
    s_10 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "del_element"),
          (ErlangCons el_1 (ErlangCons s0_2 ErlangEmptyList))])
  in let
    match_expr_18 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "is_element"),
          (ErlangCons el_1 (ErlangCons s_10 ErlangEmptyList))])
  in
    case match_expr_18 of
      (ErlangAtom "false") ->
        let
          match_expr_24 =
            (BIF.erlang__apply__3
               [mod_0, (ErlangAtom "is_set"),
                (ErlangCons s_10 ErlangEmptyList)])
        in
          case match_expr_24 of
            (ErlangAtom "true") -> s_10
            _ -> (EXC.badmatch match_expr_24)
      _ -> (EXC.badmatch match_expr_18)
erlps__del_element__3 [arg_25, arg_26, arg_27] =
  (EXC.function_clause unit)
erlps__del_element__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__intersection__4 :: ErlangFun
erlps__intersection__4 [mod_0, equal_1, s1_2, s2_3] =
  let   
    s_11 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "intersection"),
          (ErlangCons s1_2 (ErlangCons s2_3 ErlangEmptyList))])
  in let
    arg_13 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "intersection"),
          (ErlangCons s2_3 (ErlangCons s1_2 ErlangEmptyList))])
  in let
    match_expr_22 =
      (BIF.erlang__apply__2
         [equal_1, (ErlangCons s_11 (ErlangCons arg_13 ErlangEmptyList))])
  in
    case match_expr_22 of
      (ErlangAtom "true") ->
        let   
          disjoint_28 =
            (BIF.erlang__apply__3
               [mod_0, (ErlangAtom "is_empty"),
                (ErlangCons s_11 ErlangEmptyList)])
        in let
          match_expr_37 =
            (BIF.erlang__apply__3
               [mod_0, (ErlangAtom "is_disjoint"),
                (ErlangCons s1_2 (ErlangCons s2_3 ErlangEmptyList))])
        in
          case match_expr_37 of
            disjoint_36 | (disjoint_36 == disjoint_28) ->
              let
                match_expr_46 =
                  (BIF.erlang__apply__3
                     [mod_0, (ErlangAtom "is_disjoint"),
                      (ErlangCons s2_3 (ErlangCons s1_2 ErlangEmptyList))])
              in
                case match_expr_46 of
                  disjoint_45 | (disjoint_45 == disjoint_28) -> s_11
                  _ -> (EXC.badmatch match_expr_46)
            _ -> (EXC.badmatch match_expr_37)
      _ -> (EXC.badmatch match_expr_22)
erlps__intersection__4 [arg_47, arg_48, arg_49, arg_50] =
  (EXC.function_clause unit)
erlps__intersection__4 args =
  (EXC.badarity
     (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__intersection__3 :: ErlangFun
erlps__intersection__3 [mod_0, equal_1, ss_2] =
  let   
    s_8 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "intersection"),
          (ErlangCons ss_2 ErlangEmptyList)])
  in let
    head_14 =
      (BIF.do_remote_fun_call "Lists" "erlps__reverse__1" [ss_2])
  in let
    arg_10 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "intersection"),
          (ErlangCons head_14 ErlangEmptyList)])
  in let
    match_expr_18 =
      (BIF.erlang__apply__2
         [equal_1, (ErlangCons s_8 (ErlangCons arg_10 ErlangEmptyList))])
  in
    case match_expr_18 of
      (ErlangAtom "true") -> s_8
      _ -> (EXC.badmatch match_expr_18)
erlps__intersection__3 [arg_19, arg_20, arg_21] =
  (EXC.function_clause unit)
erlps__intersection__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__subtract__3 :: ErlangFun
erlps__subtract__3 [mod_0, s1_1, s2_2] =
  let   
    s_10 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "subtract"),
          (ErlangCons s1_1 (ErlangCons s2_2 ErlangEmptyList))])
  in let
    match_expr_16 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "is_set"),
          (ErlangCons s_10 ErlangEmptyList)])
  in
    case match_expr_16 of
      (ErlangAtom "true") ->
        let   
          lop_17 =
            (BIF.erlang__apply__3
               [mod_0, (ErlangAtom "size"), (ErlangCons s_10 ErlangEmptyList)])
        in let
          rop_23 =
            (BIF.erlang__apply__3
               [mod_0, (ErlangAtom "size"), (ErlangCons s1_1 ErlangEmptyList)])
        in let match_expr_29 = (BIF.erlang__op_lesserEq [lop_17, rop_23])
        in
          case match_expr_29 of
            (ErlangAtom "true") -> s_10
            _ -> (EXC.badmatch match_expr_29)
      _ -> (EXC.badmatch match_expr_16)
erlps__subtract__3 [arg_30, arg_31, arg_32] =
  (EXC.function_clause unit)
erlps__subtract__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__union__4 :: ErlangFun
erlps__union__4 [mod_0, equal_1, s1_2, s2_3] =
  let   
    s_11 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "union"),
          (ErlangCons s1_2 (ErlangCons s2_3 ErlangEmptyList))])
  in let
    arg_13 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "union"),
          (ErlangCons s2_3 (ErlangCons s1_2 ErlangEmptyList))])
  in let
    match_expr_22 =
      (BIF.erlang__apply__2
         [equal_1, (ErlangCons s_11 (ErlangCons arg_13 ErlangEmptyList))])
  in
    case match_expr_22 of
      (ErlangAtom "true") ->
        let
          match_expr_28 =
            (BIF.erlang__apply__3
               [mod_0, (ErlangAtom "is_set"),
                (ErlangCons s_11 ErlangEmptyList)])
        in
          case match_expr_28 of
            (ErlangAtom "true") -> s_11
            _ -> (EXC.badmatch match_expr_28)
      _ -> (EXC.badmatch match_expr_22)
erlps__union__4 [arg_29, arg_30, arg_31, arg_32] =
  (EXC.function_clause unit)
erlps__union__4 args =
  (EXC.badarity
     (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__union__3 :: ErlangFun
erlps__union__3 [mod_0, equal_1, ss_2] =
  let   
    s_8 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "union"), (ErlangCons ss_2 ErlangEmptyList)])
  in let
    head_14 =
      (BIF.do_remote_fun_call "Lists" "erlps__reverse__1" [ss_2])
  in let
    arg_10 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "union"),
          (ErlangCons head_14 ErlangEmptyList)])
  in let
    match_expr_18 =
      (BIF.erlang__apply__2
         [equal_1, (ErlangCons s_8 (ErlangCons arg_10 ErlangEmptyList))])
  in
    case match_expr_18 of
      (ErlangAtom "true") -> s_8
      _ -> (EXC.badmatch match_expr_18)
erlps__union__3 [arg_19, arg_20, arg_21] =
  (EXC.function_clause unit)
erlps__union__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__is_subset__4 :: ErlangFun
erlps__is_subset__4 [mod_0, equal_1, s_2, set_3] =
  let
    case_4 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "is_subset"),
          (ErlangCons s_2 (ErlangCons set_3 ErlangEmptyList))])
  in
    case case_4 of
      (ErlangAtom "false") -> (ErlangAtom "false")
      (ErlangAtom "true") ->
        let   
          case_12 =
            (BIF.erlang__apply__3
               [mod_0, (ErlangAtom "is_subset"),
                (ErlangCons set_3 (ErlangCons s_2 ErlangEmptyList))])
        in let
          _ =
            case case_12 of
              (ErlangAtom "false") -> (ErlangAtom "ok")
              (ErlangAtom "true") ->
                let
                  match_final_20_24 =
                    (BIF.erlang__apply__2
                       [equal_1,
                        (ErlangCons s_2 (ErlangCons set_3 ErlangEmptyList))])
                in
                  case match_final_20_24 of
                    (ErlangAtom "true") -> match_final_20_24
                    _ -> (EXC.badmatch match_final_20_24)
              something_else -> (EXC.case_clause something_else)
        in (ErlangAtom "true")
      something_else -> (EXC.case_clause something_else)
erlps__is_subset__4 [arg_26, arg_27, arg_28, arg_29] =
  (EXC.function_clause unit)
erlps__is_subset__4 args =
  (EXC.badarity
     (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__fold__4 :: ErlangFun
erlps__fold__4 [mod_0, f_1, a_2, s_3] =
  let
    match_expr_9 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "is_set"), (ErlangCons s_3 ErlangEmptyList)])
  in
    case match_expr_9 of
      (ErlangAtom "true") ->
        (BIF.erlang__apply__3
           [mod_0, (ErlangAtom "fold"),
            (ErlangCons f_1
               (ErlangCons a_2 (ErlangCons s_3 ErlangEmptyList)))])
      _ -> (EXC.badmatch match_expr_9)
erlps__fold__4 [arg_19, arg_20, arg_21, arg_22] =
  (EXC.function_clause unit)
erlps__fold__4 args =
  (EXC.badarity
     (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__filter__3 :: ErlangFun
erlps__filter__3 [mod_0, f_1, s_2] =
  let
    match_expr_8 =
      (BIF.erlang__apply__3
         [mod_0, (ErlangAtom "is_set"), (ErlangCons s_2 ErlangEmptyList)])
  in
    case match_expr_8 of
      (ErlangAtom "true") ->
        (BIF.erlang__apply__3
           [mod_0, (ErlangAtom "filter"),
            (ErlangCons f_1 (ErlangCons s_2 ErlangEmptyList))])
      _ -> (EXC.badmatch match_expr_8)
erlps__filter__3 [arg_16, arg_17, arg_18] =
  (EXC.function_clause unit)
erlps__filter__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)
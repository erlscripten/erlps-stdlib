module Digraph.Utils(erlps__components__1,
                     erlps__strong_components__1,
                     erlps__cyclic_strong_components__1, erlps__reachable__2,
                     erlps__reachable_neighbours__2, erlps__reaching__2,
                     erlps__reaching_neighbours__2, erlps__topsort__1,
                     erlps__is_acyclic__1, erlps__arborescence_root__1,
                     erlps__is_arborescence__1, erlps__is_tree__1,
                     erlps__loop_vertices__1, erlps__subgraph__2,
                     erlps__subgraph__3, erlps__condensation__1,
                     erlps__preorder__1, erlps__postorder__1) where
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
import Erlang.Helpers as H
import Erlang.Exception as EXC
import Erlang.Type (ErlangFun, ErlangTerm(..), weakCmp, weakEq,
                    weakNEq, weakLt, weakLeq, weakGeq, weakGt)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)


erlps__components__1 :: ErlangFun
erlps__components__1 [g_0] =
  let arg_2 = (ErlangFun 3 erlps__inout__3)
  in (erlps__forest__2 [g_0, arg_2])
erlps__components__1 [arg_3] = (EXC.function_clause unit)
erlps__components__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__strong_components__1 :: ErlangFun
erlps__strong_components__1 [g_0] =
  let    arg_2 = (ErlangFun 3 erlps__in__3)
  in let arg_3 = (erlps__revpostorder__1 [g_0])
  in (erlps__forest__3 [g_0, arg_2, arg_3])
erlps__strong_components__1 [arg_5] = (EXC.function_clause unit)
erlps__strong_components__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__cyclic_strong_components__1 :: ErlangFun
erlps__cyclic_strong_components__1 [g_0] =
  let arg_1 = (erlps__strong_components__1 [g_0])
  in (erlps__remove_singletons__3 [arg_1, g_0, ErlangEmptyList])
erlps__cyclic_strong_components__1 [arg_5] =
  (EXC.function_clause unit)
erlps__cyclic_strong_components__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__reachable__2 :: ErlangFun
erlps__reachable__2 [vs_0, g_1] | (H.isEList vs_0) =
  let    arg_4 = (ErlangFun 3 erlps__out__3)
  in let
    arg_2 =
      (erlps__forest__4 [g_1, arg_4, vs_0, (ErlangAtom "first")])
  in (BIF.do_remote_fun_call "Lists" "erlps__append__1" [arg_2])
erlps__reachable__2 [arg_7, arg_8] = (EXC.function_clause unit)
erlps__reachable__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__reachable_neighbours__2 :: ErlangFun
erlps__reachable_neighbours__2 [vs_0, g_1] | (H.isEList vs_0) =
  let    arg_4 = (ErlangFun 3 erlps__out__3)
  in let
    arg_2 =
      (erlps__forest__4 [g_1, arg_4, vs_0, (ErlangAtom "not_first")])
  in (BIF.do_remote_fun_call "Lists" "erlps__append__1" [arg_2])
erlps__reachable_neighbours__2 [arg_7, arg_8] =
  (EXC.function_clause unit)
erlps__reachable_neighbours__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__reaching__2 :: ErlangFun
erlps__reaching__2 [vs_0, g_1] | (H.isEList vs_0) =
  let    arg_4 = (ErlangFun 3 erlps__in__3)
  in let
    arg_2 =
      (erlps__forest__4 [g_1, arg_4, vs_0, (ErlangAtom "first")])
  in (BIF.do_remote_fun_call "Lists" "erlps__append__1" [arg_2])
erlps__reaching__2 [arg_7, arg_8] = (EXC.function_clause unit)
erlps__reaching__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__reaching_neighbours__2 :: ErlangFun
erlps__reaching_neighbours__2 [vs_0, g_1] | (H.isEList vs_0) =
  let    arg_4 = (ErlangFun 3 erlps__in__3)
  in let
    arg_2 =
      (erlps__forest__4 [g_1, arg_4, vs_0, (ErlangAtom "not_first")])
  in (BIF.do_remote_fun_call "Lists" "erlps__append__1" [arg_2])
erlps__reaching_neighbours__2 [arg_7, arg_8] =
  (EXC.function_clause unit)
erlps__reaching_neighbours__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__topsort__1 :: ErlangFun
erlps__topsort__1 [g_0] =
  let    l_2 = (erlps__revpostorder__1 [g_0])
  in let arg_7 = (ErlangFun 3 erlps__in__3)
  in let arg_5 = (erlps__forest__3 [g_0, arg_7, l_2])
  in let lop_4 = (BIF.erlang__length__1 [arg_5])
  in let
    arg_10 =
      (BIF.do_remote_fun_call "Digraph" "erlps__vertices__1" [g_0])
  in let rop_9 = (BIF.erlang__length__1 [arg_10])
  in let case_3 = (BIF.erlang__op_exactEq [lop_4, rop_9])
  in
    case case_3 of
      (ErlangAtom "true") -> l_2
      (ErlangAtom "false") -> (ErlangAtom "false")
      something_else -> (EXC.case_clause something_else)
erlps__topsort__1 [arg_12] = (EXC.function_clause unit)
erlps__topsort__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__is_acyclic__1 :: ErlangFun
erlps__is_acyclic__1 [g_0] =
  let    lop_2 = (erlps__loop_vertices__1 [g_0])
  in let lop_1 = (BIF.erlang__op_exactEq [lop_2, ErlangEmptyList])
  in
    case lop_1 of
      (ErlangAtom "false") -> (ErlangAtom "false")
      (ErlangAtom "true") ->
        let lop_5 = (erlps__topsort__1 [g_0])
        in (BIF.erlang__op_exactNeq [lop_5, (ErlangAtom "false")])
      _ -> (EXC.badarg1 lop_1)
erlps__is_acyclic__1 [arg_8] = (EXC.function_clause unit)
erlps__is_acyclic__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__arborescence_root__1 :: ErlangFun
erlps__arborescence_root__1 [g_0] =
  let   
    lop_2 =
      (BIF.do_remote_fun_call "Digraph" "erlps__no_edges__1" [g_0])
  in let
    lop_5 =
      (BIF.do_remote_fun_call "Digraph" "erlps__no_vertices__1" [g_0])
  in let
    rop_4 =
      (BIF.erlang__op_minus [lop_5, (ErlangInt (DBI.fromInt 1))])
  in let case_1 = (BIF.erlang__op_exactEq [lop_2, rop_4])
  in
    case case_1 of
      (ErlangAtom "true") ->
        (EXC.tryCatch
           (\ _ ->
              let   
                f_20 =
                  (ErlangFun 2
                     let
                       lambda_8 [v_11, z_12] =
                         let
                           case_13 =
                             (BIF.do_remote_fun_call "Digraph"
                                "erlps__in_degree__2" [g_0, v_11])
                         in
                           case case_13 of
                             (ErlangInt num_16) | ((ErlangInt num_16) ==
                                                     (ErlangInt
                                                        (DBI.fromInt 1))) ->
                               z_12
                             (ErlangInt num_17) | ((ErlangInt num_17) ==
                                                     (ErlangInt
                                                        (DBI.fromInt 0)))
                                                , ((==) z_12 ErlangEmptyList) ->
                               (ErlangCons v_11 ErlangEmptyList)
                             something_else -> (EXC.case_clause something_else)
                       lambda_8 [arg_9, arg_10] = (EXC.function_clause unit)
                       lambda_8 args =
                         (EXC.badarity (ErlangFun 2 lambda_8) args)
                     in lambda_8)
              in let
                arg_23 =
                  (BIF.do_remote_fun_call "Digraph" "erlps__vertices__1" [g_0])
              in let
                match_expr_26 =
                  (BIF.do_remote_fun_call "Lists" "erlps__foldl__3"
                     [f_20, ErlangEmptyList, arg_23])
              in
                case match_expr_26 of
                  (ErlangCons root_25 (ErlangEmptyList)) ->
                    (ErlangTuple [(ErlangAtom "yes"), root_25])
                  _ -> (EXC.badmatch match_expr_26))
           (\ ex_30 ->
              case ex_30 of
                (ErlangTuple [_, _, _]) -> (ErlangAtom "no")
                ex_31 -> (EXC.raise ex_31)))
      (ErlangAtom "false") -> (ErlangAtom "no")
      something_else -> (EXC.case_clause something_else)
erlps__arborescence_root__1 [arg_32] = (EXC.function_clause unit)
erlps__arborescence_root__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__is_arborescence__1 :: ErlangFun
erlps__is_arborescence__1 [g_0] =
  let lop_1 = (erlps__arborescence_root__1 [g_0])
  in (BIF.erlang__op_exactNeq [lop_1, (ErlangAtom "no")])
erlps__is_arborescence__1 [arg_4] = (EXC.function_clause unit)
erlps__is_arborescence__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__is_tree__1 :: ErlangFun
erlps__is_tree__1 [g_0] =
  let   
    lop_2 =
      (BIF.do_remote_fun_call "Digraph" "erlps__no_edges__1" [g_0])
  in let
    lop_5 =
      (BIF.do_remote_fun_call "Digraph" "erlps__no_vertices__1" [g_0])
  in let
    rop_4 =
      (BIF.erlang__op_minus [lop_5, (ErlangInt (DBI.fromInt 1))])
  in let lop_1 = (BIF.erlang__op_exactEq [lop_2, rop_4])
  in
    case lop_1 of
      (ErlangAtom "false") -> (ErlangAtom "false")
      (ErlangAtom "true") ->
        let    arg_9 = (erlps__components__1 [g_0])
        in let lop_8 = (BIF.erlang__length__1 [arg_9])
        in (BIF.erlang__op_exactEq [lop_8, (ErlangInt (DBI.fromInt 1))])
      _ -> (EXC.badarg1 lop_1)
erlps__is_tree__1 [arg_12] = (EXC.function_clause unit)
erlps__is_tree__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__loop_vertices__1 :: ErlangFun
erlps__loop_vertices__1 [g_0] =
  let
    lc_src_1 =
      (BIF.do_remote_fun_call "Digraph" "erlps__vertices__1" [g_0])
  in
    (H.flmap
       (\ lc_4 ->
          let cond_5 = (erlps__is_reflexive_vertex__2 [lc_4, g_0])
          in
            case cond_5 of
              (ErlangAtom "true") -> (ErlangCons lc_4 ErlangEmptyList)
              _ -> ErlangEmptyList)
       lc_src_1)
erlps__loop_vertices__1 [arg_9] = (EXC.function_clause unit)
erlps__loop_vertices__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__subgraph__2 :: ErlangFun
erlps__subgraph__2 [g_0, vs_1] =
  (EXC.tryCatch
     (\ _ -> (erlps__subgraph_opts__3 [g_0, vs_1, ErlangEmptyList]))
     (\ ex_6 ->
        case ex_6 of
          (ErlangTuple [(ErlangAtom "throw"), (ErlangAtom "badarg"), _]) ->
            (BIF.erlang__error__1 [(ErlangAtom "badarg")])
          ex_7 -> (EXC.raise ex_7)))
erlps__subgraph__2 [arg_9, arg_10] = (EXC.function_clause unit)
erlps__subgraph__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__subgraph__3 :: ErlangFun
erlps__subgraph__3 [g_0, vs_1, opts_2] =
  (EXC.tryCatch
     (\ _ -> (erlps__subgraph_opts__3 [g_0, vs_1, opts_2]))
     (\ ex_7 ->
        case ex_7 of
          (ErlangTuple [(ErlangAtom "throw"), (ErlangAtom "badarg"), _]) ->
            (BIF.erlang__error__1 [(ErlangAtom "badarg")])
          ex_8 -> (EXC.raise ex_8)))
erlps__subgraph__3 [arg_10, arg_11, arg_12] =
  (EXC.function_clause unit)
erlps__subgraph__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__condensation__1 :: ErlangFun
erlps__condensation__1 [g_0] =
  let    scs_2 = (erlps__strong_components__1 [g_0])
  in let
    v2i_5 =
      (BIF.do_remote_fun_call "Ets" "erlps__new__2"
         [(ErlangAtom "condensation"), ErlangEmptyList])
  in let
    i2c_8 =
      (BIF.do_remote_fun_call "Ets" "erlps__new__2"
         [(ErlangAtom "condensation"), ErlangEmptyList])
  in let
    cfun_33 =
      (ErlangFun 2
         let
           lambda_9 [sc_12, n_13] =
             let   
               arg_14 =
                 (ErlangFun 1
                    let
                      lambda_15 [v_17] =
                        let    arg_20 = (ErlangTuple [v_17, n_13])
                        in let
                          match_final_18_23 =
                            (BIF.do_remote_fun_call "Ets" "erlps__insert__2"
                               [v2i_5, arg_20])
                        in
                          case match_final_18_23 of
                            (ErlangAtom "true") -> match_final_18_23
                            _ -> (EXC.badmatch match_final_18_23)
                      lambda_15 [arg_16] = (EXC.function_clause unit)
                      lambda_15 args =
                        (EXC.badarity (ErlangFun 1 lambda_15) args)
                    in lambda_15)
             in let
               _ =
                 (BIF.do_remote_fun_call "Lists" "erlps__foreach__2"
                    [arg_14, sc_12])
             in let arg_27 = (ErlangTuple [n_13, sc_12])
             in let
               match_expr_30 =
                 (BIF.do_remote_fun_call "Ets" "erlps__insert__2"
                    [i2c_8, arg_27])
             in
               case match_expr_30 of
                 (ErlangAtom "true") ->
                   (BIF.erlang__op_plus [n_13, (ErlangInt (DBI.fromInt 1))])
                 _ -> (EXC.badmatch match_expr_30)
           lambda_9 [arg_10, arg_11] = (EXC.function_clause unit)
           lambda_9 args = (EXC.badarity (ErlangFun 2 lambda_9) args)
         in lambda_9)
  in let
    _ =
      (BIF.do_remote_fun_call "Lists" "erlps__foldl__3"
         [cfun_33, (ErlangInt (DBI.fromInt 1)), scs_2])
  in let
    scg_40 =
      (erlps__subgraph_opts__3 [g_0, ErlangEmptyList, ErlangEmptyList])
  in let
    arg_41 =
      (ErlangFun 1
         let
           lambda_42 [sc_44] =
             (erlps__condense__5 [sc_44, g_0, scg_40, v2i_5, i2c_8])
           lambda_42 [arg_43] = (EXC.function_clause unit)
           lambda_42 args = (EXC.badarity (ErlangFun 1 lambda_42) args)
         in lambda_42)
  in let
    _ =
      (BIF.do_remote_fun_call "Lists" "erlps__foreach__2"
         [arg_41, scs_2])
  in let
    _ = (BIF.do_remote_fun_call "Ets" "erlps__delete__1" [v2i_5])
  in let
    _ = (BIF.do_remote_fun_call "Ets" "erlps__delete__1" [i2c_8])
  in scg_40
erlps__condensation__1 [arg_53] = (EXC.function_clause unit)
erlps__condensation__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__preorder__1 :: ErlangFun
erlps__preorder__1 [g_0] =
  let arg_1 = (erlps__revpreorder__1 [g_0])
  in (BIF.do_remote_fun_call "Lists" "erlps__reverse__1" [arg_1])
erlps__preorder__1 [arg_3] = (EXC.function_clause unit)
erlps__preorder__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__postorder__1 :: ErlangFun
erlps__postorder__1 [g_0] =
  let arg_1 = (erlps__revpostorder__1 [g_0])
  in (BIF.do_remote_fun_call "Lists" "erlps__reverse__1" [arg_1])
erlps__postorder__1 [arg_3] = (EXC.function_clause unit)
erlps__postorder__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__forest__2 :: ErlangFun
erlps__forest__2 [g_0, sf_1] =
  let
    arg_4 =
      (BIF.do_remote_fun_call "Digraph" "erlps__vertices__1" [g_0])
  in (erlps__forest__3 [g_0, sf_1, arg_4])
erlps__forest__2 [arg_6, arg_7] = (EXC.function_clause unit)
erlps__forest__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__forest__3 :: ErlangFun
erlps__forest__3 [g_0, sf_1, vs_2] =
  (erlps__forest__4 [g_0, sf_1, vs_2, (ErlangAtom "first")])
erlps__forest__3 [arg_7, arg_8, arg_9] =
  (EXC.function_clause unit)
erlps__forest__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__forest__4 :: ErlangFun
erlps__forest__4 [g_0, sf_1, vs_2, handlefirst_3] =
  let   
    t_8 =
      (BIF.do_remote_fun_call "Ets" "erlps__new__2"
         [(ErlangAtom "forest"),
          (ErlangCons (ErlangAtom "set") ErlangEmptyList)])
  in let
    f_20 =
      (ErlangFun 2
         let
           lambda_9 [v_12, ll_13] =
             (erlps__pretraverse__6
                [handlefirst_3, v_12, sf_1, g_0, t_8, ll_13])
           lambda_9 [arg_10, arg_11] = (EXC.function_clause unit)
           lambda_9 args = (EXC.badarity (ErlangFun 2 lambda_9) args)
         in lambda_9)
  in let
    ll_24 =
      (BIF.do_remote_fun_call "Lists" "erlps__foldl__3"
         [f_20, ErlangEmptyList, vs_2])
  in let
    _ = (BIF.do_remote_fun_call "Ets" "erlps__delete__1" [t_8])
  in ll_24
erlps__forest__4 [arg_26, arg_27, arg_28, arg_29] =
  (EXC.function_clause unit)
erlps__forest__4 args =
  (EXC.badarity
     (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__pretraverse__6 :: ErlangFun
erlps__pretraverse__6 [(ErlangAtom "first"), v_0, sf_1, g_2, t_3,
                       ll_4]
  =
  (erlps__ptraverse__6
     [(ErlangCons v_0 ErlangEmptyList), sf_1, g_2, t_3,
      ErlangEmptyList, ll_4])
erlps__pretraverse__6 [(ErlangAtom "not_first"), v_0, sf_1, g_2,
                       t_3, ll_4]
  =
  let
    case_5 =
      (BIF.do_remote_fun_call "Ets" "erlps__member__2" [t_3, v_0])
  in
    case case_5 of
      (ErlangAtom "false") ->
        let
          arg_8 =
            (BIF.erlang__apply__2
               [sf_1,
                (ErlangCons g_2
                   (ErlangCons v_0
                      (ErlangCons ErlangEmptyList ErlangEmptyList)))])
        in
          (erlps__ptraverse__6
             [arg_8, sf_1, g_2, t_3, ErlangEmptyList, ll_4])
      (ErlangAtom "true") -> ll_4
      something_else -> (EXC.case_clause something_else)
erlps__pretraverse__6 [arg_18, arg_19, arg_20, arg_21, arg_22,
                       arg_23]
  =
  (EXC.function_clause unit)
erlps__pretraverse__6 args =
  (EXC.badarity
     (ErlangFun 6 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__ptraverse__6 :: ErlangFun
erlps__ptraverse__6 [(ErlangCons v_0 vs_1), sf_2, g_3, t_4, rs_5,
                     ll_6]
  =
  let
    case_7 =
      (BIF.do_remote_fun_call "Ets" "erlps__member__2" [t_4, v_0])
  in
    case case_7 of
      (ErlangAtom "false") ->
        let    arg_11 = (ErlangTuple [v_0])
        in let
          _ =
            (BIF.do_remote_fun_call "Ets" "erlps__insert__2" [t_4, arg_11])
        in let
          arg_13 =
            (BIF.erlang__apply__2
               [sf_2,
                (ErlangCons g_3
                   (ErlangCons v_0 (ErlangCons vs_1 ErlangEmptyList)))])
        in
          (erlps__ptraverse__6
             [arg_13, sf_2, g_3, t_4, (ErlangCons v_0 rs_5), ll_6])
      (ErlangAtom "true") ->
        (erlps__ptraverse__6 [vs_1, sf_2, g_3, t_4, rs_5, ll_6])
      something_else -> (EXC.case_clause something_else)
erlps__ptraverse__6 [(ErlangEmptyList), _sf_0, _g_1, _t_2,
                     (ErlangEmptyList), ll_3]
  =
  ll_3
erlps__ptraverse__6 [(ErlangEmptyList), _sf_0, _g_1, _t_2, rs_3,
                     ll_4]
  =
  (ErlangCons rs_3 ll_4)
erlps__ptraverse__6 [arg_7, arg_8, arg_9, arg_10, arg_11, arg_12]
  =
  (EXC.function_clause unit)
erlps__ptraverse__6 args =
  (EXC.badarity
     (ErlangFun 6 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__revpreorder__1 :: ErlangFun
erlps__revpreorder__1 [g_0] =
  let    arg_3 = (ErlangFun 3 erlps__out__3)
  in let arg_1 = (erlps__forest__2 [g_0, arg_3])
  in (BIF.do_remote_fun_call "Lists" "erlps__append__1" [arg_1])
erlps__revpreorder__1 [arg_4] = (EXC.function_clause unit)
erlps__revpreorder__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__revpostorder__1 :: ErlangFun
erlps__revpostorder__1 [g_0] =
  let   
    t_5 =
      (BIF.do_remote_fun_call "Ets" "erlps__new__2"
         [(ErlangAtom "forest"),
          (ErlangCons (ErlangAtom "set") ErlangEmptyList)])
  in let
    arg_6 =
      (BIF.do_remote_fun_call "Digraph" "erlps__vertices__1" [g_0])
  in let
    l_11 =
      (erlps__posttraverse__4 [arg_6, g_0, t_5, ErlangEmptyList])
  in let
    _ = (BIF.do_remote_fun_call "Ets" "erlps__delete__1" [t_5])
  in l_11
erlps__revpostorder__1 [arg_13] = (EXC.function_clause unit)
erlps__revpostorder__1 args =
  (EXC.badarity
     (ErlangFun 1 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__posttraverse__4 :: ErlangFun
erlps__posttraverse__4 [(ErlangCons v_0 vs_1), g_2, t_3, l_4] =
  let   
    case_5 =
      (BIF.do_remote_fun_call "Ets" "erlps__member__2" [t_3, v_0])
  in let
    l1_20 =
      case case_5 of
        (ErlangAtom "false") ->
          let    arg_9 = (ErlangTuple [v_0])
          in let
            _ =
              (BIF.do_remote_fun_call "Ets" "erlps__insert__2" [t_3, arg_9])
          in let arg_13 = (erlps__out__3 [g_2, v_0, ErlangEmptyList])
          in let tail_12 = (erlps__posttraverse__4 [arg_13, g_2, t_3, l_4])
          in (ErlangCons v_0 tail_12)
        (ErlangAtom "true") -> l_4
        something_else -> (EXC.case_clause something_else)
  in (erlps__posttraverse__4 [vs_1, g_2, t_3, l1_20])
erlps__posttraverse__4 [(ErlangEmptyList), _g_0, _t_1, l_2] = l_2
erlps__posttraverse__4 [arg_3, arg_4, arg_5, arg_6] =
  (EXC.function_clause unit)
erlps__posttraverse__4 args =
  (EXC.badarity
     (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__in__3 :: ErlangFun
erlps__in__3 [g_0, v_1, vs_2] =
  let
    lop_3 =
      (BIF.do_remote_fun_call "Digraph" "erlps__in_neighbours__2"
         [g_0, v_1])
  in (BIF.erlang__op_append [lop_3, vs_2])
erlps__in__3 [arg_7, arg_8, arg_9] = (EXC.function_clause unit)
erlps__in__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__out__3 :: ErlangFun
erlps__out__3 [g_0, v_1, vs_2] =
  let
    lop_3 =
      (BIF.do_remote_fun_call "Digraph" "erlps__out_neighbours__2"
         [g_0, v_1])
  in (BIF.erlang__op_append [lop_3, vs_2])
erlps__out__3 [arg_7, arg_8, arg_9] = (EXC.function_clause unit)
erlps__out__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__inout__3 :: ErlangFun
erlps__inout__3 [g_0, v_1, vs_2] =
  let arg_5 = (erlps__out__3 [g_0, v_1, vs_2])
  in (erlps__in__3 [g_0, v_1, arg_5])
erlps__inout__3 [arg_9, arg_10, arg_11] =
  (EXC.function_clause unit)
erlps__inout__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__remove_singletons__3 :: ErlangFun
erlps__remove_singletons__3 [(ErlangCons c_1@(ErlangCons v_0 (ErlangEmptyList)) cs_2),
                             g_3, l_4]
  =
  let case_5 = (erlps__is_reflexive_vertex__2 [v_0, g_3])
  in
    case case_5 of
      (ErlangAtom "true") ->
        (erlps__remove_singletons__3 [cs_2, g_3, (ErlangCons c_1 l_4)])
      (ErlangAtom "false") ->
        (erlps__remove_singletons__3 [cs_2, g_3, l_4])
      something_else -> (EXC.case_clause something_else)
erlps__remove_singletons__3 [(ErlangCons c_0 cs_1), g_2, l_3] =
  (erlps__remove_singletons__3 [cs_1, g_2, (ErlangCons c_0 l_3)])
erlps__remove_singletons__3 [(ErlangEmptyList), _g_0, l_1] = l_1
erlps__remove_singletons__3 [arg_2, arg_3, arg_4] =
  (EXC.function_clause unit)
erlps__remove_singletons__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__is_reflexive_vertex__2 :: ErlangFun
erlps__is_reflexive_vertex__2 [v_0, g_1] =
  let
    arg_3 =
      (BIF.do_remote_fun_call "Digraph" "erlps__out_neighbours__2"
         [g_1, v_0])
  in (BIF.lists__member__2 [v_0, arg_3])
erlps__is_reflexive_vertex__2 [arg_6, arg_7] =
  (EXC.function_clause unit)
erlps__is_reflexive_vertex__2 args =
  (EXC.badarity
     (ErlangFun 2 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__subgraph_opts__3 :: ErlangFun
erlps__subgraph_opts__3 [g_0, vs_1, opts_2] =
  (erlps__subgraph_opts__5
     [opts_2, (ErlangAtom "inherit"), (ErlangAtom "true"), g_0, vs_1])
erlps__subgraph_opts__3 [arg_8, arg_9, arg_10] =
  (EXC.function_clause unit)
erlps__subgraph_opts__3 args =
  (EXC.badarity
     (ErlangFun 3 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__subgraph_opts__5 :: ErlangFun
erlps__subgraph_opts__5 [(ErlangCons (ErlangTuple [(ErlangAtom "type"),
                                                   type_0]) opts_1),
                         _type0_2, keep_3, g_4, vs_5]
  | ((ErlangAtom "true") ==
       (H.falsifyErrors
          (\ _ ->
             let
               lop_11 =
                 (BIF.erlang__op_exactEq [type_0, (ErlangAtom "inherit")])
             in
               case lop_11 of
                 (ErlangAtom "true") -> (ErlangAtom "true")
                 (ErlangAtom "false") -> (BIF.erlang__is_list__1 [type_0])
                 _ -> (EXC.badarg1 lop_11)))) =
  (erlps__subgraph_opts__5 [opts_1, type_0, keep_3, g_4, vs_5])
erlps__subgraph_opts__5 [(ErlangCons (ErlangTuple [(ErlangAtom "keep_labels"),
                                                   keep_0]) opts_1),
                         type_2, _keep0_3, g_4, vs_5]
  | ((ErlangAtom "true") ==
       (H.falsifyErrors
          (\ _ -> (BIF.erlang__is_boolean__1 [keep_0])))) =
  (erlps__subgraph_opts__5 [opts_1, type_2, keep_0, g_4, vs_5])
erlps__subgraph_opts__5 [(ErlangEmptyList),
                         (ErlangAtom "inherit"), keep_0, g_1, vs_2]
  =
  let   
    info_4 =
      (BIF.do_remote_fun_call "Digraph" "erlps__info__1" [g_1])
  in let
    match_expr_9 =
      (BIF.lists__keysearch__3
         [(ErlangAtom "cyclicity"), (ErlangInt (DBI.fromInt 1)), info_4])
  in
    case match_expr_9 of
      (ErlangTuple [_, (ErlangTuple [_, cyclicity_8])]) ->
        let
          match_expr_14 =
            (BIF.lists__keysearch__3
               [(ErlangAtom "protection"), (ErlangInt (DBI.fromInt 1)), info_4])
        in
          case match_expr_14 of
            (ErlangTuple [_, (ErlangTuple [_, protection_13])]) ->
              (erlps__subgraph__4
                 [g_1, vs_2,
                  (ErlangCons cyclicity_8
                     (ErlangCons protection_13 ErlangEmptyList)),
                  keep_0])
            _ -> (EXC.badmatch match_expr_14)
      _ -> (EXC.badmatch match_expr_9)
erlps__subgraph_opts__5 [(ErlangEmptyList), type_0, keep_1, g_2,
                         vs_3]
  =
  (erlps__subgraph__4 [g_2, vs_3, type_0, keep_1])
erlps__subgraph_opts__5 [_, _type_0, _keep_1, _g_2, _vs_3] =
  (BIF.erlang__throw__1 [(ErlangAtom "badarg")])
erlps__subgraph_opts__5 [arg_5, arg_6, arg_7, arg_8, arg_9] =
  (EXC.function_clause unit)
erlps__subgraph_opts__5 args =
  (EXC.badarity
     (ErlangFun 5 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__subgraph__4 :: ErlangFun
erlps__subgraph__4 [g_0, vs_1, type_2, keep_3] =
  (EXC.tryOfCatch
     (\ _ ->
        (BIF.do_remote_fun_call "Digraph" "erlps__new__1" [type_2]))
     (\ of_5 ->
        let   
          arg_9 =
            (ErlangFun 1
               let
                 lambda_10 [v_12] =
                   (erlps__subgraph_vertex__4 [v_12, g_0, of_5, keep_3])
                 lambda_10 [arg_11] = (EXC.function_clause unit)
                 lambda_10 args = (EXC.badarity (ErlangFun 1 lambda_10) args)
               in lambda_10)
        in let
          _ =
            (BIF.do_remote_fun_call "Lists" "erlps__foreach__2"
               [arg_9, vs_1])
        in let
          efun_32 =
            (ErlangFun 1
               let
                 lambda_18 [v_20] =
                   let   
                     arg_21 =
                       (ErlangFun 1
                          let
                            lambda_22 [e_24] =
                              (erlps__subgraph_edge__4
                                 [e_24, g_0, of_5, keep_3])
                            lambda_22 [arg_23] = (EXC.function_clause unit)
                            lambda_22 args =
                              (EXC.badarity (ErlangFun 1 lambda_22) args)
                          in lambda_22)
                   in let
                     arg_29 =
                       (BIF.do_remote_fun_call "Digraph" "erlps__out_edges__2"
                          [g_0, v_20])
                   in
                     (BIF.do_remote_fun_call "Lists" "erlps__foreach__2"
                        [arg_21, arg_29])
                 lambda_18 [arg_19] = (EXC.function_clause unit)
                 lambda_18 args = (EXC.badarity (ErlangFun 1 lambda_18) args)
               in lambda_18)
        in let
          arg_34 =
            (BIF.do_remote_fun_call "Digraph" "erlps__vertices__1" [of_5])
        in let
          _ =
            (BIF.do_remote_fun_call "Lists" "erlps__foreach__2"
               [efun_32, arg_34])
        in of_5)
     (\ ex_6 ->
        case ex_6 of
          (ErlangTuple [(ErlangAtom "error"), (ErlangAtom "badarg"), _]) ->
            (BIF.erlang__throw__1 [(ErlangAtom "badarg")])
          ex_7 -> (EXC.raise ex_7)))
erlps__subgraph__4 [arg_37, arg_38, arg_39, arg_40] =
  (EXC.function_clause unit)
erlps__subgraph__4 args =
  (EXC.badarity
     (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__subgraph_vertex__4 :: ErlangFun
erlps__subgraph_vertex__4 [v_0, g_1, sg_2, keep_3] =
  let
    case_4 =
      (BIF.do_remote_fun_call "Digraph" "erlps__vertex__2" [g_1, v_0])
  in
    case case_4 of
      (ErlangAtom "false") -> (ErlangAtom "ok")
      _ | ((ErlangAtom "true") ==
             (H.falsifyErrors (\ _ -> (BIF.erlang__not__1 [keep_3])))) ->
        (BIF.do_remote_fun_call "Digraph" "erlps__add_vertex__2"
           [sg_2, v_0])
      (ErlangTuple [_v_10, label_11]) | ((ErlangAtom "true") ==
                                           (H.falsifyErrors (\ _ -> keep_3))) ->
        (BIF.do_remote_fun_call "Digraph" "erlps__add_vertex__3"
           [sg_2, v_0, label_11])
      something_else -> (EXC.case_clause something_else)
erlps__subgraph_vertex__4 [arg_15, arg_16, arg_17, arg_18] =
  (EXC.function_clause unit)
erlps__subgraph_vertex__4 args =
  (EXC.badarity
     (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__subgraph_edge__4 :: ErlangFun
erlps__subgraph_edge__4 [e_0, g_1, sg_2, keep_3] =
  let
    match_expr_10 =
      (BIF.do_remote_fun_call "Digraph" "erlps__edge__2" [g_1, e_0])
  in
    case match_expr_10 of
      (ErlangTuple [_e_6, v1_7, v2_8, label_9]) ->
        let
          case_11 =
            (BIF.do_remote_fun_call "Digraph" "erlps__vertex__2"
               [sg_2, v2_8])
        in
          case case_11 of
            (ErlangAtom "false") -> (ErlangAtom "ok")
            _ | ((ErlangAtom "true") ==
                   (H.falsifyErrors (\ _ -> (BIF.erlang__not__1 [keep_3])))) ->
              (BIF.do_remote_fun_call "Digraph" "erlps__add_edge__5"
                 [sg_2, e_0, v1_7, v2_8, ErlangEmptyList])
            _ | ((ErlangAtom "true") == (H.falsifyErrors (\ _ -> keep_3))) ->
              (BIF.do_remote_fun_call "Digraph" "erlps__add_edge__5"
                 [sg_2, e_0, v1_7, v2_8, label_9])
            something_else -> (EXC.case_clause something_else)
      _ -> (EXC.badmatch match_expr_10)
erlps__subgraph_edge__4 [arg_25, arg_26, arg_27, arg_28] =
  (EXC.function_clause unit)
erlps__subgraph_edge__4 args =
  (EXC.badarity
     (ErlangFun 4 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__condense__5 :: ErlangFun
erlps__condense__5 [sc_0, g_1, scg_2, v2i_3, i2c_4] =
  let   
    t_7 =
      (BIF.do_remote_fun_call "Ets" "erlps__new__2"
         [(ErlangAtom "condense"), ErlangEmptyList])
  in let
    nfun_19 =
      (ErlangFun 1
         let
           lambda_8 [neighbour_10] =
             let
               match_expr_15 =
                 (BIF.do_remote_fun_call "Ets" "erlps__lookup__2"
                    [v2i_3, neighbour_10])
             in
               case match_expr_15 of
                 (ErlangCons (ErlangTuple [_v_13, i_14]) (ErlangEmptyList)) ->
                   let arg_17 = (ErlangTuple [i_14])
                   in
                     (BIF.do_remote_fun_call "Ets" "erlps__insert__2"
                        [t_7, arg_17])
                 _ -> (EXC.badmatch match_expr_15)
           lambda_8 [arg_9] = (EXC.function_clause unit)
           lambda_8 args = (EXC.badarity (ErlangFun 1 lambda_8) args)
         in lambda_8)
  in let
    vfun_27 =
      (ErlangFun 1
         let
           lambda_20 [v_22] =
             let
               arg_24 =
                 (BIF.do_remote_fun_call "Digraph" "erlps__out_neighbours__2"
                    [g_1, v_22])
             in
               (BIF.do_remote_fun_call "Lists" "erlps__foreach__2"
                  [nfun_19, arg_24])
           lambda_20 [arg_21] = (EXC.function_clause unit)
           lambda_20 args = (EXC.badarity (ErlangFun 1 lambda_20) args)
         in lambda_20)
  in let
    _ =
      (BIF.do_remote_fun_call "Lists" "erlps__foreach__2"
         [vfun_27, sc_0])
  in let
    _ =
      (BIF.do_remote_fun_call "Digraph" "erlps__add_vertex__2"
         [scg_2, sc_0])
  in let
    arg_32 = (BIF.do_remote_fun_call "Ets" "erlps__first__1" [t_7])
  in let
    _ = (erlps__condense__6 [arg_32, t_7, sc_0, g_1, scg_2, i2c_4])
  in (BIF.do_remote_fun_call "Ets" "erlps__delete__1" [t_7])
erlps__condense__5 [arg_40, arg_41, arg_42, arg_43, arg_44] =
  (EXC.function_clause unit)
erlps__condense__5 args =
  (EXC.badarity
     (ErlangFun 5 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)

erlps__condense__6 :: ErlangFun
erlps__condense__6 [(ErlangAtom "$end_of_table"), _t_0, _sc_1,
                    _g_2, _scg_3, _i2c_4]
  =
  (ErlangAtom "ok")
erlps__condense__6 [i_0, t_1, sc_2, g_3, scg_4, i2c_5] =
  let
    match_expr_9 =
      (BIF.do_remote_fun_call "Ets" "erlps__lookup__2" [i2c_5, i_0])
  in
    case match_expr_9 of
      (ErlangCons (ErlangTuple [_, c_8]) (ErlangEmptyList)) ->
        let   
          _ =
            (BIF.do_remote_fun_call "Digraph" "erlps__add_vertex__2"
               [scg_4, c_8])
        in let cond_12 = (BIF.erlang__op_exactNeq [c_8, sc_2])
        in let
          match_expr_19 =
            case cond_12 of
              (ErlangAtom "true") ->
                let
                  lc_ret_15 =
                    (BIF.do_remote_fun_call "Digraph" "erlps__add_edge__3"
                       [scg_4, sc_2, c_8])
                in (ErlangCons lc_ret_15 ErlangEmptyList)
              _ -> ErlangEmptyList
        in let
          arg_20 =
            (BIF.do_remote_fun_call "Ets" "erlps__next__2" [t_1, i_0])
        in (erlps__condense__6 [arg_20, t_1, sc_2, g_3, scg_4, i2c_5])
      _ -> (EXC.badmatch match_expr_9)
erlps__condense__6 [arg_28, arg_29, arg_30, arg_31, arg_32,
                    arg_33]
  =
  (EXC.function_clause unit)
erlps__condense__6 args =
  (EXC.badarity
     (ErlangFun 6 (\ _ -> (ErlangAtom "purs_tco_sucks"))) args)
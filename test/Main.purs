module Test.Main where

import Prelude

import Effect.Aff.AVar as AVar
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Unsafe
import Effect.Ref as Ref
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff hiding (error)
import Effect.Exception(catchException)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, expectError)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.String.CodePoints as StrCP
import Data.String as Str
import Unsafe.Coerce

import Data.Time.Duration
import Data.Lazy
import Data.Either
import Data.Tuple as T
import Data.Array as A
import Data.Maybe as M
import Partial.Unsafe
import Erlang.Type
import Erlang.Exception
import Erlang.Builtins as BIF
import Data.BigInt as DBI
import Erlang.Invoke
import Erlang.TestUtil

import Lists
import Array.SUITE
import Maps.SUITE
import Lists.SUITE as LS
import Queue.SUITE as QS
import Dict.SUITE as DS
import Proplists.SUITE as PropS
import Sets.SUITE as SetsS
import Math.SUITE as MathS
import Sofs.SUITE as SofsS
import Erl.Scan.SUITE as ESS
import Erl.Anno.SUITE as EAS
import Erl.Eval.SUITE as EES
import Ets.Tests as ETS
import Io.Lib as IOLIB

test_reverse :: forall e. ToErlang e => Array e -> Aff Unit
test_reverse a = do
    let input = toErl a
    let output = makeOk $ toErl (A.reverse a)
    res <- exec erlps__reverse__1 [input]
    output `shouldEqual` res

test_sort :: forall e. Ord e => ToErlang e => Array e -> Aff Unit
test_sort a = do
    let input = toErl a
    let output = makeOk $ toErl (A.sort a)
    res <- exec erlps__sort__1 [input]
    output `shouldEqual` res

test_map :: forall e. ToErlang e => Array e -> ErlangTerm -> (e -> e) -> Aff Unit
test_map a ef pf = do
    let input = toErl a
    let output = makeOk $ toErl (map pf a)
    res <- exec erlps__map__2 [ef, input]
    output `shouldEqual` res

test_zip_ok :: forall e. ToErlang e => Array e -> Array e -> Aff Unit
test_zip_ok a b = do
    let input_a = toErl a
    let input_b = toErl b
    let output = makeOk $ toErl $ map (\(T.Tuple x y) -> ErlangTuple [x,y]) (A.zip (map toErl a) (map toErl b))
    res <- exec erlps__zip__2 [input_a, input_b]
    output `shouldEqual` res

test_zip_fail :: forall e. ToErlang e => Array e -> Array e -> Aff Unit
test_zip_fail a b = do
    let input_a = toErl a
    let input_b = toErl b
    res <- exec erlps__zip__2 [input_a, input_b]
    err `shouldEqual` res

test_seq :: forall e. ToErlang e => e -> e -> Array e -> Aff Unit
test_seq from to expected = do
    calc <- exec erlps__seq__2 [toErl from, toErl to]
    let out = makeOk $ toErl expected
    out `shouldEqual` calc

test_iolib_format :: String -> Array ErlangTerm -> Aff Unit
test_iolib_format expected args = do
    calc <- exec IOLIB.erlps__format__2 args
    nested <- unpackOk calc
    testExecOk (toErl expected) erlps__flatten__1 [nested]

main :: Effect Unit
main =
    launchAff_ $ runSpec [consoleReporter] do

    describe "Sanity check" do
        it "one should equal one" do
            1 `shouldEqual` 1
        it "two should equal two" do
            2 `shouldEqual` 2

    describe "ETS" do
      it "set table" do
        testExecOk ok ETS.erlps__test_set__0 []
      it "ordered set table" do
        testExecOk ok ETS.erlps__test_ordered_set__0 []
      it "bag table" do
        testExecOk ok ETS.erlps__test_bag__0 []

    describe "io_lib_format" do
      it "no terms" do
        test_iolib_format "" [toErl "", ErlangEmptyList]
        test_iolib_format "asdf" [toErl "asdf", ErlangEmptyList]
        test_iolib_format "asdf 1337" [toErl "asdf 1337", ErlangEmptyList]
      it "~p works" do
        test_iolib_format "test: []" [toErl "test: ~p", toErl [ErlangEmptyList]]
        test_iolib_format "test: \"asdf\"" [toErl "test: ~p", toErl ["asdf"]]
        test_iolib_format "test: 1337" [toErl "test: ~p", toErl [1337]]
        test_iolib_format "test: {1,2,\"asdf\"}" [toErl "test: ~p", toErl [ErlangTuple [toErl 1, toErl 2, toErl "asdf"]]]
        test_iolib_format "test: <<>>" [toErl "test: ~p", toErl [bin []]]
        -- Works but output different than in erlang
        --test_iolib_format "test: <<1,2,3,4>>" [toErl "test: ~p", toErl [bin [1,2,3,4]]]
      it "~s works" do
        test_iolib_format "test:  " [toErl "test: ~s", toErl ""]
        test_iolib_format "test: asdf" [toErl "test: ~s", toErl "asdf"]
        test_iolib_format "test: asdf" [toErl "test: ~s", ErlangAtom "asdf"]

    describe "STDLIB Lists" do
        it "reverse/1" do
            test_reverse [1,2,3,4,5,6,7,8,9,10]
            test_reverse [1]
            test_reverse ([] :: Array Int)
            test_reverse [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
        it "sort/1" do
            test_sort [1,2,3,4,5,6,7,8,9,10]
            test_sort [10,9,8,7,6,5,4,3,2,1]
            test_sort [5,3,34,6,2,5,7565,4,3,7,8,5,3]
        it "map/1" do
            test_map [1,2,3,4,5]
              (ErlangFun 1 (unsafePartial $ \ [ErlangInt a] -> ErlangInt (a* DBI.fromInt(20)))) (\x -> x*20)
            test_map [1,2,3,4,5]
              (ErlangFun 1 (unsafePartial $ \ [ErlangInt a] -> ErlangInt (-a))) (\x -> -x)
        it "zip/2" do
            test_zip_ok [1,2,3,4] [4,3,2,1]
            test_zip_ok [1,2,7,4] [1,3,2,1]
            test_zip_fail [1] [1,2]
            test_zip_fail [1,2] [1]
        it "seq/2" do
            test_seq 0 0 [0]
            test_seq 1 0 ([] :: Array Int)
            test_seq 1 10 [1,2,3,4,5,6,7,8,9,10]

    describe "Real Array tests taken from OTP - array_SUITE.erl" do
        it "new_test" do
            testExecOk ok erlps__new_test__1 [ErlangEmptyList]
        it "fix_test" do
            testExecOk ok erlps__fix_test__1 [ErlangEmptyList]
        it "relax_test" do
            testExecOk ok erlps__relax_test__1 [ErlangEmptyList]
        it "resize_test" do
            testExecOk ok erlps__resize_test__1 [ErlangEmptyList]
        it "set_get_test" do
            testExecOk ok erlps__set_get_test__1 [ErlangEmptyList]
        it "to_list_test" do
            testExecOk ok erlps__to_list_test__1 [ErlangEmptyList]
        it "sparse_to_list_test" do
            testExecOk ok erlps__sparse_to_list_test__1 [ErlangEmptyList]
        it "from_list_test" do
            testExecOk ok erlps__from_list_test__1 [ErlangEmptyList]
        it "to_orddict_test" do
            testExecOk ok erlps__to_orddict_test__1 [ErlangEmptyList]
        it "sparse_to_orddict_test" do
            testExecOk ok erlps__sparse_to_orddict_test__1 [ErlangEmptyList]
        it "from_orddict_test" do
            testExecOk ok erlps__from_orddict_test__1 [ErlangEmptyList]
        it "map_test" do
            testExecOk ok erlps__map_test__1 [ErlangEmptyList]
        it "sparse_map_test" do
            testExecOk ok erlps__sparse_map_test__1 [ErlangEmptyList]
        it "foldl_test" do
            testExecOk ok erlps__foldl_test__1 [ErlangEmptyList]
        it "sparse_foldl_test" do
            testExecOk ok erlps__sparse_foldl_test__1 [ErlangEmptyList]
        it "foldr_test" do
            testExecOk ok erlps__foldr_test__1 [ErlangEmptyList]
        it "sparse_foldr_test" do
            testExecOk ok erlps__sparse_foldr_test__1 [ErlangEmptyList]

    describe "Real Maps tests taken from OTP - maps_SUITE.erl" do
        it "t_update_with_3" do
            testExecOk ok erlps__t_update_with_3__1 [ErlangEmptyList]
        it "t_update_with_4" do
            testExecOk ok erlps__t_update_with_4__1 [ErlangEmptyList]
        it "t_get_3" do
            testExecOk ok erlps__t_get_3__1 [ErlangEmptyList]
        it "t_filter_2" do
            testExecOk ok erlps__t_filter_2__1 [ErlangEmptyList]
        it "t_fold_3" do
            testExecOk ok erlps__t_fold_3__1 [ErlangEmptyList]
        it "t_map_2" do
            testExecOk ok erlps__t_map_2__1 [ErlangEmptyList]
        it "t_size_1" do
            testExecOk ok erlps__t_size_1__1 [ErlangEmptyList]
        it "t_iterator_1" do
            testExecOk ok erlps__t_iterator_1__1 [ErlangEmptyList]
        it "t_put_opt" do
            testExecOk ok erlps__t_put_opt__1 [ErlangEmptyList]
        it "t_merge_opt" do
            testExecOk ok erlps__t_merge_opt__1 [ErlangEmptyList]
        it "t_with_2" do
            testExecOk ok erlps__t_with_2__1 [ErlangEmptyList]
        it "t_without_2" do
            testExecOk ok erlps__t_without_2__1 [ErlangEmptyList]
        -- Those functions were added to OTP only a week ago...
        -- https://github.com/erlang/otp/commit/2736731d1c2acf17c216aae89e160a58d035d081
        --it "t_intersect" do
        --    r <- exec erlps__t_intersect__1 [ErlangEmptyList]
        --    makeOk ok `shouldEqual` r
        --it "t_intersect_with" do
        --    r <- exec erlps__t_intersect_with__1 [ErlangEmptyList]
        --    makeOk ok `shouldEqual` r
        --it "t_merge_with" do
        --    r <- exec erlps__t_merge_with__1 [ErlangEmptyList]
        --    makeOk ok `shouldEqual` r

    describe "Real Lists tests taken from OTP - lists_SUITE.erl" do
        it "append_1" do
            testExecOk ok LS.erlps__append_1__1 [ErlangEmptyList]
        it "append_2" do
            testExecOk ok LS.erlps__append_2__1 [ErlangEmptyList]

        it "umerge" do
            testExecOk ok LS.erlps__umerge__1 [ErlangEmptyList]
        it "rumerge" do
            testExecOk ok LS.erlps__rumerge__1 [ErlangEmptyList]
        it "usort_1" do
            testExecOk ok LS.erlps__usort_1__1 [ErlangEmptyList]
        it "usort_rand" do
            testExecOk ok LS.erlps__usort_rand__1 [ErlangEmptyList]
        -- it "usort_stable" do
        --     r <- exec LS.erlps__usort_stable__1 [ErlangEmptyList]
        --     makeOk ok `shouldEqual` r

        it "keymerge" do
            testExecOk ok LS.erlps__keymerge__1 [ErlangEmptyList]
        it "rkeymerge" do
            testExecOk ok LS.erlps__rkeymerge__1 [ErlangEmptyList]
        it "keysort_1" do
            testExecOk ok LS.erlps__keysort_1__1 [ErlangEmptyList]
        it "keysort_rand" do
            testExecOk ok LS.erlps__keysort_rand__1 [ErlangEmptyList]
        it "keysort_i" do
            testExecOk ok LS.erlps__keysort_i__1 [ErlangEmptyList]
        it "keysort_stable" do
            testExecOk ok LS.erlps__keysort_stable__1 [ErlangEmptyList]
        it "keysort_error" do
            testExecOk ok LS.erlps__keysort_error__1 [ErlangEmptyList]

        it "keymember" do
            testExecOk ok LS.erlps__keymember__1 [ErlangEmptyList]
        it "keysearch_keyfind" do
            testExecOk ok LS.erlps__keysearch_keyfind__1 [ErlangEmptyList]
        it "keystore" do
            testExecOk ok LS.erlps__keystore__1 [ErlangEmptyList]
        it "keytake" do
            testExecOk ok LS.erlps__keytake__1 [ErlangEmptyList]
        it "keyreplace" do
            testExecOk ok LS.erlps__keyreplace__1 [ErlangEmptyList]

        it "merge" do
            testExecOk ok LS.erlps__merge__1 [ErlangEmptyList]
        it "rmerge" do
            testExecOk ok LS.erlps__rmerge__1 [ErlangEmptyList]
        it "sort_1" do
            testExecOk ok LS.erlps__sort_1__1 [ErlangEmptyList]
        it "sort_rand" do
            testExecOk ok LS.erlps__sort_rand__1 [ErlangEmptyList]

        it "ukeymerge" do
            testExecOk ok LS.erlps__ukeymerge__1 [ErlangEmptyList]
        it "rukeymerge" do
            testExecOk ok LS.erlps__rukeymerge__1 [ErlangEmptyList]
        it "ukeysort_1" do
            testExecOk ok LS.erlps__ukeysort_1__1 [ErlangEmptyList]
        it "ukeysort_rand" do
            testExecOk ok LS.erlps__ukeysort_rand__1 [ErlangEmptyList]
        it "ukeysort_i" do
            testExecOk ok LS.erlps__ukeysort_i__1 [ErlangEmptyList]
        -- it "ukeysort_stable" do
        --     r <- exec LS.erlps__ukeysort_stable__1 [ErlangEmptyList]
        --     makeOk ok `shouldEqual` r
        it "ukeysort_error" do
            testExecOk ok LS.erlps__ukeysort_error__1 [ErlangEmptyList]
        it "funmerge" do
            testExecOk ok LS.erlps__funmerge__1 [ErlangEmptyList]
        it "rfunmerge" do
            testExecOk ok LS.erlps__rfunmerge__1 [ErlangEmptyList]
        it "funsort_1" do
            testExecOk ok LS.erlps__funsort_1__1 [ErlangEmptyList]
        it "funsort_stable" do
            testExecOk ok LS.erlps__funsort_stable__1 [ErlangEmptyList]
        it "funsort_error" do
            testExecOk ok LS.erlps__funsort_error__1 [ErlangEmptyList]
        it "funsort_rand" do
            testExecOk ok LS.erlps__funsort_rand__1 [ErlangEmptyList]

        it "ufunmerge" do
            testExecOk ok LS.erlps__ufunmerge__1 [ErlangEmptyList]
        it "rufunmerge" do
            testExecOk ok LS.erlps__rufunmerge__1 [ErlangEmptyList]
        it "ufunsort_1" do
            testExecOk ok LS.erlps__ufunsort_1__1 [ErlangEmptyList]
        -- it "ufunsort_stable" do
        --     r <- exec LS.erlps__ufunsort_stable__1 [ErlangEmptyList]
        --     makeOk ok `shouldEqual` r
        it "ufunsort_error" do
            testExecOk ok LS.erlps__ufunsort_error__1 [ErlangEmptyList]
        it "ufunsort_rand" do
            testExecOk ok LS.erlps__ufunsort_rand__1 [ErlangEmptyList]

        it "seq_loop" do
            testExecOk ok LS.erlps__seq_loop__1 [ErlangEmptyList]
        it "seq_2" do
            testExecOk ok LS.erlps__seq_2__1 [ErlangEmptyList]
        it "seq_3" do
            testExecOk ok LS.erlps__seq_3__1 [ErlangEmptyList]
        it "seq_2_e" do
            testExecOk ok LS.erlps__seq_2_e__1 [ErlangEmptyList]
        it "seq_3_e" do
            testExecOk ok LS.erlps__seq_3_e__1 [ErlangEmptyList]

        it "sublist_2" do
            testExecOk ok LS.erlps__sublist_2__1 [ErlangEmptyList]
        it "sublist_3" do
            testExecOk ok LS.erlps__sublist_3__1 [ErlangEmptyList]
        it "sublist_2_e" do
            testExecOk ok LS.erlps__sublist_2_e__1 [ErlangEmptyList]
        it "sublist_3_e" do
            testExecOk ok LS.erlps__sublist_3_e__1 [ErlangEmptyList]

        it "flatten_1" do
            testExecOk ok LS.erlps__flatten_1__1 [ErlangEmptyList]
        it "flatten_2" do
            testExecOk ok LS.erlps__flatten_2__1 [ErlangEmptyList]
        it "flatten_1_e" do
            testExecOk ok LS.erlps__flatten_1_e__1 [ErlangEmptyList]
        it "flatten_2_e" do
            testExecOk ok LS.erlps__flatten_2_e__1 [ErlangEmptyList]

        it "otp_5939" do
            testExecOk ok LS.erlps__otp_5939__1 [ErlangEmptyList]
        it "otp_6023" do
            testExecOk ok LS.erlps__otp_6023__1 [ErlangEmptyList]
        it "otp_6606" do
            testExecOk ok LS.erlps__otp_6606__1 [ErlangEmptyList]
        --it "otp_7230" do
        --    r <- exec LS.erlps__otp_7230__1 [ErlangEmptyList]
        --    makeOk ok `shouldEqual` r

        it "zip_unzip" do
            testExecOk ok LS.erlps__zip_unzip__1 [ErlangEmptyList]
        it "zip_unzip3" do
            testExecOk ok LS.erlps__zip_unzip3__1 [ErlangEmptyList]
        it "zipwith" do
            testExecOk ok LS.erlps__zipwith__1 [ErlangEmptyList]
        it "zipwith3" do
            testExecOk ok LS.erlps__zipwith3__1 [ErlangEmptyList]

        it "reverse" do
            testExecOk ok LS.erlps__reverse__1 [ErlangEmptyList]
        it "member" do
            testExecOk ok LS.erlps__member__1 [ErlangEmptyList]
        it "dropwhile" do
            testExecOk ok LS.erlps__dropwhile__1 [ErlangEmptyList]
        it "takewhile" do
            testExecOk ok LS.erlps__takewhile__1 [ErlangEmptyList]
        it "filter_partition" do
            testExecOk ok LS.erlps__filter_partition__1 [ErlangEmptyList]
        it "suffix" do
            testExecOk ok LS.erlps__suffix__1 [ErlangEmptyList]
        it "subtract" do
            testExecOk ok LS.erlps__subtract__1 [ErlangEmptyList]
        it "join" do
            testExecOk ok LS.erlps__join__1 [ErlangEmptyList]
        it "hof" do
            testExecOk ok LS.erlps__hof__1 [ErlangEmptyList]
        it "droplast" do
            testExecOk ok LS.erlps__droplast__1 [ErlangEmptyList]
        it "search" do
            testExecOk ok LS.erlps__search__1 [ErlangEmptyList]

    describe "Real Queue tests taken from OTP - queue_SUITE.erl" do
        it "do" do
            testExecOk ok QS.erlps__do__1 [ErlangEmptyList]
        it "to_list" do
            testExecOk ok QS.erlps__to_list__1 [ErlangEmptyList]
        it "io_test" do
            testExecOk ok QS.erlps__io_test__1 [ErlangEmptyList]
        it "op_test" do
            r <- exec QS.erlps__op_test__1 [ErlangEmptyList]
            makeOk ok `shouldEqual` r
        it "error" do
            testExecOk ok QS.erlps__error__1 [ErlangEmptyList]
        --it "oops" do
        --    r <- exec QS.erlps__oops__1 [ErlangEmptyList]
        --    makeOk ok `shouldEqual` r

    describe "Real Dict tests taken from OTP - dict_SUITE.erl" do
        it "create" do
            testExecOk ok DS.erlps__create__1 [ErlangEmptyList]
        it "store" do
            testExecOk ok DS.erlps__store__1 [ErlangEmptyList]
        it "remove" do
            testExecOk ok DS.erlps__remove__1 [ErlangEmptyList]
        it "iterate" do
            testExecOk ok DS.erlps__iterate__1 [ErlangEmptyList]

    describe "Real Proplists tests taken from OTP - proplists_SUITE.erl" do
        it "create" do
            testExecOk ok PropS.erlps__examples__1 [ErlangEmptyList]

    describe "Real Sets tests taken from OTP - sets_SUITE.erl" do
        it "create" do
            testExecOk ok SetsS.erlps__create__1 [ErlangEmptyList]
        it "add_element" do
            testExecOk ok SetsS.erlps__add_element__1 [ErlangEmptyList]
        it "del_element" do
            testExecOk ok SetsS.erlps__del_element__1 [ErlangEmptyList]
        it "subtract" do
            testExecOk ok SetsS.erlps__subtract__1 [ErlangEmptyList]
        it "intersection" do
            testExecOk ok SetsS.erlps__intersection__1 [ErlangEmptyList]
        it "union" do
            testExecOk ok SetsS.erlps__union__1 [ErlangEmptyList]
        it "is_subset" do
            testExecOk ok SetsS.erlps__is_subset__1 [ErlangEmptyList]
        it "is_set" do
            testExecOk ok SetsS.erlps__is_set__1 [ErlangEmptyList]
        it "fold" do
            testExecOk ok SetsS.erlps__fold__1 [ErlangEmptyList]
        it "filter" do
            testExecOk ok SetsS.erlps__filter__1 [ErlangEmptyList]
        it "take_smallest" do
            testExecOk ok SetsS.erlps__take_smallest__1 [ErlangEmptyList]
        it "take_largest" do
            testExecOk ok SetsS.erlps__take_largest__1 [ErlangEmptyList]
        it "iterate" do
            testExecOk ok SetsS.erlps__iterate__1 [ErlangEmptyList]
        it "is_empty" do
            testExecOk ok SetsS.erlps__is_empty__1 [ErlangEmptyList]

    describe "Real Math tests taken from OTP - math_SUITE.erl" do
        it "floor_ceil" do
            testExecOk ok MathS.erlps__floor_ceil__1 [ErlangEmptyList]

    describe "Real Sofs tests taken from OTP - sofs_SUITE.erl" do
        it "from_term_1" do
            testExecOk ok SofsS.erlps__from_term_1__1 [ErlangEmptyList]
        it "set_1" do
            testExecOk ok SofsS.erlps__set_1__1 [ErlangEmptyList]
        it "from_sets_1" do
            testExecOk ok SofsS.erlps__from_sets_1__1 [ErlangEmptyList]
        it "relation_1" do
            testExecOk ok SofsS.erlps__relation_1__1 [ErlangEmptyList]

        it "a_function_1" do
            testExecOk ok SofsS.erlps__a_function_1__1 [ErlangEmptyList]
        it "family_1" do
            testExecOk ok SofsS.erlps__family_1__1 [ErlangEmptyList]
        it "relation_to_family_1" do
            testExecOk ok SofsS.erlps__relation_to_family_1__1 [ErlangEmptyList]
        it "domain_1" do
            testExecOk ok SofsS.erlps__domain_1__1 [ErlangEmptyList]

        it "range_1" do
            testExecOk ok SofsS.erlps__range_1__1 [ErlangEmptyList]
        it "image" do
            testExecOk ok SofsS.erlps__image__1 [ErlangEmptyList]
        it "inverse_image" do
            testExecOk ok SofsS.erlps__inverse_image__1 [ErlangEmptyList]
        it "inverse_1" do
            testExecOk ok SofsS.erlps__inverse_1__1 [ErlangEmptyList]
        it "converse_1" do
            testExecOk ok SofsS.erlps__converse_1__1 [ErlangEmptyList]

        it "no_elements_1" do
            testExecOk ok SofsS.erlps__no_elements_1__1 [ErlangEmptyList]
        it "substitution" do
            testExecOk ok SofsS.erlps__substitution__1 [ErlangEmptyList]
        it "restriction" do
            testExecOk ok SofsS.erlps__restriction__1 [ErlangEmptyList]
        it "drestriction" do
            testExecOk ok SofsS.erlps__drestriction__1 [ErlangEmptyList]

        it "projection" do
            testExecOk ok SofsS.erlps__projection__1 [ErlangEmptyList]
        it "strict_relation_1" do
            testExecOk ok SofsS.erlps__strict_relation_1__1 [ErlangEmptyList]
        it "extension" do
            testExecOk ok SofsS.erlps__extension__1 [ErlangEmptyList]

        it "weak_relation_1" do
            testExecOk ok SofsS.erlps__weak_relation_1__1 [ErlangEmptyList]
        it "to_sets_1" do
            testExecOk ok SofsS.erlps__to_sets_1__1 [ErlangEmptyList]
        it "specification" do
            testExecOk ok SofsS.erlps__specification__1 [ErlangEmptyList]
        it "union_1" do
            testExecOk ok SofsS.erlps__union_1__1 [ErlangEmptyList]

        it "intersection_1" do
            testExecOk ok SofsS.erlps__intersection_1__1 [ErlangEmptyList]
        it "difference" do
            testExecOk ok SofsS.erlps__difference__1 [ErlangEmptyList]
        it "symdiff" do
            testExecOk ok SofsS.erlps__symdiff__1 [ErlangEmptyList]

        it "symmetric_partition" do
            testExecOk ok SofsS.erlps__symmetric_partition__1 [ErlangEmptyList]
        it "is_sofs_set_1" do
            testExecOk ok SofsS.erlps__is_sofs_set_1__1 [ErlangEmptyList]
        it "is_set_1" do
            testExecOk ok SofsS.erlps__is_set_1__1 [ErlangEmptyList]
        it "is_equal" do
            testExecOk ok SofsS.erlps__is_equal__1 [ErlangEmptyList]

        it "is_subset" do
            testExecOk ok SofsS.erlps__is_subset__1 [ErlangEmptyList]
        it "is_a_function_1" do
            testExecOk ok SofsS.erlps__is_a_function_1__1 [ErlangEmptyList]
        it "is_disjoint" do
            testExecOk ok SofsS.erlps__is_disjoint__1 [ErlangEmptyList]
        it "join" do
            testExecOk ok SofsS.erlps__join__1 [ErlangEmptyList]

        it "canonical" do
            testExecOk ok SofsS.erlps__canonical__1 [ErlangEmptyList]
        it "composite_1" do
            testExecOk ok SofsS.erlps__composite_1__1 [ErlangEmptyList]
        it "relative_product_1" do
            testExecOk ok SofsS.erlps__relative_product_1__1 [ErlangEmptyList]

        it "relative_product_2" do
            testExecOk ok SofsS.erlps__relative_product_2__1 [ErlangEmptyList]
        it "product_1" do
            testExecOk ok SofsS.erlps__product_1__1 [ErlangEmptyList]
        it "partition_1" do
            testExecOk ok SofsS.erlps__partition_1__1 [ErlangEmptyList]
        it "partition_3" do
            testExecOk ok SofsS.erlps__partition_3__1 [ErlangEmptyList]

        it "multiple_relative_product" do
            testExecOk ok SofsS.erlps__multiple_relative_product__1 [ErlangEmptyList]
        -- it "digraph" do
        --     r <- exec SofsS.erlps__digraph__1 [ErlangEmptyList]
        --     makeOk ok `shouldEqual` r
        it "constant_function" do
            testExecOk ok SofsS.erlps__constant_function__1 [ErlangEmptyList]

        it "misc" do
            testExecOk ok SofsS.erlps__misc__1 [ErlangEmptyList]

        it "family_specification" do
            testExecOk ok SofsS.erlps__family_specification__1 [ErlangEmptyList]
        it "family_domain_1" do
            testExecOk ok SofsS.erlps__family_domain_1__1 [ErlangEmptyList]
        it "family_range_1" do
            testExecOk ok SofsS.erlps__family_range_1__1 [ErlangEmptyList]

        it "family_to_relation_1" do
            testExecOk ok SofsS.erlps__family_to_relation_1__1 [ErlangEmptyList]
        it "union_of_family_1" do
            testExecOk ok SofsS.erlps__union_of_family_1__1 [ErlangEmptyList]

        it "intersection_of_family_1" do
            testExecOk ok SofsS.erlps__intersection_of_family_1__1 [ErlangEmptyList]
        it "family_projection" do
            testExecOk ok SofsS.erlps__family_projection__1 [ErlangEmptyList]

        it "family_difference" do
            testExecOk ok SofsS.erlps__family_difference__1 [ErlangEmptyList]
        it "family_intersection_1" do
            testExecOk ok SofsS.erlps__family_intersection_1__1 [ErlangEmptyList]

        it "family_intersection_2" do
            testExecOk ok SofsS.erlps__family_intersection_2__1 [ErlangEmptyList]
        it "family_union_1" do
            testExecOk ok SofsS.erlps__family_union_1__1 [ErlangEmptyList]
        it "family_union_2" do
            testExecOk ok SofsS.erlps__family_union_2__1 [ErlangEmptyList]

        it "partition_family" do
            testExecOk ok SofsS.erlps__partition_family__1 [ErlangEmptyList]

    describe "Real Erlang Lexer/Parser tests taken from OTP - erl_scan_SUITE.erl" do
        it "iso88591" do
            testExecOk ok ESS.erlps__iso88591__1 [ErlangEmptyList]
        -- it "otp_7810" do
        --     r <- exec ESS.erlps__otp_7810__1 [ErlangEmptyList]
        --     makeOk ok `shouldEqual` r
        it "otp_10990" do
            testExecOk ok ESS.erlps__otp_10990__1 [ErlangEmptyList]
        it "otp_10992" do
            testExecOk ok ESS.erlps__otp_10992__1 [ErlangEmptyList]
        it "otp_11807" do
            testExecOk ok ESS.erlps__otp_11807__1 [ErlangEmptyList]
        -- it "otp_16480" do
        --     r <- exec ESS.erlps__otp_16480__1 [ErlangEmptyList]
        --     makeOk ok `shouldEqual` r
        -- it "otp_17024" do
        --     r <- exec ESS.erlps__otp_17024__1 [ErlangEmptyList]
        --     makeOk ok `shouldEqual` r
        it "error_1" do
            testExecOk ok ESS.erlps__error_1__1 [ErlangEmptyList]
        -- it "error_2" do
        --     r <- exec ESS.erlps__error_2__1 [ErlangEmptyList]
        --     makeOk ok `shouldEqual` r

    describe "Real Erlang Anno tests taken from OTP - erl_anno_SUITE.erl" do
        it "new" do
            testExecOk ok EAS.erlps__new__1 [ErlangEmptyList]
        it "is_anno" do
            testExecOk ok EAS.erlps__is_anno__1 [ErlangEmptyList]
        it "generated" do
            testExecOk ok EAS.erlps__generated__1 [ErlangEmptyList]
        it "end_location" do
            testExecOk ok EAS.erlps__end_location__1 [ErlangEmptyList]
        it "file" do
            testExecOk ok EAS.erlps__file__1 [ErlangEmptyList]
        it "line" do
            testExecOk ok EAS.erlps__line__1 [ErlangEmptyList]
        it "location" do
            testExecOk ok EAS.erlps__location__1 [ErlangEmptyList]
        it "record" do
            testExecOk ok EAS.erlps__record__1 [ErlangEmptyList]
        it "text" do
            testExecOk ok EAS.erlps__text__1 [ErlangEmptyList]
        it "bad" do
            testExecOk ok EAS.erlps__bad__1 [ErlangEmptyList]
        -- it "parse_abstract" do
        --     r <- exec EAS.erlps__parse_abstract__1 [ErlangEmptyList]
        --     makeOk ok `shouldEqual` r
        it "mapfold_anno" do
            testExecOk ok EAS.erlps__mapfold_anno__1 [ErlangEmptyList]


    describe "Real Erlang Eval tests taken from OTP - erl_eval_SUITE.erl" do
        it "guard_1" do
          testExecOk ok EES.erlps__guard_1__1 [ErlangEmptyList]

        it "guard_2" do
          testExecOk ok EES.erlps__guard_2__1 [ErlangEmptyList]

        -- it "match_pattern" do
        --   r <- exec EES.erlps__match_pattern__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        it "string_plusplus" do
          testExecOk ok EES.erlps__string_plusplus__1 [ErlangEmptyList]

        it "pattern_expr" do
          testExecOk ok EES.erlps__pattern_expr__1 [ErlangEmptyList]

        -- it "match_bin" do
        --   r <- exec EES.erlps__match_bin__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "guard_3" do
        --   r <- exec EES.erlps__guard_3__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        it "guard_4" do
          testExecOk ok EES.erlps__guard_4__1 [ErlangEmptyList]

        it "guard_5" do
          testExecOk ok EES.erlps__guard_5__1 [ErlangEmptyList]

        -- it "lc" do
        --   r <- exec EES.erlps__lc__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "simple_cases" do
        --   r <- exec EES.erlps__simple_cases__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "unary_plus" do
        --   r <- exec EES.erlps__unary_plus__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "apply_atom" do
        --   r <- exec EES.erlps__apply_atom__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "otp_5269" do
        --   r <- exec EES.erlps__otp_5269__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "otp_6539" do
        --   r <- exec EES.erlps__otp_6539__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "otp_6543" do
        --   r <- exec EES.erlps__otp_6543__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "otp_6787" do
        --   r <- exec EES.erlps__otp_6787__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        it "otp_6977" do
          testExecOk ok EES.erlps__otp_6977__1 [ErlangEmptyList]

        -- it "otp_7550" do
        --   r <- exec EES.erlps__otp_7550__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "otp_8133" do
        --   r <- exec EES.erlps__otp_8133__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "otp_10622" do
        --   r <- exec EES.erlps__otp_10622__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "otp_13228" do
        --   r <- exec EES.erlps__otp_13228__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "otp_14826" do
        --   r <- exec EES.erlps__otp_14826__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "funs" do
        --   r <- exec EES.erlps__funs__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "try_catch" do
        --   r <- exec EES.erlps__try_catch__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        it "eval_expr_5" do
          testExecOk ok EES.erlps__eval_expr_5__1 [ErlangEmptyList]

        -- it "zero_width" do
        --   r <- exec EES.erlps__zero_width__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "eep37" do
        --   r <- exec EES.erlps__eep37__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "eep43" do
        --   r <- exec EES.erlps__eep43__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

        -- it "otp_15035" do
        --   r <- exec EES.erlps__otp_15035__1 [ErlangEmptyList]
        --   makeOk ok `shouldEqual` r

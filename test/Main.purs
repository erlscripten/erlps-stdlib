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

-- BEWARE - HERE BE DRAGONS - I've lost too many hours debugging alternative helpers
-- If you think you can make a better wrapper which does not crash the testing infrastructure then please make a PR
-- If you can replace this helper with something better then please feel free to do so :)
exec_may_throw_aff :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw_aff fun args =
    let
      t = defer $ (\_ -> run_erlang fun args)
      f = defer $ (\_ -> ErlangAtom "error")
    in do
      v <- liftEffect (catchException (\_ -> pure f
                                      ) (pure t))
      pure $ force v

wololo_term :: Error -> ErlangTerm
wololo_term res = unsafeCoerce res

wololo_codepoint :: Partial => ErlangTerm -> StrCP.CodePoint
wololo_codepoint (ErlangInt res) = unsafeCoerce res

print_err (Right r) = show r
print_err (Left e) =
    case show e of
        "[object Object]" ->
            case (wololo_term e) of
                ErlangTuple [a,b,stack] ->
                    let
                        m1 = show a
                        m2 = show b
                        m3 = show stack
                    in
                        "[" <> m1 <> ", " <> m2 <> ", " <> m3 <> "]"
                r ->
                    show r
        r ->
            r

exec_may_throw :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw fun args = do
    res <- attempt $ exec_may_throw_aff fun args
    liftEffect $ log $ print_err res -- Uncomment for logs :)
    case res of
        Left _ -> pure make_err
        Right r -> pure $ make_ok r

lift_aff_to_erlang_process :: forall a. (Unit -> Aff a) -> Aff (T.Tuple ErlangTerm a)
lift_aff_to_erlang_process calc = do
        -- ONLY TOUCH THIS IF YOU KNOW WHAT YOU ARE DOING!!!!!
        -- THIS IS A DIRTY HACK TO "lift" an calculation in the Aff monad to an ErlangProcess from the GLOBAL scope
        res_channel <- AVar.empty
        pid_channel <- AVar.empty
        _ <- forkAff do
            packed_pid <- exec_may_throw BIF.erlang__spawn__1 [(
                ErlangFun 0 (\ _ -> let -- TODO: Fixme - the calculation should yield to the scheduler and only then we may launch the avar. We need a jump to FFI here :(
                    a = unsafePerformEffect $ launchAff_ (
                        do
                            res <- calc unit
                            AVar.put res res_channel
                        )
                    in
                       ErlangInt (DBI.fromInt 1)))]
            -- At this point we never yielded so the process MUST be alive
            pid <- unpack_ok packed_pid
            AVar.put pid pid_channel

        pid <- AVar.take pid_channel
        packed_is_alive <- exec_may_throw BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "true") `shouldEqualOk` packed_is_alive

        res <- AVar.take res_channel

        delay (Milliseconds 1.0) -- force a context switch to cleanup the process :P
        packed_is_alive <- exec_may_throw BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "false") `shouldEqualOk` packed_is_alive
        pure $ T.Tuple pid res

make_ok term = ErlangTuple [ErlangAtom "ok", term]
make_err = ErlangAtom "error"
mkInt :: Int -> ErlangTerm
mkInt = DBI.fromInt >>> ErlangInt

unpack_ok :: ErlangTerm -> Aff ErlangTerm
unpack_ok (ErlangTuple [ErlangAtom "ok", term]) = pure term
unpack_ok _ = do
    1 `shouldEqual` 0
    pure ErlangEmptyList
test_reverse a = do
    let input = arrayToErlangList $ map mkInt a
    let output = make_ok $ arrayToErlangList $ map mkInt (A.reverse a)
    res <- exec_may_throw erlps__reverse__1 [input]
    output `shouldEqual` res

test_sort a = do
    let input = arrayToErlangList $ map mkInt a
    let output = make_ok $ arrayToErlangList $ map mkInt (A.sort a)
    res <- exec_may_throw erlps__sort__1 [input]
    output `shouldEqual` res

test_map a ef pf = do
    let input = arrayToErlangList $ map mkInt a
    let output = make_ok $ arrayToErlangList $ map mkInt (map pf a)
    res <- exec_may_throw erlps__map__2 [ef, input]
    output `shouldEqual` res

test_zip_ok a b = do
    let input_a = arrayToErlangList $ map mkInt a
    let input_b = arrayToErlangList $ map mkInt b
    let output = make_ok $ arrayToErlangList $ map (\(T.Tuple x y) -> ErlangTuple [x,y]) (A.zip (map mkInt a) (map mkInt b))
    res <- exec_may_throw erlps__zip__2 [input_a, input_b]
    output `shouldEqual` res

test_zip_fail a b = do
    let input_a = arrayToErlangList $ map mkInt a
    let input_b = arrayToErlangList $ map mkInt b
    res <- exec_may_throw erlps__zip__2 [input_a, input_b]
    make_err `shouldEqual` res

test_seq from to expected = do
    calc <- exec_may_throw erlps__seq__2 [mkInt from, mkInt to]
    let out = make_ok $ arrayToErlangList $ map mkInt expected
    out `shouldEqual` calc

shouldEqualOk a b = make_ok a `shouldEqual` b

main :: Effect Unit
main =
    launchAff_ $ runSpec [consoleReporter] do

    let whitelist = case unit of
          _ -> M.Nothing  -- comment for whitelist :)
          _ -> M.Just ["ETS"]
    let describe_ s = case whitelist of
          M.Nothing -> describe s
          M.Just l ->
            if A.elemIndex s l == M.Nothing then \_ -> pure unit else describe s
    describe_ "Sanity check" do
        it "one should equal one" do
            1 `shouldEqual` 1
        it "two should equal two" do
            2 `shouldEqual` 2

    describe_ "ETS" do
      it "set table" do
        r <- exec_may_throw ETS.erlps__test_set__0 []
        ErlangAtom "ok" `shouldEqualOk` r
      it "bag table" do
        r <- exec_may_throw ETS.erlps__test_bag__0 []
        ErlangAtom "ok" `shouldEqualOk` r

    describe_ "STDLIB Lists" do
        it "reverse/1" do
            test_reverse [1,2,3,4,5,6,7,8,9,10]
            test_reverse [1]
            test_reverse []
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
            test_seq 1 0 []
            test_seq 1 10 [1,2,3,4,5,6,7,8,9,10]

    describe_ "Real Array tests taken from OTP - array_SUITE.erl" do
        it "new_test" do
            r <- exec_may_throw erlps__new_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "fix_test" do
            r <- exec_may_throw erlps__fix_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "relax_test" do
            r <- exec_may_throw erlps__relax_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "resize_test" do
            r <- exec_may_throw erlps__resize_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "set_get_test" do
            r <- exec_may_throw erlps__set_get_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "to_list_test" do
            r <- exec_may_throw erlps__to_list_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sparse_to_list_test" do
            r <- exec_may_throw erlps__sparse_to_list_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "from_list_test" do
            r <- exec_may_throw erlps__from_list_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "to_orddict_test" do
            r <- exec_may_throw erlps__to_orddict_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sparse_to_orddict_test" do
            r <- exec_may_throw erlps__sparse_to_orddict_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "from_orddict_test" do
            r <- exec_may_throw erlps__from_orddict_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "map_test" do
            r <- exec_may_throw erlps__map_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sparse_map_test" do
            r <- exec_may_throw erlps__sparse_map_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "foldl_test" do
            r <- exec_may_throw erlps__foldl_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sparse_foldl_test" do
            r <- exec_may_throw erlps__sparse_foldl_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "foldr_test" do
            r <- exec_may_throw erlps__foldr_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sparse_foldr_test" do
            r <- exec_may_throw erlps__sparse_foldr_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Maps tests taken from OTP - maps_SUITE.erl" do
        it "t_update_with_3" do
            r <- exec_may_throw erlps__t_update_with_3__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_update_with_4" do
            r <- exec_may_throw erlps__t_update_with_4__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_get_3" do
            r <- exec_may_throw erlps__t_get_3__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_filter_2" do
            r <- exec_may_throw erlps__t_filter_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_fold_3" do
            r <- exec_may_throw erlps__t_fold_3__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_map_2" do
            r <- exec_may_throw erlps__t_map_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_size_1" do
            r <- exec_may_throw erlps__t_size_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_iterator_1" do
            r <- exec_may_throw erlps__t_iterator_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_put_opt" do
            r <- exec_may_throw erlps__t_put_opt__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_merge_opt" do
            r <- exec_may_throw erlps__t_merge_opt__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_with_2" do
            r <- exec_may_throw erlps__t_with_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "t_without_2" do
            r <- exec_may_throw erlps__t_without_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- Those functions were added to OTP only a week ago...
        -- https://github.com/erlang/otp/commit/2736731d1c2acf17c216aae89e160a58d035d081
        --it "t_intersect" do
        --    r <- exec_may_throw erlps__t_intersect__1 [ErlangEmptyList]
        --    make_ok (ErlangAtom "ok") `shouldEqual` r
        --it "t_intersect_with" do
        --    r <- exec_may_throw erlps__t_intersect_with__1 [ErlangEmptyList]
        --    make_ok (ErlangAtom "ok") `shouldEqual` r
        --it "t_merge_with" do
        --    r <- exec_may_throw erlps__t_merge_with__1 [ErlangEmptyList]
        --    make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Lists tests taken from OTP - lists_SUITE.erl" do
        it "append_1" do
            r <- exec_may_throw LS.erlps__append_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "append_2" do
            r <- exec_may_throw LS.erlps__append_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "umerge" do
            r <- exec_may_throw LS.erlps__umerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "rumerge" do
            r <- exec_may_throw LS.erlps__rumerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "usort_1" do
            r <- exec_may_throw LS.erlps__usort_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "usort_rand" do
            r <- exec_may_throw LS.erlps__usort_rand__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "usort_stable" do
        --     r <- exec_may_throw LS.erlps__usort_stable__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r

        it "keymerge" do
            r <- exec_may_throw LS.erlps__keymerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "rkeymerge" do
            r <- exec_may_throw LS.erlps__rkeymerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "keysort_1" do
            r <- exec_may_throw LS.erlps__keysort_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "keysort_rand" do
            r <- exec_may_throw LS.erlps__keysort_rand__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "keysort_i" do
            r <- exec_may_throw LS.erlps__keysort_i__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "keysort_stable" do
            r <- exec_may_throw LS.erlps__keysort_stable__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "keysort_error" do
            r <- exec_may_throw LS.erlps__keysort_error__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "keymember" do
            r <- exec_may_throw LS.erlps__keymember__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "keysearch_keyfind" do
            r <- exec_may_throw LS.erlps__keysearch_keyfind__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "keystore" do
            r <- exec_may_throw LS.erlps__keystore__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "keytake" do
            r <- exec_may_throw LS.erlps__keytake__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "keyreplace" do
            r <- exec_may_throw LS.erlps__keyreplace__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "merge" do
            r <- exec_may_throw LS.erlps__merge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "rmerge" do
            r <- exec_may_throw LS.erlps__rmerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sort_1" do
            r <- exec_may_throw LS.erlps__sort_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sort_rand" do
            r <- exec_may_throw LS.erlps__sort_rand__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "ukeymerge" do
            r <- exec_may_throw LS.erlps__ukeymerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "rukeymerge" do
            r <- exec_may_throw LS.erlps__rukeymerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "ukeysort_1" do
            r <- exec_may_throw LS.erlps__ukeysort_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "ukeysort_rand" do
            r <- exec_may_throw LS.erlps__ukeysort_rand__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "ukeysort_i" do
            r <- exec_may_throw LS.erlps__ukeysort_i__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "ukeysort_stable" do
        --     r <- exec_may_throw LS.erlps__ukeysort_stable__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r
        it "ukeysort_error" do
            r <- exec_may_throw LS.erlps__ukeysort_error__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "funmerge" do
            r <- exec_may_throw LS.erlps__funmerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "rfunmerge" do
            r <- exec_may_throw LS.erlps__rfunmerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "funsort_1" do
            r <- exec_may_throw LS.erlps__funsort_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "funsort_stable" do
            r <- exec_may_throw LS.erlps__funsort_stable__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "funsort_error" do
            r <- exec_may_throw LS.erlps__funsort_error__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "funsort_rand" do
            r <- exec_may_throw LS.erlps__funsort_rand__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "ufunmerge" do
            r <- exec_may_throw LS.erlps__ufunmerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "rufunmerge" do
            r <- exec_may_throw LS.erlps__rufunmerge__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "ufunsort_1" do
            r <- exec_may_throw LS.erlps__ufunsort_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "ufunsort_stable" do
        --     r <- exec_may_throw LS.erlps__ufunsort_stable__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r
        it "ufunsort_error" do
            r <- exec_may_throw LS.erlps__ufunsort_error__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "ufunsort_rand" do
            r <- exec_may_throw LS.erlps__ufunsort_rand__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "seq_loop" do
            r <- exec_may_throw LS.erlps__seq_loop__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "seq_2" do
            r <- exec_may_throw LS.erlps__seq_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "seq_3" do
            r <- exec_may_throw LS.erlps__seq_3__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "seq_2_e" do
            r <- exec_may_throw LS.erlps__seq_2_e__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "seq_3_e" do
            r <- exec_may_throw LS.erlps__seq_3_e__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "sublist_2" do
            r <- exec_may_throw LS.erlps__sublist_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sublist_3" do
            r <- exec_may_throw LS.erlps__sublist_3__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sublist_2_e" do
            r <- exec_may_throw LS.erlps__sublist_2_e__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sublist_3_e" do
            r <- exec_may_throw LS.erlps__sublist_3_e__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "flatten_1" do
            r <- exec_may_throw LS.erlps__flatten_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "flatten_2" do
            r <- exec_may_throw LS.erlps__flatten_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "flatten_1_e" do
            r <- exec_may_throw LS.erlps__flatten_1_e__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "flatten_2_e" do
            r <- exec_may_throw LS.erlps__flatten_2_e__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "otp_5939" do
            r <- exec_may_throw LS.erlps__otp_5939__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "otp_6023" do
            r <- exec_may_throw LS.erlps__otp_6023__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "otp_6606" do
            r <- exec_may_throw LS.erlps__otp_6606__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        --it "otp_7230" do
        --    r <- exec_may_throw LS.erlps__otp_7230__1 [ErlangEmptyList]
        --    make_ok (ErlangAtom "ok") `shouldEqual` r

        it "zip_unzip" do
            r <- exec_may_throw LS.erlps__zip_unzip__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "zip_unzip3" do
            r <- exec_may_throw LS.erlps__zip_unzip3__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "zipwith" do
            r <- exec_may_throw LS.erlps__zipwith__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "zipwith3" do
            r <- exec_may_throw LS.erlps__zipwith3__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "reverse" do
            r <- exec_may_throw LS.erlps__reverse__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "member" do
            r <- exec_may_throw LS.erlps__member__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "dropwhile" do
            r <- exec_may_throw LS.erlps__dropwhile__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "takewhile" do
            r <- exec_may_throw LS.erlps__takewhile__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "filter_partition" do
            r <- exec_may_throw LS.erlps__filter_partition__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "suffix" do
            r <- exec_may_throw LS.erlps__suffix__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "subtract" do
            r <- exec_may_throw LS.erlps__subtract__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "join" do
            r <- exec_may_throw LS.erlps__join__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "hof" do
            r <- exec_may_throw LS.erlps__hof__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "droplast" do
            r <- exec_may_throw LS.erlps__droplast__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "search" do
            r <- exec_may_throw LS.erlps__search__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Queue tests taken from OTP - queue_SUITE.erl" do
        it "do" do
            r <- exec_may_throw QS.erlps__do__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "to_list" do
            r <- exec_may_throw QS.erlps__to_list__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "io_test" do
            r <- exec_may_throw QS.erlps__io_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "op_test" do
        --     r <- exec_may_throw QS.erlps__op_test__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r
        it "error" do
            r <- exec_may_throw QS.erlps__error__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "oops" do
        --     r <- exec_may_throw QS.erlps__oops__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Dict tests taken from OTP - dict_SUITE.erl" do
        it "create" do
            r <- exec_may_throw DS.erlps__create__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "store" do
            r <- exec_may_throw DS.erlps__store__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "remove" do
            r <- exec_may_throw DS.erlps__remove__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "iterate" do
            r <- exec_may_throw DS.erlps__iterate__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Proplists tests taken from OTP - proplists_SUITE.erl" do
        it "create" do
            r <- exec_may_throw PropS.erlps__examples__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Sets tests taken from OTP - sets_SUITE.erl" do
        it "create" do
            r <- exec_may_throw SetsS.erlps__create__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "add_element" do
            r <- exec_may_throw SetsS.erlps__add_element__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "del_element" do
            r <- exec_may_throw SetsS.erlps__del_element__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "subtract" do
            r <- exec_may_throw SetsS.erlps__subtract__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "intersection" do
            r <- exec_may_throw SetsS.erlps__intersection__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "union" do
            r <- exec_may_throw SetsS.erlps__union__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is_subset" do
            r <- exec_may_throw SetsS.erlps__is_subset__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is_set" do
            r <- exec_may_throw SetsS.erlps__is_set__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "fold" do
            r <- exec_may_throw SetsS.erlps__fold__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "filter" do
            r <- exec_may_throw SetsS.erlps__filter__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "take_smallest" do
            r <- exec_may_throw SetsS.erlps__take_smallest__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "take_largest" do
            r <- exec_may_throw SetsS.erlps__take_largest__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "iterate" do
            r <- exec_may_throw SetsS.erlps__iterate__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is_empty" do
            r <- exec_may_throw SetsS.erlps__is_empty__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Math tests taken from OTP - math_SUITE.erl" do
        it "floor_ceil" do
            r <- exec_may_throw MathS.erlps__floor_ceil__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Sofs tests taken from OTP - sofs_SUITE.erl" do
        it "from_term_1" do
            r <- exec_may_throw SofsS.erlps__from_term_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "set_1" do
            r <- exec_may_throw SofsS.erlps__set_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "from_sets_1" do
            r <- exec_may_throw SofsS.erlps__from_sets_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "relation_1" do
            r <- exec_may_throw SofsS.erlps__relation_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "a_function_1" do
            r <- exec_may_throw SofsS.erlps__a_function_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "family_1" do
            r <- exec_may_throw SofsS.erlps__family_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "relation_to_family_1" do
            r <- exec_may_throw SofsS.erlps__relation_to_family_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "domain_1" do
            r <- exec_may_throw SofsS.erlps__domain_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "range_1" do
            r <- exec_may_throw SofsS.erlps__range_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "image" do
            r <- exec_may_throw SofsS.erlps__image__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "inverse_image" do
            r <- exec_may_throw SofsS.erlps__inverse_image__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "inverse_1" do
            r <- exec_may_throw SofsS.erlps__inverse_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "converse_1" do
            r <- exec_may_throw SofsS.erlps__converse_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "no_elements_1" do
            r <- exec_may_throw SofsS.erlps__no_elements_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "substitution" do
            r <- exec_may_throw SofsS.erlps__substitution__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "restriction" do
            r <- exec_may_throw SofsS.erlps__restriction__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "drestriction" do
            r <- exec_may_throw SofsS.erlps__drestriction__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "projection" do
            r <- exec_may_throw SofsS.erlps__projection__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "strict_relation_1" do
            r <- exec_may_throw SofsS.erlps__strict_relation_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "extension" do
            r <- exec_may_throw SofsS.erlps__extension__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "weak_relation_1" do
            r <- exec_may_throw SofsS.erlps__weak_relation_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "to_sets_1" do
            r <- exec_may_throw SofsS.erlps__to_sets_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "specification" do
            r <- exec_may_throw SofsS.erlps__specification__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "union_1" do
            r <- exec_may_throw SofsS.erlps__union_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "intersection_1" do
            r <- exec_may_throw SofsS.erlps__intersection_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "difference" do
            r <- exec_may_throw SofsS.erlps__difference__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "symdiff" do
            r <- exec_may_throw SofsS.erlps__symdiff__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "symmetric_partition" do
            r <- exec_may_throw SofsS.erlps__symmetric_partition__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is_sofs_set_1" do
            r <- exec_may_throw SofsS.erlps__is_sofs_set_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is_set_1" do
            r <- exec_may_throw SofsS.erlps__is_set_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is_equal" do
            r <- exec_may_throw SofsS.erlps__is_equal__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "is_subset" do
            r <- exec_may_throw SofsS.erlps__is_subset__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is_a_function_1" do
            r <- exec_may_throw SofsS.erlps__is_a_function_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is_disjoint" do
            r <- exec_may_throw SofsS.erlps__is_disjoint__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "join" do
            r <- exec_may_throw SofsS.erlps__join__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "canonical" do
            r <- exec_may_throw SofsS.erlps__canonical__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "composite_1" do
            r <- exec_may_throw SofsS.erlps__composite_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "relative_product_1" do
            r <- exec_may_throw SofsS.erlps__relative_product_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "relative_product_2" do
            r <- exec_may_throw SofsS.erlps__relative_product_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "product_1" do
            r <- exec_may_throw SofsS.erlps__product_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "partition_1" do
            r <- exec_may_throw SofsS.erlps__partition_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "partition_3" do
            r <- exec_may_throw SofsS.erlps__partition_3__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "multiple_relative_product" do
            r <- exec_may_throw SofsS.erlps__multiple_relative_product__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "digraph" do
        --     r <- exec_may_throw SofsS.erlps__digraph__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r
        it "constant_function" do
            r <- exec_may_throw SofsS.erlps__constant_function__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "misc" do
            r <- exec_may_throw SofsS.erlps__misc__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "family_specification" do
            r <- exec_may_throw SofsS.erlps__family_specification__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "family_domain_1" do
            r <- exec_may_throw SofsS.erlps__family_domain_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "family_range_1" do
            r <- exec_may_throw SofsS.erlps__family_range_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "family_to_relation_1" do
            r <- exec_may_throw SofsS.erlps__family_to_relation_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "union_of_family_1" do
            r <- exec_may_throw SofsS.erlps__union_of_family_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "intersection_of_family_1" do
            r <- exec_may_throw SofsS.erlps__intersection_of_family_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "family_projection" do
            r <- exec_may_throw SofsS.erlps__family_projection__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "family_difference" do
            r <- exec_may_throw SofsS.erlps__family_difference__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "family_intersection_1" do
            r <- exec_may_throw SofsS.erlps__family_intersection_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "family_intersection_2" do
            r <- exec_may_throw SofsS.erlps__family_intersection_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "family_union_1" do
            r <- exec_may_throw SofsS.erlps__family_union_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "family_union_2" do
            r <- exec_may_throw SofsS.erlps__family_union_2__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

        it "partition_family" do
            r <- exec_may_throw SofsS.erlps__partition_family__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Erlang Lexer/Parser tests taken from OTP - erl_scan_SUITE.erl" do
        it "iso88591" do
            r <- exec_may_throw ESS.erlps__iso88591__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "otp_7810" do
        --     r <- exec_may_throw ESS.erlps__otp_7810__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r
        it "otp_10990" do
            r <- exec_may_throw ESS.erlps__otp_10990__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "otp_10992" do
            r <- exec_may_throw ESS.erlps__otp_10992__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "otp_11807" do
            r <- exec_may_throw ESS.erlps__otp_11807__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "otp_16480" do
        --     r <- exec_may_throw ESS.erlps__otp_16480__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "otp_17024" do
        --     r <- exec_may_throw ESS.erlps__otp_17024__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r
        it "error_1" do
            r <- exec_may_throw ESS.erlps__error_1__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "error_2" do
        --     r <- exec_may_throw ESS.erlps__error_2__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Erlang Anno tests taken from OTP - erl_anno_SUITE.erl" do
        it "new" do
            r <- exec_may_throw EAS.erlps__new__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is_anno" do
            r <- exec_may_throw EAS.erlps__is_anno__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "generated" do
            r <- exec_may_throw EAS.erlps__generated__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "end_location" do
            r <- exec_may_throw EAS.erlps__end_location__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "file" do
            r <- exec_may_throw EAS.erlps__file__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "line" do
            r <- exec_may_throw EAS.erlps__line__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "location" do
            r <- exec_may_throw EAS.erlps__location__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "record" do
            r <- exec_may_throw EAS.erlps__record__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "text" do
            r <- exec_may_throw EAS.erlps__text__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "bad" do
            r <- exec_may_throw EAS.erlps__bad__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        -- it "parse_abstract" do
        --     r <- exec_may_throw EAS.erlps__parse_abstract__1 [ErlangEmptyList]
        --     make_ok (ErlangAtom "ok") `shouldEqual` r
        it "mapfold_anno" do
            r <- exec_may_throw EAS.erlps__mapfold_anno__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r


    describe_ "Real Erlang Eval tests taken from OTP - erl_eval_SUITE.erl" do
        it "guard_1" do
          r <- exec_may_throw EES.erlps__guard_1__1 [ErlangEmptyList]
          make_ok (ErlangAtom "ok") `shouldEqual` r

        it "guard_2" do
          r <- exec_may_throw EES.erlps__guard_2__1 [ErlangEmptyList]
          make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "match_pattern" do
        --   r <- exec_may_throw EES.erlps__match_pattern__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        it "string_plusplus" do
          r <- exec_may_throw EES.erlps__string_plusplus__1 [ErlangEmptyList]
          make_ok (ErlangAtom "ok") `shouldEqual` r

        it "pattern_expr" do
          r <- exec_may_throw EES.erlps__pattern_expr__1 [ErlangEmptyList]
          make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "match_bin" do
        --   r <- exec_may_throw EES.erlps__match_bin__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "guard_3" do
        --   r <- exec_may_throw EES.erlps__guard_3__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        it "guard_4" do
          r <- exec_may_throw EES.erlps__guard_4__1 [ErlangEmptyList]
          make_ok (ErlangAtom "ok") `shouldEqual` r

        it "guard_5" do
          r <- exec_may_throw EES.erlps__guard_5__1 [ErlangEmptyList]
          make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "lc" do
        --   r <- exec_may_throw EES.erlps__lc__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "simple_cases" do
        --   r <- exec_may_throw EES.erlps__simple_cases__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "unary_plus" do
        --   r <- exec_may_throw EES.erlps__unary_plus__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "apply_atom" do
        --   r <- exec_may_throw EES.erlps__apply_atom__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "otp_5269" do
        --   r <- exec_may_throw EES.erlps__otp_5269__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "otp_6539" do
        --   r <- exec_may_throw EES.erlps__otp_6539__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "otp_6543" do
        --   r <- exec_may_throw EES.erlps__otp_6543__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "otp_6787" do
        --   r <- exec_may_throw EES.erlps__otp_6787__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        it "otp_6977" do
          r <- exec_may_throw EES.erlps__otp_6977__1 [ErlangEmptyList]
          make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "otp_7550" do
        --   r <- exec_may_throw EES.erlps__otp_7550__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "otp_8133" do
        --   r <- exec_may_throw EES.erlps__otp_8133__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "otp_10622" do
        --   r <- exec_may_throw EES.erlps__otp_10622__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "otp_13228" do
        --   r <- exec_may_throw EES.erlps__otp_13228__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "otp_14826" do
        --   r <- exec_may_throw EES.erlps__otp_14826__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "funs" do
        --   r <- exec_may_throw EES.erlps__funs__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "try_catch" do
        --   r <- exec_may_throw EES.erlps__try_catch__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        it "eval_expr_5" do
          r <- exec_may_throw EES.erlps__eval_expr_5__1 [ErlangEmptyList]
          make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "zero_width" do
        --   r <- exec_may_throw EES.erlps__zero_width__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "eep37" do
        --   r <- exec_may_throw EES.erlps__eep37__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "eep43" do
        --   r <- exec_may_throw EES.erlps__eep43__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

        -- it "otp_15035" do
        --   r <- exec_may_throw EES.erlps__otp_15035__1 [ErlangEmptyList]
        --   make_ok (ErlangAtom "ok") `shouldEqual` r

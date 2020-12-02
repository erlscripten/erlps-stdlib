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
          _ -> M.Just ["Binaries"]
    let describe_ s = case whitelist of
          M.Nothing -> describe s
          M.Just l ->
            if A.elemIndex s l == M.Nothing then \_ -> pure unit else describe s
    describe_ "Sanity check" do
        it "one should equal one" do
            1 `shouldEqual` 1
        it "two should equal two" do
            2 `shouldEqual` 2

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
        it "usort_stable" do
            r <- exec_may_throw LS.erlps__usort_stable__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

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



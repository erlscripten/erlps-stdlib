module Ets where

import Data.ArrayBuffer.Types
import Data.Binary.Base64
import Erlang.Builtins
import Erlang.Type
import Node.Buffer
import Partial.Unsafe
import Prelude
import Unsafe.Coerce

import Data.Array as DA
import Data.BigInt as DBI
import Data.Either as DE
import Data.List as DL
import Data.Maybe as DM
import Erlang.Binary as BIN
import Erlang.Builtins as BIF
import Erlang.Exception as EXC
import Erlang.Helpers as H

import Data.Set as Set
import Data.Map as Map

type ETSOpts =
  { named :: Boolean
  , table_type :: String
  }

data ETSTable
  = ETSSet String (Map.Map ErlangTerm (Array ErlangTerm))
  | ETSBag String (Set.Set (Array ErlangTerm))

defaultOpts :: ETSOpts
defaultOpts = {named: false, table_type: "set"}

name :: ETSTable -> String
name t = case t of
  ETSSet n _ -> n
  ETSBag n _ -> n

newSet :: String -> ETSTable
newSet n = ETSSet n Map.empty

newBag :: String -> ETSTable
newBag n = ETSBag n Set.empty

foreign import registerNamed
  :: String -> Int -> ETSTable -> Unit
foreign import register
  :: Int -> ETSTable -> Unit
foreign import getTableImpl
  :: (Unit -> Unit) -> Int -> ETSTable
getTable :: Int -> ETSTable
getTable = getTableImpl EXC.badarg
foreign import solveNameImpl
  :: DM.Maybe Int -> (Int -> DM.Maybe Int) -> String -> DM.Maybe Int
solveName :: String -> DM.Maybe Int
solveName = solveNameImpl DM.Nothing DM.Just
foreign import etsDeleteImpl
  :: (Unit -> Unit) -> Int -> Unit
etsDelete :: Int -> Unit
etsDelete = etsDeleteImpl EXC.badarg


etsInsert :: Int -> (Array ErlangTerm) -> Unit
etsInsert ref entry | DM.Just {head: key} <- DA.uncons entry =
  case getTable ref of
    ETSSet n m -> register ref (ETSSet n $ Map.insert key entry m)
    ETSBag n s -> register ref (ETSBag n $ Set.insert entry s)
etsInsert _ _ = EXC.badarg unit

etsLookup :: Int -> ErlangTerm -> ErlangTerm
etsLookup ref key =
  case getTable ref of
    ETSSet _ m -> case Map.lookup key m of
      DM.Nothing -> ErlangEmptyList
      DM.Just arr -> arrayToErlangList [ErlangTuple arr]
    ETSBag _ s ->
      arrayToErlangList $
      map ErlangTuple $
      DA.filter (\arr -> case DA.uncons arr of
                    DM.Nothing -> H.error "Malformed ETS bag"
                    DM.Just {head: key'} -> key' == key
                ) $
      Set.toUnfoldable $
      s

etsTab2List :: Int -> ErlangTerm
etsTab2List ref =
  let arr = case getTable ref of
        ETSSet _ m ->
          DA.fromFoldable $
          Map.values m
        ETSBag _ s ->
          Set.toUnfoldable s
  in arrayToErlangList $ map ErlangTuple $ arr


mkref :: Unit -> Int
mkref unit = case BIF.erlang__make_ref__0 [] of
  ErlangReference i -> i
  _ -> H.error "make_ref error"

buildOpts :: DL.List ErlangTerm -> ETSOpts -> ETSOpts
buildOpts DL.Nil acc = acc
buildOpts (DL.Cons opt rest) acc =
  let acc1 = case opt of
        ErlangAtom "set" -> acc{table_type = "set"}
        ErlangAtom "bag" -> acc{table_type = "bag"}
        ErlangAtom "ordered_set" -> H.error $ "Unimplemented ets type: ordered_set"
        ErlangAtom "duplicate_bag" -> H.error $ "Unimplemented ets type: duplicate_bag"
        ErlangAtom "named_table" -> acc{named = true}
        _ -> EXC.badarg unit
  in buildOpts rest acc1

erlps__new__2 :: ErlangFun
erlps__new__2 [ErlangAtom name, eopts]
  | DM.Just optlist <- erlangListToList eopts
  = let opts = buildOpts optlist defaultOpts
        ref = mkref unit
        table = case opts.table_type of
          "set" -> newSet name
          "bag" -> newBag name
          _ -> EXC.badarg unit
        _ = if opts.named then registerNamed name ref table
            else register ref table
    in if opts.named
       then ErlangAtom name
       else ErlangReference ref
erlps__new__2 [_, _] = EXC.badarg unit
erlps__new__2 args = EXC.badarity (ErlangFun 2 erlps__new__2) args


erlps__insert__2 :: ErlangFun
erlps__insert__2 [ErlangAtom name, entry]
  | DM.Just ref <- solveName name
  = erlps__insert__2 [ErlangReference ref, entry]
erlps__insert__2 ([ErlangReference ref, ErlangTuple entry]) =
    let _ = etsInsert ref entry
    in ErlangAtom "true"
erlps__insert__2 ([ErlangReference _, ErlangEmptyList]) =
    ErlangAtom "true"
erlps__insert__2 ([ErlangReference ref, ErlangCons (ErlangTuple entry) rest]) =
    let _ = etsInsert ref entry
    in erlps__insert__2 [ErlangReference ref, rest]
erlps__insert__2 [_, _] = EXC.badarg unit
erlps__insert__2 args = EXC.badarity (ErlangFun 2 erlps__insert__2) args


erlps__lookup__2 :: ErlangFun
erlps__lookup__2 [ErlangAtom name, key]
  | DM.Just ref <- solveName name
  = erlps__lookup__2 [ErlangReference ref, key]
erlps__lookup__2 [ErlangReference ref, key] = etsLookup ref key
erlps__lookup__2 [_, _] = EXC.badarg unit
erlps__lookup__2 args = EXC.badarity (ErlangFun 2 erlps__lookup__2) args


erlps__delete__1 :: ErlangFun
erlps__delete__1 [ErlangAtom name]
  | DM.Just ref <- solveName name
  = erlps__delete__1 [ErlangReference ref]
erlps__delete__1 [ErlangReference ref] =
  let _ = etsDelete ref
  in ErlangAtom "true"
erlps__delete__1 [_] = EXC.badarg unit
erlps__delete__1 args = EXC.badarity (ErlangFun 1 erlps__delete__1) args

erlps__tab2list__1 :: ErlangFun
erlps__tab2list__1 [ErlangAtom name]
  | DM.Just ref <- solveName name
  = erlps__tab2list__1 [ErlangReference ref]
erlps__tab2list__1 [ErlangReference ref] =
  etsTab2List ref
erlps__tab2list__1 [_] = EXC.badarg unit
erlps__tab2list__1 args = EXC.badarity (ErlangFun 1 erlps__tab2list__1) args

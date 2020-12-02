%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%%----------------------------------------------------------------
%%% Purpose: Test suite for the 'maps' module.
%%%-----------------------------------------------------------------

-module(maps_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).

-export([t_update_with_3/1, t_update_with_4/1,
         t_get_3/1, t_filter_2/1,
         t_fold_3/1,t_map_2/1,t_size_1/1,
         t_iterator_1/1, t_put_opt/1, t_merge_opt/1,
         t_with_2/1,t_without_2/1,
         t_intersect/1, t_intersect_with/1,
         t_merge_with/1]).

-define(badmap(V,F,Args), {'EXIT', {{badmap,V}, _}}).
-define(badkey(K,F,Args), {'EXIT', {{badkey,K}, _}}).
-define(badarg(F,Args), {'EXIT', {badarg, _}}).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [t_update_with_3,t_update_with_4,
     t_get_3,t_filter_2,
     t_fold_3,t_map_2,t_size_1,
     t_iterator_1,t_put_opt,t_merge_opt,
     t_with_2,t_without_2,
     t_intersect, t_intersect_with,
     t_merge_with].

t_update_with_3(Config) when is_list(Config) ->
    V1 = value1,
    V2 = <<"value2">>,
    V3 = "value3",
    io:format("AAAA\n"),
    Map = #{ key1 => V1, key2 => V2, "key3" => V3 },
    io:format("BBBB\n"),
    Fun = fun(V) -> [V,V,{V,V}] end,

    io:format("TEST: ~p\n", [maps:update_with(key1,Fun,Map)]),
    #{ key1 := [V1,V1,{V1,V1}] } = maps:update_with(key1,Fun,Map),
    #{ key2 := [V2,V2,{V2,V2}] } = maps:update_with(key2,Fun,Map),
    #{ "key3" := [V3,V3,{V3,V3}] } = maps:update_with("key3",Fun,Map),

    %% error case
    ?badmap(b,update_with,[[a,b],a,b]) = (catch maps:update_with([a,b],id(a),b)),
    ?badarg(update_with,[[a,b],a,#{}]) = (catch maps:update_with([a,b],id(a),#{})),
    ?badkey([a,b],update_with,[[a,b],Fun,#{}]) = (catch maps:update_with([a,b],Fun,#{})),
    ok.

t_update_with_4(Config) when is_list(Config) ->
    V1 = value1,
    V2 = <<"value2">>,
    V3 = "value3",
    Map = #{ key1 => V1, key2 => V2, "key3" => V3 },
    Fun = fun(V) -> [V,V,{V,V}] end,
    Init = 3,

    #{ key1 := [V1,V1,{V1,V1}] } = maps:update_with(key1,Fun,Init,Map),
    #{ key2 := [V2,V2,{V2,V2}] } = maps:update_with(key2,Fun,Init,Map),
    #{ "key3" := [V3,V3,{V3,V3}] } = maps:update_with("key3",Fun,Init,Map),

    #{ key3 := Init } = maps:update_with(key3,Fun,Init,Map),

    %% error case
    ?badmap(b,update_with,[[a,b],a,b]) = (catch maps:update_with([a,b],id(a),b)),
    ?badarg(update_with,[[a,b],a,#{}]) = (catch maps:update_with([a,b],id(a),#{})),
    ok.


t_get_3(Config) when is_list(Config) ->
    Map = #{ key1 => value1, key2 => value2 },
    DefaultValue = "Default value",
    value1 = maps:get(key1, Map, DefaultValue),
    value2 = maps:get(key2, Map, DefaultValue),
    DefaultValue = maps:get(key3, Map, DefaultValue),

    %% error case
    ?badmap(a,get,[[a,b],a,def]) = (catch maps:get([a,b],id(a),def)),
    ok.

t_without_2(_Config) ->
    Ki = [11,22,33,44,55,66,77,88,99],
    M0 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100)]),
    M1 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100) -- Ki]),
    M1 = maps:without([{k,I}||I <- Ki],M0),

    %% error case
    ?badmap(a,without,[[a,b],a]) = (catch maps:without([a,b],id(a))),
    ?badmap(a,without,[{a,b},a]) = (catch maps:without({a,b},id(a))),
    ?badmap({0,<<>>,97},without,[[],{0,<<>>,97}]) = (catch maps:without([], {0,<<>>,97})),
    ?badmap({0,<<>>,97},without,[[false, -20, -8],{0,<<>>,97}]) = (catch maps:without([false, -20, -8], {0, <<>>, 97})),
    ?badarg(without,[a,#{}]) = (catch maps:without(a,#{})),
    ok.

t_with_2(_Config) ->
    Ki = [11,22,33,44,55,66,77,88,99],
    M0 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100)]),
    M1 = maps:from_list([{{k,I},{v,I}}||I<-Ki]),
    M1 = maps:with([{k,I}||I <- Ki],M0),

    %% error case
    ?badmap(a,with,[[a,b],a]) = (catch maps:with([a,b],id(a))),
    ?badmap(a,with,[{a,b},a]) = (catch maps:with({a,b},id(a))),
    ?badmap({0,<<>>,97},with,[[],{0,<<>>,97}]) = (catch maps:with([], {0,<<>>,97})),
    ?badmap({0,<<>>,97},with,[[false, -20, -8],{0,<<>>,97}]) = (catch maps:with([false, -20, -8], {0, <<>>, 97})),
    ?badarg(with,[a,#{}]) = (catch maps:with(a,#{})),
    ok.

t_filter_2(Config) when is_list(Config) ->
    M = #{a => 2, b => 3, c=> 4, "a" => 1, "b" => 2, "c" => 4},
    Pred1 = fun(K,V) -> is_atom(K) andalso (V rem 2) =:= 0 end,
    Pred2 = fun(K,V) -> is_list(K) andalso (V rem 2) =:= 0 end,
    #{a := 2,c := 4} = maps:filter(Pred1,M),
    #{"b" := 2,"c" := 4} = maps:filter(Pred2,M),
    #{a := 2,c := 4} = maps:filter(Pred1,maps:iterator(M)),
    #{"b" := 2,"c" := 4} = maps:filter(Pred2,maps:iterator(M)),
    %% error case
    ?badmap(a,filter,[_,a]) = (catch maps:filter(fun(_,_) -> ok end,id(a))),
    ?badarg(filter,[<<>>,#{}]) = (catch maps:filter(id(<<>>),#{})),
    ok.

t_fold_3(Config) when is_list(Config) ->
    Vs = lists:seq(1,200),
    M0 = maps:from_list([{{k,I},I}||I<-Vs]),
    #{ {k,1} := 1, {k,200} := 200} = M0,
    Tot0 = lists:sum(Vs),
    Tot1 = maps:fold(fun({k,_},V,A) -> A + V end, 0, M0),
    true = Tot0 =:= Tot1,
    Tot2 = maps:fold(fun({k,_},V,A) -> A + V end, 0, maps:iterator(M0)),
    true = Tot0 =:= Tot2,

    %% error case
    ?badmap(a,fold,[_,0,a]) = (catch maps:fold(fun(_,_,_) -> ok end,0,id(a))),
    ?badarg(fold,[<<>>,0,#{}]) = (catch maps:fold(id(<<>>),0,#{})),
    ok.

t_map_2(Config) when is_list(Config) ->
    Vs = lists:seq(1,200),
    M0 = maps:from_list([{{k,I},I}||I<-Vs]),
    #{ {k,1} := 1, {k,200} := 200} = M0,
    M1 = maps:map(fun({k,_},V) -> V + 42 end, M0),
    #{ {k,1} := 43, {k,200} := 242} = M1,
    M2 = maps:map(fun({k,_},V) -> V + 42 end, maps:iterator(M0)),
    #{ {k,1} := 43, {k,200} := 242} = M2,

    %% error case
    ?badmap(a,map,[_,a]) = (catch maps:map(fun(_,_) -> ok end, id(a))),
    ?badarg(map,[<<>>,#{}]) = (catch maps:map(id(<<>>),#{})),
    ok.

t_iterator_1(Config) when is_list(Config) ->

    %% Small map test
    M0 = #{ a => 1, b => 2 },
    I0 = maps:iterator(M0),
    {K1,V1,I1} = maps:next(I0),
    {K2,V2,I2} = maps:next(I1),
    none = maps:next(I2),

    KVList = lists:sort([{K1,V1},{K2,V2}]),
    KVList = lists:sort(maps:to_list(M0)),

    %% Large map test

    Vs2 = lists:seq(1,200),
    M2 = maps:from_list([{{k,I},I}||I<-Vs2]),
    KVList2 = lists:sort(iter_kv(maps:iterator(M2))),
    KVList2 = lists:sort(maps:to_list(M2)),

    %% Larger map test

    Vs3 = lists:seq(1,4000),
  %%Vs3 = lists:seq(1,10000),
    M3 = maps:from_list([{{k,I},I}||I<-Vs3]),
    KVList3 = lists:sort(iter_kv(maps:iterator(M3))),
    KVList3 = lists:sort(maps:to_list(M3)),
    ok.

iter_kv(I) ->
    case maps:next(I) of
        none ->
            [];
        {K,V,NI} ->
            [{K,V} | iter_kv(NI)]
    end.

t_put_opt(Config) when is_list(Config) ->
    Value = id(#{complex => map}),
    Map = id(#{a => Value}),
    true = (maps:put(a, Value, Map) =:= Map),
    ok.

t_merge_opt(Config) when is_list(Config) ->
    Small = id(#{a => 1}),
    true = (maps:merge(#{}, Small) =:= Small),
    true = (maps:merge(Small, #{}) =:= Small),
    true = (maps:merge(Small, Small) =:= Small),

    Large = maps:from_list([{I,I}||I<-lists:seq(1,200)]),
    true = (maps:merge(#{}, Large) =:= Large),
    true = (maps:merge(Large, #{}) =:= Large),
    true = (maps:merge(Large, Large) =:= Large),

    List = id([a|b]),
    ?badmap([a|b],merge,[[a|b],[a|b]]) = (catch maps:merge(List, List)),

    ok.

random_map(SizeConstant, InitSeed) ->
    {Ret, _} =
        lists:foldl(
          fun(_, {Map, Seed}) ->
                  K = Seed+1,
                  V = Seed+2,
                  {Map#{K => V}, Seed+5}
          end,
          {#{}, 1},
          lists:seq(1, SizeConstant)),
    Ret.

check_map_combiners_same_small(MapCombiner1, MapCombiner2, Seed) ->
    lists:foreach(
      fun(SizeConstant) ->
              lists:foreach(
                fun(SeedMult) ->
                   RandMap1 = random_map(SizeConstant,
                                         SizeConstant + 100000*SeedMult + Seed),
                   RandMap2 = random_map(SizeConstant,
                                         SizeConstant + 200000*SeedMult + Seed),
                   Comb1Res = MapCombiner1(RandMap1, RandMap2),
                   Comb2Res = MapCombiner2(RandMap1, RandMap2),
                   Comb1Res = Comb2Res
                end,
                lists:seq(1,100))

      end,
      lists:seq(1,10)).


check_map_combiners_same_large(MapCombiner1, MapCombiner2, Seed) ->
    lists:foreach(
      fun(SizeConstant) ->
              RandMap1 = random_map(SizeConstant, SizeConstant + Seed),
              RandMap2 = random_map(SizeConstant, SizeConstant + Seed),
              Comb1Res = MapCombiner1(RandMap1, RandMap2),
              Comb2Res = MapCombiner2(RandMap1, RandMap2),
              Comb1Res = Comb2Res
      end,
      [1000, 10000]),
    ok.

t_merge_with(Config) when is_list(Config) ->
    Small = #{1 => 1, 2 => 3},
    Large = #{1 => 3, 2 => 2, 10=>10},
    #{1 := {1,3}, 2 := {3,2}, 10 := 10} =
        maps:merge_with(fun(1, 1, 3) -> {1, 3};
                           (2, 3, 2) -> {3, 2}
                        end,
                        Small,
                        Large),

    %% Swapping input maps should reverse tuples

    #{1 := {3,1}, 2 := {2,3}, 10 := 10} =
        maps:merge_with(fun(1, V1, V2) -> {V1, V2};
                           (2, V1, V2) -> {V1, V2}
                        end,
                        Large,
                        Small),

    %% Swapping parameters in the output of the fun should also reverse
    %% tuples

    #{1 := {3,1}, 2 := {2,3}, 10 := 10} =
        maps:merge_with(fun(1, V1, V2) -> {V2, V1};
                           (2, V1, V2) -> {V2, V1}
                        end,
                        Small,
                        Large),

    %% Should give the same result as maps:merge/2 with the right combiner

    DefaultCombiner = fun(_, _, V2) -> V2 end,
    Merge2FromMerge3 = fun (M1, M2) -> maps:merge_with(DefaultCombiner, M1, M2) end,
    check_map_combiners_same_small(fun maps:merge/2, Merge2FromMerge3, 1),
    check_map_combiners_same_large(fun maps:merge/2, Merge2FromMerge3, 2),

    %% Should conceptually compute the same thing as
    %% lists:ukey_merge/2 with the right combiner

    MergeFromUKeyMerge =
        fun(M1, M2) ->
                L1 = lists:sort(maps:to_list(M1)),
                L2 = lists:sort(maps:to_list(M2)),
                %% ukeymerge takes from the first when collision
                ResList = lists:ukeymerge(1, L2, L1),
                maps:from_list(ResList)
        end,
    check_map_combiners_same_small(MergeFromUKeyMerge, Merge2FromMerge3, 3),
    check_map_combiners_same_large(MergeFromUKeyMerge, Merge2FromMerge3, 4),

    %% Empty maps

    Large = maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                            Large,
                            #{}),
    Large = maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                            #{},
                            Large),
    #{} = maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                          #{},
                          #{}),

    %% Errors

    {'EXIT', {badarg, _}} =
        (catch maps:merge_with(not_a_fun,#{},#{})),
    {'EXIT', {{badmap, a}, _}} =
        (catch maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, a, #{})),
    {'EXIT', {{badmap, b}, _}} =
        (catch maps:merge_with(fun(_K, _V1, _V2) -> error(ok) end, #{}, b)),
    {'EXIT', {{badmap, a}, _}} =
        (catch maps:merge_with(fun(_K, _V1, _V2) -> error(ok) end, a, b)),
    ok.

t_intersect(Config) when is_list(Config) ->
    Small = #{1 => 1, 2 => 3},
    Large = #{1 => 3, 2 => 2, 10=>10},
    #{1 := 3,2 := 2} = maps:intersect(Small, Large),

    %% Swapping input maps can make a difference

    #{1 := 1, 2 := 3} = maps:intersect(Large, Small),

    %% Should conceptually compute the same thing as
    %% gb_sets:intersect/2 with the right combiner

    IntersectFromGBSets =
        fun(M1, M2) ->
                Map1Keys = maps:keys(M1),
                Map2Keys = maps:keys(M2),
                GBSet1 = gb_sets:from_list(Map1Keys),
                GBSet2 = gb_sets:from_list(Map2Keys),
                GBSetIntersection = gb_sets:intersection(GBSet1, GBSet2),
                IntersectList = gb_sets:to_list(GBSetIntersection),
                lists:foldl(
                  fun(Key, SoFar) ->
                          SoFar#{Key => maps:get(Key, M2)}
                  end,
                  #{},
                  IntersectList)
        end,
    check_map_combiners_same_small(fun maps:intersect/2,
                                   IntersectFromGBSets,
                                   11),
    check_map_combiners_same_large(fun maps:intersect/2,
                                   IntersectFromGBSets,
                                   13),

    %% Empty maps

    #{} = maps:intersect(Large, #{}),
    #{} = maps:intersect(#{}, Large),
    #{} = maps:intersect(#{}, #{}),

    %% Errors

    {'EXIT', {{badmap, a}, _}} =
        (catch maps:intersect(a, #{})),
    {'EXIT', {{badmap, b}, _}} =
        (catch maps:intersect(#{}, b)),
    {'EXIT', {{badmap, a}, _}} =
        (catch maps:intersect(a, b)),
    ok.

t_intersect_with(Config) when is_list(Config) ->
    Small = #{1 => 1, 2 => 3},
    Large = #{1 => 3, 2 => 2, 10=>10},
    #{1 := {1,3}, 2 := {3,2}} =
        maps:intersect_with(fun(1, 1, 3) -> {1, 3};
                               (2, 3, 2) -> {3, 2}
                            end,
                            Small,
                            Large),

    %% Swapping input maps should reverse tuples

    #{1 := {3,1}, 2 := {2,3}} =
        maps:intersect_with(fun(1, V1, V2) -> {V1, V2};
                               (2, V1, V2) -> {V1, V2}
                            end,
                            Large,
                            Small),

    %% Swapping parameters in the output of the fun should also reverse
    %% tuples

    #{1 := {3,1}, 2 := {2,3}} =
        maps:intersect_with(fun(1, V1, V2) -> {V2, V1};
                               (2, V1, V2) -> {V2, V1}
                            end,
                            Small,
                            Large),

    %% Should give the same result as intersect/2 with the right combiner

    DefaultCombiner = fun(_, _, V2) -> V2 end,
    Intersect2FromIntersect3 =
        fun (M1, M2) -> maps:intersect_with(DefaultCombiner, M1, M2) end,
    check_map_combiners_same_small(fun maps:intersect/2,
                                   Intersect2FromIntersect3,
                                   7),
    check_map_combiners_same_large(fun maps:intersect/2,
                                   Intersect2FromIntersect3,
                                   8),

    %% Empty maps

    #{} = maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                              Large,
                              #{}),
    #{} = maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                              #{},
                              Large),
    #{} = maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                              #{},
                              #{}),

    %% Errors

    {'EXIT', {badarg, _}} =
        (catch maps:intersect_with(not_a_fun,#{},#{})),
    {'EXIT', {{badmap, a}, _}} =
        (catch maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, a, #{})),
    {'EXIT', {{badmap, b}, _}} =
        (catch maps:intersect_with(fun(_K, _V1, _V2) -> error(ok) end, #{}, b)),
    {'EXIT', {{badmap, a}, _}} =
        (catch maps:intersect_with(fun(_K, _V1, _V2) -> error(ok) end, a, b)),
    ok.

t_size_1(Config) when is_list(Config) ->
      0 = maps:size(#{}),
     10 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,10)])),
     20 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,20)])),
     30 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,30)])),
     40 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,40)])),
     50 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,50)])),
     60 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,60)])),
    600 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,600)])),

    %% error case
    %%
    %% Note that the stack trace is ignored because the compiler may have
    %% rewritten maps:size/2 to map_size.
    {'EXIT', {{badmap,a}, _}} = (catch maps:size(id(a))),
    {'EXIT', {{badmap,<<>>}, _}} = (catch maps:size(id(<<>>))),
    ok.

id(I) -> I.

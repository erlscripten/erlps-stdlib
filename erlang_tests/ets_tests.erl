-module(ets_tests).

-export([test_set/0, test_bag/0]).

test_set() ->
    T = ets:new(t, [set]),
    true = ets:insert(T, {1, a}),
    true = ets:insert(T, {1, b}),
    true = ets:insert(T, {1, b}),
    true = ets:insert(T, {1.0, c}),
    [{1, b}] = ets:lookup(T, 1),
    [{1, b}, {1.0, c}] = ets:tab2list(T),
    true = ets:delete(T),

    T1 = ets:new(t1, [set]),
    true = ets:insert(T1, {b}),
    true = ets:insert(T1, {c}),
    true = ets:insert(T1, {b}),
    true = ets:insert(T1, {a}),
    [{a}, {b}, {c}] = ets:tab2list(T1),
    true = ets:delete(T1),
    ok.


test_bag() ->
    T = ets:new(t, [bag]),
    true = ets:insert(T, {1, a}),
    true = ets:insert(T, {1, b}),
    true = ets:insert(T, {1, b}),
    true = ets:insert(T, {1.0, c}),
    [{1, a}, {1, b}] = ets:lookup(T, 1),
    [{1, a}, {1, b}, {1.0, c}] = ets:tab2list(T),
    true = ets:delete(T),

    T1 = ets:new(t1, [bag]),
    true = ets:insert(T1, {b}),
    true = ets:insert(T1, {c}),
    true = ets:insert(T1, {b}),
    true = ets:insert(T1, {a}),
    [{a}, {b}, {c}] = ets:tab2list(T1),
    true = ets:delete(T1),
    ok.

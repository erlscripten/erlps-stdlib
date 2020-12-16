-module(ct).

-export([fail/1]).

fail(Arg) ->
  io:put_chars(io_lib:format("TEST FAILED: ~p\n", [Arg])),
  throw(failed).

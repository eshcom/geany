-module(tmp2).

-export([test1/0, test2/0, test3/0, test4/0, test5/0, test6/1]).

%~ tmp2:test1().
test1() ->
  io:format("test1", []),
  try throw(123)
  catch
    throw:123 = Val -> max(1, Val) % this is a bif-func
  end.

%~ tmp2:test2().
test2() ->
  try throw(test_err2)
  catch
    throw:test_err2 = Val -> Val
  end.

%~ tmp2:test3().
test3() ->
  try throw(test_err3)
  catch
    throw:Err -> Err
  end.

%~ tmp2:test4().
test4() ->
  try throw(test_err4)
  catch
    throw:_Err -> _Err
  end.

%~ tmp2:test5().
test5() ->
  try throw(#{})
  catch
    throw:#{} = Val -> Val
  end.

%~ tmp2:test6(test_err6).
test6(Var) ->
  try throw({error, Var})
  catch
    throw:{error, Err} -> Err
  end.

-module(tmp2).
-compile(nowarn_export_all).
-compile(export_all).

-record(rmq@obj_, {f1@1, f1_2}).
-record(rmq@obj@, {f21@, f22_}).
-record('@rmq@obj@', {f21@, f22_}).

-define(CALL@_TIMEOUT, 1000).
-define(CALL_TIMEOUT@, 2000).
-define('call@_timeout', 3000).
%~ -define(call@_timeout, 3000). % equal 'call@_timeout'
-define('@call_timeout@', 4000).
-define(call@_timeout@, 5000).

%~ tmp2:test1().
test1() ->
  try throw(123)
  catch
    throw:123 = Val -> max(1, Val) % this is a bif-func
  end.

%~ tmp2:test2().
test2() ->
  try throw(test_@err2@)
  catch
    throw:test_@err2@ = Val ->
      io:format("~n~p is atom = ~p~n", [Val, is_atom(Val)])
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

%~ tmp2:test7().
test7() ->
  try throw("test\n_
             err7")
  catch
    throw:V@al -> V@al
  end.

%~ tmp2:test8().
test8() ->
  try throw('test\n_
             err8') % edit this line fails highligting (fixed in this commit)
  catch
    throw:_@al ->
      io:format("~n~p is atom = ~p~n", [_@al, is_atom(_@al)])
  end.

%~ tmp2:test9().
test9() ->
  try throw('tes@t\n_
             err9')
  catch
    throw:_@al ->
      io:format("~n~p is atom = ~p~n", [_@al, is_atom(_@al)])
  end.

%~ tmp2:test10().
test10() ->
  try throw('@tes@t\n_
             err10')
  catch
    throw:_@al@ ->
      io:format("~n~p is atom = ~p~n", [_@al@, is_atom(_@al@)])
  end.

%~ tmp2:test11().
test11() ->
  Node1 = node@test,
  Node2 = 'node@test',
  Node3 = '@test',
  Atom1 = node@te@st,
  Atom2 = 'node@te@st',
  Atom3 = '@te@st',
  Record1 = #rmq@obj_{f1@1 = val_1, f1_2 = val@2},
  Record2 = #rmq@obj@{f21@ = val_1, f22_ = val@2},
  Record3 = # rmq@obj_{f1@1 = val_1, f1_2 = val@2},
  Record4 = #	rmq@obj@{f21@ = val_1, f22_ = val@2},
  Record5 = #'@rmq@obj@'{f21@ = val_1, f22_ = val@2},
  Record6 = # '@rmq@obj@'{f21@ = val_1, f22_ = val@2},
  Record7 = #	'@rmq@obj@'{f21@ = val_1, f22_ = val@2},
  io:format("~p is atom = ~p~n", [Node1, is_atom(Node1)]),
  io:format("~p is atom = ~p~n", [Node2, is_atom(Node2)]),
  io:format("~p is atom = ~p~n", [Node3, is_atom(Node3)]),
  io:format("~p is atom = ~p~n", [Atom1, is_atom(Atom1)]),
  io:format("~p is atom = ~p~n", [Atom2, is_atom(Atom2)]),
  io:format("~p is atom = ~p~n", [Atom3, is_atom(Atom3)]),
  io:format("Macro1 = ~p~n", [?CALL@_TIMEOUT]),
  io:format("Macro2 = ~p~n", [?CALL_TIMEOUT@]),
  io:format("Macro2 = ~p~n", [? CALL_TIMEOUT@]),
  io:format("Macro2 = ~p~n", [?	CALL_TIMEOUT@]),
  io:format("Macro3 = ~p~n", [?'call@_timeout']),
  io:format("Macro3 = ~p~n", [? 'call@_timeout']),
  io:format("Macro3 = ~p~n", [?	'call@_timeout']),
  io:format("Macro4 = ~p~n", [?'@call_timeout@']),
  io:format("Macro4 = ~p~n", [? '@call_timeout@']),
  io:format("Macro4 = ~p~n", [?	'@call_timeout@']),
  io:format("Macro5 = ~p~n", [?call@_timeout@]),
  io:format("Macro5 = ~p~n", [? call@_timeout@]),
  io:format("Macro5 = ~p~n", [?	call@_timeout@]),
  io:format("Record1 = ~p~n", [Record1]),
  io:format("Record2 = ~p~n", [Record2]),
  io:format("Record3 = ~p~n", [Record3]),
  io:format("Record4 = ~p~n", [Record4]),
  io:format("Record5 = ~p~n", [Record5]),
  io:format("Record6 = ~p~n", [Record6]),
  io:format("Record7 = ~p~n", [Record7]),
  io:format("Record7.f21@ = ~p~n", [Record7#'@rmq@obj@'.f21@]),
  ok.

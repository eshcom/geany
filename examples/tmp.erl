-module(tmpmodule).
-define
-defin

io:format("test ~s~n~c\n~5f\n~5.7f\n~5.*f\n~-5f\n~ts~lp\n~n",
		  ["test", 1, 2, 3.5, 2, 78, 0.12]).

TEST1 = "\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x".
TEST2 = "\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0".
TEST3 = <<"\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0">>.
TEST4 = <<"test123\312546\76875\547sad">>.

Var01 = <<"1.0000000000000000000000001">>.
Var02 = 2.300000 = 2.30000e+0 = 2.3.
Var03 = 1.1+1.2 = 1.1 + 1.2 = 2.3.
Var04 = 1.1-1.2 = 1.1 - 1.2 = -0.09999999999999987.
Var05 = 0.123456789e-100 * 0.123456789e-100 = 1.524157875019052e-202.
Var06 = 2.30000e-10 = 2.3e-10.
Var07 = 2.30000e+10 = 2.3e10.
Var08 = 2.30000e-1 = 2.30000e-01 = 2.30000e-001 = 0.23.
Var09 = 2.30000e+1 = 2.30000e+01 = 2.30000e+001 = 23.0.
Var10 = X + Y = X+Y.
Var11 = X - Y = X-Y.
Var12 = 0.123456789e-1 = 0.123456789 / 10 = 0.0123456789.
Var13 = 0.123456789e-2 = 0.123456789 / 100 = 0.00123456789.
Var14 = 0.123456789e+1 = 0.123456789e1 = 0.123456789 * 10 = 1.23456789.
Var15 = 0.123456789e+2 = 0.123456789e2 = 0.123456789 * 100 = 12.3456789.
Var16 = +0.123 = 0+0.123 = 0.123.
Var17 = -0.123 = 0-0.123 = -0.123.
Var18 = -0.1e+0 = -0.1e+00.
Var19 = 1+1-2 = 1 + 1 - 2.

+
++
-
--

test(Var) ->
	_V10 = yield(),
	_V20 = integer_to_list(123),
	_V30 = max(1,3),		% this is a bif-func
	_V31 = erlang:max(1,3),	% this is a bif-func
	_V32 = test:max(1,3),	% this is a not bif-func
	_V33 = erlang:yield(),
	_V34 = erlang:integer_to_list(123),
	_V40 = os_stat_mem:default(),
	_V50 = os_stat_mem:'L6Proto'(Var),
	_V51 = os_stat_mem:'L6Proto'(test_atom),
	ok.

'L6Proto'(Param) ->
	Param.

info() ->
  Info#'RuntimeInfo'.duration - 0 > 0 orelse error([bad_duration,Info#'RuntimeInfo'.duration]),
  Info#'RuntimeInfo'.startedAt - 1600000000 > 0 andalso Info#'RuntimeInfo'.startedAt - 1800000000 < 0 orelse error([bad_startedAt,Info#'RuntimeInfo'.startedAt]),
  Info#'RuntimeInfo'.medias - 0 > 0 orelse error([bad_medias,Info#'RuntimeInfo'.medias]),
  Info#'RuntimeInfo'.sessions - 0 > 0 orelse error([bad_sessions,Info#'RuntimeInfo'.sessions]),
  % Info#'RuntimeInfo'.inMediaBandwidth - 0 > 0 orelse error([bad_inMediaBandwidth,Info#'RuntimeInfo'.inMediaBandwidth]),
  % Info#'RuntimeInfo'.outMediaBandwidth - 0 > 0 orelse error([bad_outMediaBandwidth,Info#'RuntimeInfo'.outMediaBandwidth]),
  % Info#'RuntimeInfo'.inSystemBandwidth - 0 > 0 orelse error([bad_inSystemBandwidth,Info#'RuntimeInfo'.inSystemBandwidth]),
  % Info#'RuntimeInfo'.outSystemBandwidth - 0 > 0 orelse error([bad_outSystemBandwidth,Info#'RuntimeInfo'.outSystemBandwidth]),
  Info#'RuntimeInfo'.cpu - 0 > 0 orelse error([bad_cpu,Info#'RuntimeInfo'.cpu]),
  Info#'RuntimeInfo'.memory - 0 > 0 orelse error([bad_memory,Info#'RuntimeInfo'.memory]),
  ok.

call(Call) ->
  try gen_server:call(?MODULE, Call, 60000)
  catch
    exit:{timeout,_} ->
      {Dict,ST} = case whereis(?MODULE) of
        undefined -> {[],[]};
        Pid ->
          case process_info(Pid, [dictionary, current_stacktrace]) of
            [{dictionary, Dict_},{_,ST_}] -> {Dict_,ST_};
            undefined -> {[],[]}
          end
      end,
      State = proplists:get_value(state, Dict, false),
      events:info("config2_server in state ~p calling timeout: ~p\n~p", [State, Call, ST]),
      {error, timeout}
  end.

domain4(<<":", I, _/binary>>, Acc) when I >= $0 andalso I =< $9 orelse I == $/ -> Acc.
	[{<<"v2">>,
	  #{data:= <<_, $P, $N, $G, _/binary>>,
		path:= <<"@logo.png">>,
		type:= logo,
		x := 10,
		y := 10}} = S1] = streamcoder:get_tracks_logo_binary_spec(logo, Options).

config(<<C, Bin/binary>>, L, R) when C >= $a andalso C =< $z ->
  command(Bin, L, R, R+1, <<C>>);
config(<<C, Rest/binary>>, L, R) ->
  [{error, #{error => invalid_symbol,
			 line => L,
			 col => R,
			 what => <<C>>,
			 rest => Rest}}].

command(<<C, Bin/binary>>, L, R0, R, Acc)
		when C == $\t orelse C == $\r orelse C == $\s -> 
  [{command, L, R0, Acc} | params(Bin, L, R+1, Acc == <<"url">> orelse
											   Acc == <<"dvr">> orelse
											   Acc == <<"root">> orelse
											   Acc == <<"path">> orelse
											   Acc == <<"backend">> orelse
											   Acc == <<"push">> orelse
											   Acc == <<"sink">> orelse
											   Acc == <<"auth">>)];

quoted_string(<<"\n", Bin/binary>>, L0, R0, L, _R, Acc) ->
	quoted_string(Bin, L0, R0, L+1, 1, <<Acc/binary,"\n">>);
quoted_string(<<$\\,$", Bin/binary>>, L0, R0, L, R, Acc) ->
	quoted_string(Bin, L0, R0, L, R+1, <<Acc/binary,$">>);
quoted_string(<<$\\,$\\, Bin/binary>>, L0, R0, L, R, Acc) ->
	quoted_string(Bin, L0, R0, L, R+1, <<Acc/binary,$\\>>).

to_int3(<<I, Bin/binary>>, Base, Num) when I >= $0 andalso I =< $9 ->
  to_int3(Bin, Base, Num*Base + I - $0).

activate_from_file() ->
  try activate_from_file0()
  catch
    throw:R -> R
  end.

-module(tmp).
-compile(nowarn_export_all).
-compile(export_all).

-export([is_session_closed/1]).

-author(esh).
-version(1.0).

-define(CONFIG_MODULE, server_config).
-define(CODE_VERSION, "1.4").
-define(CALL@_TIMEOUT, 1000).
%~ -define
%~ -defin

-define(ZIP3(RecordName, Record),
  lists:zip3(
    record_info(fields, RecordName),
    tl(tuple_to_list(#RecordName{})),
    tl(tuple_to_list(Record))
  )
).

-record(tmp, {t1, t2}).
get(#?MODULE{} = Media, Key) -> 
  case record_info(fields, ?MODULE) of
    undefined ->
      Props = Media#?MODULE.t1,
      proplists:get_value(Key, Props);
    Pos ->  
      element(Pos, Media)
  end.

-record('RuntimeInfo', {
  duration,
  startedAt,
  medias,
  sessions,
  inMediaBandwidth,
  inSystemBandwidth,
  outMediaBandwidth,
  outSystemBandwidth,
  cpu,
  memory,
  %% with exensions
  erlang,
  schedulerLoad,
  schedulerLoadNormal,
  schedulerLoadDCPU,
  schedulerLoadDIO
}).

%%test comment @author @copyright { @copyright} @copyright}
%%test comment {@link edoc:file/2} {@docRoot} { @docRoot} @docRoot} {@docRoot
%%%test comment @author @copyright { @copyright} @copyright}
%%%test comment {@link edoc:file/2} {@docRoot} { @docRoot} @docRoot} {@docRoot

-record(tstrec, {
  name :: binary(),
  handler = undefined :: atom(),
  options = undefined :: any(),
  erlang = test_module
}).

tmp() ->
	io:format("test ~s~n~c\n~5f\n~5.7f\n~5.*f\n~-5f\n~ts~lp\n~n",
			  ["test", $a, 2, 3.5, 2.3e-10, 78, $\377, 5, 2]),

	_TEST1 = "\ \sx\tx\nx\rxx\"x\'x\!x\@x\#x\$x\%x\6x",
	_TEST2 = "\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0",
	_TEST3 = <<"\u00d0x\u00a8x\u00d0x\u00b0x\u00d0x\u00b1x\u00d0x\u00bbx\u00d0">>,
	_TEST4 = <<"test123\312546\76875\547sad">>,

	X = 15,
	Y = 20,
	_Tmp01 = X + Y,
	_Tmp02 = X - Y,
	_Tmp03 = X+Y,
	_Tmp04 = X-Y,
	
	_Var01 = <<"1.0000000000000000000000001">>,
	_Var02 = 2.300000 = 2.30000e+0 = 2.3,
	_Var03 = 1.1+1.2 = 1.1 + 1.2 = 2.3,
	_Var04 = 1.1-1.2 = 1.1 - 1.2 = -0.09999999999999987,
	_Var05 = 0.123456789e-100 * 0.123456789e-100 = 1.524157875019052e-202,
	_Var06 = 2.30000e-10 = 2.3e-10,
	_Var07 = 2.30000e+10 = 2.3e10,
	_Var08 = 2.30000e-1 = 2.30000e-01 = 2.30000e-001 = 0.23,
	_Var09 = 2.30000e+1 = 2.30000e+01 = 2.30000e+001 = 23.0,
	_Var10 = 0.123456789e-1 = 0.123456789 / 10 = 0.0123456789,
	_Var11 = 0.123456789e-2 = 0.123456789 / 100 = 0.00123456789,
	_Var12 = 0.123456789e+1 = 0.123456789e1 = 0.123456789 * 10 = 1.23456789,
	_Var13 = 0.123456789e+2 = 0.123456789e2 = 0.123456789 * 100 = 12.3456789,
	_Var14 = +0.123 = 0+0.123 = 0.123,
	_Var15 = -0.123 = 0-0.123 = -0.123,
	_Var16 = -0.1e+0 = -0.1e+00,
	_Var17 = 1+1-2 = 1 + 1 - 2,
	
	%~ _Var18 = 16#9g, % invalid hex
	_Var19 = 16#91, % valid hex
	_Var20 = 16#9f, % valid hex
	
	%~ _Var21 = 10#9f, % invalid dec
	_Var22 = 10#99, % valid dec
	
	%~ _Var23 = 8#78,  % invalid oct
	_Var24 = 8#77,  % valid oct
	
	%~ _Var25 = 2#12,  % invalid bin
	_Var26 = 2#11.  % valid bin

%~ +
%~ ++
%~ -
%~ --

tmp2(Body, Major, Minor, Patch, Build, Mod, Socket, Request) ->
	_Val1 = io_lib:format("~2..0B~2..0B~2..0B~2..0B", [Major, Minor, Patch, Build]),
	_Val2 = [ Y || <<_:16, Y:16/unsigned-little-integer>> <= Body ],
	<<_UnitId:8, UnitSize:8, _Unit0:UnitSize/binary, _Rest/binary>> = _Val2,
	
	ok = Mod:send(Socket, Request).		% this is a not bif-func

default() -> ok.
yield() -> ok.

max(V1, V2) when V1 > V2 -> V1;
max(_, V2) -> V2.

test(Var) ->
	Rec = #tstrec{},
	try throw(123)
	catch
		throw:123 = Val -> max(1, Val) % this is a bif-func
	end,
	_V11 = max(1,3),						% this is a bif-func
	_V12 = erlang:max(1,3),					% this is a bif-func
	_V12 = erlang: max(1,3),				% this is a bif-func
	_V12 = erlang :max(1,3),				% this is a bif-func
	_V13 = test:max(1,3),					% this is a not bif-func
	_V13 = test: max(1,3),					% this is a not bif-func
	_V13 = test :max(1,3),					% this is a not bif-func
	_V14 = 'erlang':max(1,3),				% this is a not bif-func
	_V14 = 'erlang': max(1,3),				% this is a not bif-func
	_V14 = 'erlang' :max(1,3),				% this is a not bif-func
	Mod = ?MODULE,
	_V15 = Mod:max(1,3),					% this is a not bif-func
	_V15 = Mod: max(1,3),					% this is a not bif-func
	_V15 = Mod :max(1,3),					% this is a not bif-func
	_V15 = (Rec#tstrec.erlang):max(1,3),	% this is a not bif-func
	_V15 = (Rec#tstrec.erlang): max(1,3),	% this is a not bif-func
	_V15 = (Rec#tstrec.erlang) :max(1,3),	% this is a not bif-func
	_V15 = ?MODULE:max(1,3),				% this is a not bif-func
	_V15 = ?MODULE: max(1,3),				% this is a not bif-func
	_V15 = ?MODULE :max(1,3),				% this is a not bif-func
	_V21 = yield(),							% this is a bif-func
	_V22 = erlang:yield(),					% this is a bif-func
	_V31 = integer_to_list(123),			% this is a bif-func
	_V32 = erlang:integer_to_list(123),		% this is a bif-func
	_V41 = default(),						% this is a not atom-spec
	_V42 = os_stat_mem:default(),			% this is a not atom-spec
	_V51 = os_stat_mem:'L6Proto'(Var),
	_V52 = os_stat_mem:'L6Proto'(test_atom),
	_V53 = server_config:'$mapper_record'(tc_decoder),
	Bin = <<"test">>,
	_V61 = 'L6proto':decode('PresenceMessage', Bin),
	_V62 = 'L6proto':'decode'('PresenceMessage', Bin),
	_V71 = nonode@nohost,
	_V71 = 'nonode@nohost',
	ok.

'L6Proto'(Param) ->
	Param.

info() ->
  Info = #'RuntimeInfo'{duration = 35, startedAt = 2, medias = 7, sessions = 78, cpu = 16, memory = 2048},
  
  Info#'RuntimeInfo'.duration - 0 > 0 orelse error([bad_duration,Info#'RuntimeInfo'.duration]),
  Info#'RuntimeInfo'.startedAt - 1600000000 > 0 andalso Info#'RuntimeInfo'.startedAt - 1800000000 < 0 orelse error([bad_startedAt,Info#'RuntimeInfo'.startedAt]),
  Info#'RuntimeInfo'.medias - 0 > 0 orelse error([bad_medias,Info#'RuntimeInfo'.medias]),
  Info#'RuntimeInfo'.sessions - 0 > 0 orelse error([bad_sessions,Info#'RuntimeInfo'.sessions]),
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

tmp3(S1, Options) ->
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

params(_V1, _V2, _V3, _V4) -> ok.

command(<<C, Bin/binary>>, L, R0, R, Acc)
		when C == $\t orelse C == $\r orelse C == $\s -> 
  [{command, L, R0, Acc} | params(Bin, L, R+1, Acc == <<"url">> orelse
											   Acc == <<"dvr">> orelse
											   Acc == <<"root">> orelse
											   Acc == <<"path">> orelse
											   Acc == <<"backend">> orelse
											   Acc == <<"push">> orelse
											   Acc == <<"sink">> orelse
											   Acc == <<"auth">>)].

quoted_string(<<"\n", Bin/binary>>, L0, R0, L, _R, Acc) ->
	quoted_string(Bin, L0, R0, L+1, 1, <<Acc/binary,"\n">>);
quoted_string(<<$\\,$", Bin/binary>>, L0, R0, L, R, Acc) ->
	quoted_string(Bin, L0, R0, L, R+1, <<Acc/binary,$">>);
quoted_string(<<$\\,$\\, Bin/binary>>, L0, R0, L, R, Acc) ->
	quoted_string(Bin, L0, R0, L, R+1, <<Acc/binary,$\\>>).

to_int3(<<I, Bin/binary>>, Base, Num) when I >= $0 andalso I =< $9 ->
  to_int3(Bin, Base, Num*Base + I - $0).

activate_from_file0() -> ok.
activate_from_file1() ->
  try activate_from_file0()
  catch
    throw:_R -> $
end. % edit this line - fails highlighting

activate_from_file2() ->
  try activate_from_file0()
  catch  % edit this line - fails highlighting
    throw:_R -> $\
end.

activate_from_file3() ->
  try activate_from_file0()
  catch
    throw:_R -> $\ end.

activate_from_file4() ->
  try activate_from_file0()
  catch
    throw:_R -> $\\end.

activate_from_file5() ->
  try activate_from_file0()
  catch
    throw:_R -> $\377end.

dec_FastStartToken(Bytes) ->
	'H235-SECURITY-MESSAGES':dec_ClearToken(Bytes).

'enc_FastStartToken'(Val) ->
	'H235-SECURITY-MESSAGES':'enc_ClearToken'(Val).

tmp4(Module, RecName, TName) ->
	case Module:'$mapper_record'(RecName) of
	  undefined -> undefined;
	  Oth1 -> Oth1
	end,
	
	case ?CONFIG_MODULE:'$mapper_type'(TName) of
	  undefined -> undefined;
	  Oth2 -> Oth2
	end,
	
	_Records = ?MODULE:'$mapper_records'().

capabilities(Cs) ->
  lists:foldl(fun erlang:'bor'/2, 0, Cs),
  lists:foldl(fun erlang:length/1, 0, Cs),
  lists:foldl(fun erlang:length2/1, 0, Cs),
  lists:foldl(fun erlang2:length/1, 0, Cs),
  lists:foldl(fun length/1, 0, Cs).

yeccpars1(_Tokens, _Tzr, _State, _States, _Vstack) -> ok.
yecc_error_type(_Error, _Stacktrace) -> ok.

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:1 -> erlang:raise(error, Error, Stacktrace)
            catch _:2 -> erlang:raise2(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

add_conf(Stream, Acc) ->
  case application:get_env(config2, {fake_backend, Stream}) of
    {ok, ignore} -> Acc;
    {ok, #{} = Conf} -> Acc#{Stream => Conf};
    {ok, null} -> Acc#{Stream => null};
    undefined -> Acc#{Stream => null}
  end.

-define(STUN_ATTR_SOFTWARE, 16#8022).
-define(STUN_ATTR_ALTERNATE_SERVER, 16#8023).
-define(STUN_ATTR_FINGERPRINT, 16#8028).
-define(STUN_ATTR_XOR_MAPPED_ADDRESS, 16#8028).
-define(STUN_ATTR_DATA, 16#8028).
-record(stun, {class = request :: request | response | error | indication,
	       trid = 0 :: non_neg_integer(),
	       raw = <<>> :: binary(),
	       'ALTERNATE-SERVER',
	       'DATA',
	       'ERROR-CODE',
	       'MAPPED-ADDRESS',
	       'SOFTWARE',
	       'UNKNOWN-ATTRIBUTES' = [],
	       'XOR-MAPPED-ADDRESS'}).
enc_attr(_Attr, undefined) ->
    <<>>;
enc_attr(Attr, Val) ->
    Len = size(Val),
    PaddLen = length(Len),
    <<Attr:16, Len:16, Val/binary, 0:PaddLen>>.

enc_addr(_Type, undefined) ->
    <<>>;
enc_addr(Type, {{A1, A2, A3, A4}, Port}) ->
    enc_attr(Type, <<0, 1, Port:16, A1, A2, A3, A4>>);
enc_addr(Type, {{A1, A2, A3, A4, A5, A6, A7, A8}, Port}) ->
    enc_attr(Type, <<0, 2, Port:16, A1:16, A2:16, A3:16,
		    A4:16, A5:16, A6:16, A7:16, A8:16>>).
enc_addr(_, _, _, _) -> ok.
enc_attrs(Msg) ->
    iolist_to_binary(
      [enc_attr(?STUN_ATTR_SOFTWARE, Msg#stun.'SOFTWARE'),
       enc_addr(?STUN_ATTR_FINGERPRINT, Msg#stun.'MAPPED-ADDRESS'),
       enc_addr(?STUN_ATTR_XOR_MAPPED_ADDRESS,
		Msg#stun.raw, Msg#stun.trid,
		Msg#stun.'XOR-MAPPED-ADDRESS'),
       enc_attr(?STUN_ATTR_DATA, Msg#stun.'DATA'),
       enc_addr(?STUN_ATTR_ALTERNATE_SERVER, Msg#stun.'ALTERNATE-SERVER'),
       enc_attr(test, Msg#stun.'UNKNOWN-ATTRIBUTES')]).

fun1(SessionId) ->
  Headers = <<"#!::t=",SessionId/binary,",a=",(base:version(describe))/binary>>,
  binary:copy(<<0>>, 184-1-size(Headers)),
  'L6proto':decode('PresenceMessage', Headers).


is_session_closed(#tmp{} = S)
    when S#tmp.t1 /= test1 -> true;
is_session_closed(#tmp{} = S)
    when S#tmp.t1 =/= test2 -> true.


runtime_device_ip0(Name) ->
  Output = os:cmd("ip -j address show dev "++binary_to_list(Name)),
  try jsx:decode(iolist_to_binary(Output), [return_maps,{labels,atom}]) of
    Runtime -> {ok, Runtime}
  catch
    C:E:ST ->
      events:error("failed to read ~s address: ~p:~p\n~p\n~p",[Name,C,E,ST,Output]),
      undefined
  end.

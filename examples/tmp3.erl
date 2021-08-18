-module( tmp3).
-compile(nowarn_export_all).
-compile(export_all).

-record(record1, {f1, f2}).
-record('record2', {f3, f4}).
-record( record3, {f1, f2}).
-record( 'record4', {f3, f4}).

-define(macros1, 2000).
-define('macros2', 3000).
-define( macros3, 2000).
-define( 'macros4', 3000).

-define(P, test).

test1() ->
  _Record1 = #record1{},
  _Record2 = #'record2'{},
  _Record3 = #record3{},
  _Record4 = #'record4'{},
  io:format("~n~p~n", [macros1]),
  io:format("~n~p~n", ['macros2']),
  io:format("~n~p~n", [macros3]),
  io:format("~n~p~n", ['macros4']),
  ok.

'test2'() ->
  <<"-----BEGIN RSA PRIVATE KEY-----
MIIEpAIBAAKCAQEA0SkqjxyvO5yOvqiU664Jo3ZMP8hb5ZFlK4bCV47VIWew3D3N
P+Nma/vPDVilzE1i9lg8SgPRo1LL0szRoRTR/Aw0y0ws9MINjAGXlw==
-----END RSA PRIVATE KEY-----">>.


run() ->
  test1(),
  'test2'(),
  ?P.

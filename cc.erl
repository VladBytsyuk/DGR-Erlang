-module(cc).
-export([run/0]).

run() ->
   compile:file(oset),
   compile:file(client),
   compile:file(server),
   compile:file(s_utils),
   compile:file(dgr),
   compile:file(run),
   compile:file(s_utils_test),
   s_utils_test:test(),
   {ok, compile_all}. 

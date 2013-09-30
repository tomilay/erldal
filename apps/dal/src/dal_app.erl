-module(dal_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
	application:start(dal).

start(_StartType, _StartArgs) ->
    dal_sup:start_link().

stop(_State) ->
    ok.

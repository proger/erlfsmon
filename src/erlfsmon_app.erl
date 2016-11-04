-module(erlfsmon_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erlfsmon_sup:start_link().

stop(_State) ->
    erlfsmon_sup:stop().

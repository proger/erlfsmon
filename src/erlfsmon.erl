-module(erlfsmon).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% APIs
-export([subscribe/0, start_logger/0, start_link_subscribe/3]).

start(_StartType, _StartArgs) ->
    {ok, Path} = application:get_env(erlfsmon, path),
    {ok, Backend} = application:get_env(erlfsmon, backend),
    {ok, GenEvent} = application:get_env(erlfsmon, gen_event_name),
    erlfsmon_sup:start_link(Path, Backend, GenEvent).

stop(_State) ->
    % application takes care of stopping the supervisor
    ok.

subscribe() ->
    {ok, GenEvent} = application:get_env(erlfsmon, gen_event_name),
    gen_event:add_sup_handler(GenEvent, {erlfsmon_event_bridge, self()}, [self()]).

start_logger() ->
    spawn(fun() -> subscribe(), logger_loop() end).

logger_loop() ->
    receive
        {_Pid, {erlfsmon_server, file_event}, {Path, Flags}} ->
            error_logger:info_msg("file_event: ~p ~p", [Path, Flags]);
        _ -> ignore
    end,
    logger_loop().

start_link_subscribe(Path, Backend, GenEvent) ->
    {ok, _} = erlfsmon_sup:start_link(Path, Backend, GenEvent),
    gen_event:add_sup_handler(GenEvent, {erlfsmon_event_bridge, self()}, [self()]).

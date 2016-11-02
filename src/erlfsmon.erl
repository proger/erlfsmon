-module(erlfsmon).
-export([
         subscribe/1,
         subscribe/2,
         subscribe/3,
         unsubscribe/1,
         known_events/0,
         start_logger/1]).

-type filter() :: fun((Filename :: string()) -> boolean()).
-type known_events() :: [atom()].

-spec subscribe(Path :: string()) -> Ref :: any().
subscribe(Path) ->
    subscribe(Path, fun(_) -> true end).

-spec subscribe(Path :: string(), filter()) -> Ref :: any().
subscribe(Path, Filter) ->
    KnownEvents = known_events(),
    gen_server:call(erlfsmon, {subscribe, Path, Filter, KnownEvents}).

-spec subscribe(Path :: string(), filter(), known_events()) -> Ref :: any().
subscribe(Path, Filter, KnownEvents) ->
    gen_server:call(erlfsmon, {subscribe, Path, Filter, KnownEvents}).

unsubscribe(Port) ->
    gen_server:cast(erlfsmon, {unsubscribe, Port}).

known_events() ->
    gen_server:call(erlfsmon, known_events).

start_logger(Path) ->
    spawn(fun() -> subscribe(Path), logger_loop() end).

logger_loop() ->
    receive
        {erlfsmon_events, Path, Flags} ->
            error_logger:info_msg("file_event: ~p ~p", [Path, Flags]);
        _ -> ignore
    end,
    logger_loop().

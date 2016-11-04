-module(erlfsmon_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    erlfsmon_server:stop().

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Backend = case os:type() of
        {unix, darwin} -> fsevents;
        {unix, linux} -> inotifywait;
        %{unix, linux} -> fanotify;
        _ -> throw(os_not_supported)
    end,

    case Backend:find_executable() of
        false -> throw({executable_not_found, Backend});
        _ -> ok
    end,

    {ok, { {one_for_one, 5, 10}, [
                ?CHILD(erlfsmon_server, worker, [Backend]),
                ?CHILD(gen_event, worker, [{local, erlfsmon_events}])
            ] } }.

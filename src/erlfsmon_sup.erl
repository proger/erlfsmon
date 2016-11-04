-module(erlfsmon_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Backend} = application:get_env(erlfsmon, backend),

    case Backend:find_executable() of
        false -> throw(executable_not_found);
        _ -> ok
    end,

    {ok, Path} = application:get_env(erlfsmon, path),

    {ok, { {one_for_one, 5, 10}, [
                ?CHILD(erlfsmon_server, worker, [Backend, Path, Path]),
                ?CHILD(gen_event, worker, [{local, erlfsmon_events}])
            ] } }.

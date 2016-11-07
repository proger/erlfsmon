-module(erlfsmon_sup).
-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link(Path, Backend, GenEvent) ->
    supervisor:start_link(?MODULE, [Path, Backend, GenEvent]).

init([Path, Backend, GenEvent]) ->
    case Backend:find_executable() of
        false -> throw(executable_not_found);
        _ -> ok
    end,
    {ok, { {one_for_one, 5, 10}, [
                ?CHILD(erlfsmon_server, worker, [Backend, Path, Path, GenEvent]),
                ?CHILD(gen_event, worker, [{local, GenEvent}])
            ] } }.

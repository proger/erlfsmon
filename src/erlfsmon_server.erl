-module(erlfsmon_server).
-behaviour(gen_server).

%% API Function Exports
-export([start_link/4]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port, path, backend, eventmgr}).

start_link(Backend, Path, Cwd, EventMgr) ->
    gen_server:start_link(?MODULE, [Backend, Path, Cwd, EventMgr], []).

init([Backend, Path, Cwd, EventMgr]) ->
    Port = Backend:start_port(Path, Cwd),
    {ok, #state{port=Port, path=Path, backend=Backend, eventmgr=EventMgr}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_Port, {data, {eol, Line}}}, #state{backend=Backend, eventmgr=Mgr} = State) ->
    Event = Backend:line_to_event(Line),
    gen_event:notify(Mgr, {self(), {?MODULE, file_event}, Event}),
    {noreply, State};
handle_info({_Port, {data, {noeol, Line}}}, State) ->
    error_logger:error_msg("~p line too long: ~p, ignoring~n", [self(), Line]),
    {noreply, State};
handle_info({_Port, {exit_status, Status}}, State) ->
    {stop, {port_exit, Status}, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port=Port}) ->
    (catch port_close(Port)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

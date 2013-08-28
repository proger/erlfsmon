-module(erlfsmon_fsevent).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API Function Exports
-export([start_link/1, subscribe/0]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port, path}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Path) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Path], []).

subscribe() ->
    gproc:reg({p, l, {?MODULE, file_event}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Path]) ->
    {Mon, MonArgs} = monitor_executable(Path),
    Port = erlang:open_port({spawn_executable, os:find_executable(Mon)},
        [stream, exit_status, {line, 16384}, {args, MonArgs}, {cd, "."}]),
    {ok, #state{
            port=Port,
            path=Path
        }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({Port, {data, {eol, Line}}}, State) ->
    [_EventId, Flags1, Path] = string:tokens(Line, [$\t]),
    [_, Flags2] = string:tokens(Flags1, [$=]),
    
    {ok, T, _} = erl_scan:string(Flags2 ++ "."),
    {ok, Flags} = erl_parse:parse_term(T),

    file_event(Path, Flags),
    {noreply, State};
handle_info({Port, {data, {noeol, Line}}}, State) ->
    error_logger:error_msg("~p line too long: ~p, ignoring~n", [?SERVER, Line]),
    {noreply, State};
handle_info({Port, {data, {exit_status, Status}}}, State) ->
    {stop, {port_exit, Status}, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

monitor_executable(Path) ->
    case os:type() of
        {unix, darwin} -> {"fsevent_watch", ["-F", Path]};
        {unix, linux} -> {"fanotify_watch", ["-c"]};
        _ -> undefined
    end.

% possible events:
% 
% mustscansubdirs,userdropped,kerneldropped,eventidswrapped,historydone,rootchanged,
% mount,unmount,created,removed,inodemetamod,renamed,modified,finderinfomod,changeowner,
% xattrmod,isfile,isdir,issymlink,ownevent
%

file_event(Path, Flags) ->
    io:format("event ~p path ~p~n", [Flags, Path]),
    notify(file_event, {Path, Flags}).

notify(file_event = A, Msg) ->
    Key = {?MODULE, A},
    gproc:send({p, l, Key}, {self(), Key, Msg}).

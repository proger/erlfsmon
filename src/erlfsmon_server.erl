-module(erlfsmon_server).
-behaviour(gen_server).
-define(SERVER, erlfsmon).

%% API Function Exports
-export([start_link/1, stop/0]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(subscribe, {
          owner,
          path = "" :: string(),
          file_filter,
          known_events = [] :: [atom()]
         }).

-record(state, {
          subscribers = #{} :: #{pid() => #subscribe{}},
          backend
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Backend) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Backend], []).

stop() -> gen_server:stop({local, ?SERVER}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Backend]) ->
    {ok, #state{backend = Backend}}.

handle_call({subscribe, Path, FileFilter, KnownEvents}, {Pid, _},
            #state{backend = Backend, subscribers = Subs} = State) ->
    Port = Backend:start_port(Path, Path),
    erlang:monitor(process, Pid),
    {reply, Port, State#state{
        subscribers = Subs#{Port => #subscribe{
                                       owner = Pid,
                                       path = Path,
                                       file_filter = FileFilter,
                                       known_events = KnownEvents
                                      }}}
    };

handle_call(known_events, _From, #state{backend=Backend} = State) ->
    {reply, Backend:known_events(), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({unsubscribe, Port}, #state{subscribers = Subs} = State) ->
    NewSubs = case maps:is_key(Port, Subs) of
        true ->  maps:remove(Port, Subs);
        false -> Subs
    end,
    {noreply, State#state{subscribers = NewSubs}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, {eol, Line}}},
            #state{backend = Backend, subscribers = Subs} = State) ->
    Sub = maps:get(Port, Subs),
    Event = Backend:line_to_event(Line),
    notify(Event, Sub),
    {noreply, State};
handle_info({_Port, {data, {noeol, Line}}}, State) ->
    error_logger:error_msg("~p line too long: ~p, ignoring~n", [?SERVER, Line]),
    {noreply, State};
handle_info({_Port, {exit_status, Status}}, State) ->
    {stop, {port_exit, Status}, State};

handle_info({'DOWN', _, process, Pid, _Info},
            State = #state{subscribers = Subs}) ->
    NewSubs = maps:filter(
      fun(Port, #subscribe{owner = Owner}) when Owner == Pid ->
              catch erlang:port_close(Port),
              false;
         (_, _) -> true
      end, Subs),
    {noreply, State#state{subscribers = NewSubs}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{subscribers = Subs}) ->
    io:format("Reason ~p~n", [Reason]),
    [(catch erlang:port_close(Port)) || Port  <- maps:keys(Subs)],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
notify({FileName, Actions}, #subscribe{owner = Owner,
                                       known_events = KnownEvents,
                                       file_filter = FileFilter}) ->
    case [ Action || Action <- Actions, lists:member(Action, KnownEvents) ] of
        [] -> noting;
        FilteredActions ->
            FileFilter(FileName) andalso
                (Owner ! {erlfsmon_events, {FileName, FilteredActions}})
    end.

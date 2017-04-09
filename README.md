## erlfsmon

`erlfsmon` is a tiny (under 100 LOC) wrapper around [fswatch](https://github.com/emcrisostomo/fswatch) used as port. It used to support more backends, see [previous releases](https://github.com/proger/erlfsmon/releases).

The application relies on `fswatch` present in `$PATH`.

The monitoring path is read from the application configuration (variable `path`). It defaults to `"."`.

Once the `erlfsmon` app is started you may use the simple API.

### The Core

`erlfsmon` is an OTP application and uses `gen_event` to allow multiple subscribers. In case you do not want that, save the following to snippet `simplefs.erl` which is only 25 LoC. Test by running `simplefs:run(".", ".", self()).` You need [erlsh for reasons](https://github.com/proger/erlsh#fdlink-port). Otherwise, keep reading.

```erlang
-module(simplefs).
-compile(export_all).

run(Path, Cwd, Receiver) ->
    Args = [os:find_executable("fswatch"),
            "--format=%p\t%f", "--event-flag-separator", ",", Path],
    erlang:open_port({spawn_executable, erlsh:fdlink_executable()},
                     [stream, exit_status, {line, 16384}, {args, Args}, {cd, Cwd}]),
    ?MODULE:loop([Path, Cwd, Receiver]).

loop([_, _, Receiver] = Args) ->
    receive
        {_Port, {data, {eol, Line}}} -> % noeols are ignored!
            [Path, Flags1] = string:tokens(Line, [$\t]),
            Flags2 = string:tokens(Flags1, [$,]),
            %% we trust fswatch enough to do list_to_atom
            Receiver ! {Path, [list_to_atom(F) || F <- Flags2]},
            ?MODULE:loop(Args);
        {_Port, {exit_status, Status}} ->
            error_logger:error_msg("fswatch exited with ~p, restarting~n", [Status]),
            erlang:apply(?MODULE, run, Args);
        T ->
            error_logger:error_msg("unhandled message ~p~n", [T]),
            ?MODULE:loop(Args)
    end.
```

### Building

```console
$ rebar get-deps compile
$ ERL_LIBS=deps erl -pa ebin -eval 'ok = application:ensure_started(erlfsmon, permanent).'
```

### Running

Make `self()` receive events as messages, assuming `ok = application:ensure_started(erlfsmon, permanent)`:

```erlang
> erlfsmon:subscribe().
%
% Wait for events:
%
> flush(). 
Shell got {<0.47.0>,
           {erlfsmon_server,file_event},
           {"/tank/proger/erlfsmon/src/4913",[closed,modified]}}
Shell got {<0.47.0>,
           {erlfsmon_server,file_event},
           {"/tank/proger/erlfsmon/src/erlfsmon_fanotify.erl",
            [closed,modified]}}
Shell got {<0.47.0>,
           {erlfsmon_server,file_event},
           {"/tank/proger/erlfsmon/ebin/erlfsmon_fanotify.bea#",
            [closed,modified]}}
Shell got {<0.47.0>,
           {erlfsmon_server,file_event},
           {"/tank/proger/erlfsmon/ebin/erlfsmon.app",[closed,modified]}}
```

Start a process that logs events using [error_logger](https://www.google.com.ua/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=error_logger), assuming `ok = application:ensure_started(erlfsmon, permanent)`:

```erlang
> erlfsmon:start_logger().

=INFO REPORT==== 28-Aug-2013::19:36:26 ===
file_event: "/tank/proger/erlfsmon/src/4913" [closed,modified]

=INFO REPORT==== 28-Aug-2013::19:36:26 ===
file_event: "/tank/proger/erlfsmon/src/erlfsmon_fanotify.erl" [closed, modified]

=INFO REPORT==== 28-Aug-2013::19:36:28 ===
file_event: "/tank/proger/erlfsmon/ebin/erlfsmon_fanotify.bea#" [closed, modified]
```

Alternatively, you can use the `start_link_subscribe/3` ignoring the `erlfsmon` OTP application by running:

```erlang
> Backend = fswatch. % or even supply your own backend module
> GenEvent = my_erlfsmon_events. % a unique identifier for gen_event worker
> erlfsmon:start_link_subscribe("/path/to/directory", Backend, GenEvent).
```

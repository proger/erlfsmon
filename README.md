## erlfsmon

`erlfsmon` is a tiny (under 100 LOC) wrapper around [fswatch](https://github.com/emcrisostomo/fswatch) used as port. It used to support more backends, see [previous releases](https://github.com/proger/erlfsmon/releases).

The application relies on `fswatch` present in `$PATH`.

The monitoring path is read from the application configuration (variable `path`). It defaults to `"."`.

Once the `erlfsmon` app is started you may use the simple API.

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

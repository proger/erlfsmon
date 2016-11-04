## erlfsmon

`erlfsmon` is a wrapper around [fswatch](https://github.com/emcrisostomo/fswatch) used as port. It used to support more backends, see [previous releases](https://github.com/proger/erlfsmon/releases).

The application relies on `fswatch` present in `$PATH`.

The monitoring path is read from the application configuration (variable `path`). It defaults to `"."`.

Once the `erlfsmon` app is started you may use the simple API.

```erlang
% rebar compile && erl -pa ebin -eval 'application:start(erlfsmon).'
%
> erlfsmon:subscribe(). % the pid will receive events as messages
% wait for events
> flush(). 
Shell got {<0.47.0>,
           {erlfsmon,file_event},
           {"/tank/proger/erlfsmon/src/4913",[closed,modified]}}
Shell got {<0.47.0>,
           {erlfsmon,file_event},
           {"/tank/proger/erlfsmon/src/erlfsmon_fanotify.erl",
            [closed,modified]}}
Shell got {<0.47.0>,
           {erlfsmon,file_event},
           {"/tank/proger/erlfsmon/ebin/erlfsmon_fanotify.bea#",
            [closed,modified]}}
Shell got {<0.47.0>,
           {erlfsmon,file_event},
           {"/tank/proger/erlfsmon/ebin/erlfsmon.app",[closed,modified]}}

> erlfsmon:start_logger(). % starts a sample process that logs events with error_logger

=INFO REPORT==== 28-Aug-2013::19:36:26 ===
file_event: "/tank/proger/erlfsmon/src/4913" [closed,modified]

=INFO REPORT==== 28-Aug-2013::19:36:26 ===
file_event: "/tank/proger/erlfsmon/src/erlfsmon_fanotify.erl" [closed,
                                                               modified]

=INFO REPORT==== 28-Aug-2013::19:36:28 ===
file_event: "/tank/proger/erlfsmon/ebin/erlfsmon_fanotify.bea#" [closed,
                                                                 modified]

=INFO REPORT==== 28-Aug-2013::19:36:28 ===
file_event: "/tank/proger/erlfsmon/ebin/erlfsmon.app" [closed,modified]
```

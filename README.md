## erlfsmon

`erlfsmon` is a wrapper around [fsevent_watch](https://github.com/proger/fsevent_watch)
and [fanotify_watch](https://github.com/proger/fanotify_watch) used as ports.

The application relies on the appropriate binaries to be in your `$PATH`.

The monitoring path is read from the application configuration (variable `path`). It defaults to `"."`.
Stay alert to the fact that in Linux you will get events for the whole
mount point -- the client must filter events by itself (see Caveats section in README.md for `fanotify_watch`).

The application is fairly lightweight -- it uses only [gproc](https://github.com/uwiger/gproc).

Once the `erlfsmon` app is started you may use the simple API.

```erlang
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

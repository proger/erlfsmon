-module(fsevents).
-export([find_executable/0, start_port/2, known_events/0, line_to_event/1]).

find_executable() ->
    os:find_executable("fsevent_watch").

start_port(Path, Cwd) ->
    erlang:open_port({spawn_executable, find_executable()},
        [stream, exit_status, {line, 16384}, {args, ["-F", Path]}, {cd, Cwd}]).

known_events() ->
    [mustscansubdirs,userdropped,kerneldropped,eventidswrapped,historydone,rootchanged,
        mount,unmount,created,removed,inodemetamod,renamed,modified,finderinfomod,changeowner,
        xattrmod,isfile,isdir,issymlink,ownevent].

line_to_event(Line) ->
    [_EventId, Flags1, Path] = string:tokens(Line, [$\t]),
    [_, Flags2] = string:tokens(Flags1, [$=]),
    
    {ok, T, _} = erl_scan:string(Flags2 ++ "."),
    {ok, Flags} = erl_parse:parse_term(T),

    {Path, Flags}.

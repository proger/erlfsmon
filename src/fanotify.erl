-module(fanotify).
-export([find_executable/0, start_port/2, known_events/0, line_to_event/1]).

find_executable() ->
    os:find_executable("fanotify_watch").

start_port(_Path, Cwd) ->
    erlang:open_port({spawn_executable, find_executable()},
        [stream, exit_status, {line, 16384}, {args, ["-c"]}, {cd, Cwd}]).

known_events() ->
    [closed, modified, isdir, undefined].

line_to_event(Line) ->
    [_EventId, Flags1, Path] = string:tokens(Line, [$\t]),
    Flags = [convert_flag(F) || F <- Flags1],
    {Path, Flags}.

convert_flag($C) -> closed;
convert_flag($W) -> modified;
convert_flag($D) -> isdir;
convert_flag(_) -> undefined.

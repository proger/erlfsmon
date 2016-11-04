-module(fswatch).
-export([find_executable/0, start_port/2, line_to_event/1]).

find_executable() ->
    os:find_executable("fswatch").

start_port(Path, Cwd) ->
    Args = ["-c", "$0 \"$@\" & PID=$!; read a; kill $PID",
            "fswatch", "--format=%p\t%f", "--event-flag-separator", ",", Path],
    erlang:open_port({spawn_executable, os:find_executable("sh")},
        [stream, exit_status, {line, 16384}, {args, Args}, {cd, Cwd}]).

line_to_event(Line) ->
    [Path, Flags1] = string:tokens(Line, [$\t]),
    Flags2 = string:tokens(Flags1, [$,]),
    % trust fswatch
    {Path, [list_to_atom(F) || F <- Flags2]}.

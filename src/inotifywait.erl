-module(inotifywait).
-export([find_executable/0, start_port/2, known_events/0, line_to_event/1]).

find_executable() ->
    os:find_executable("inotifywait").

start_port(Path, Cwd) ->
    Path1 = filename:absname(Path),
    Args = [find_executable(), "-m", "-e", "close_write", "-e", "moved_to", "-e", "create", "-r", Path1],

    erlang:open_port({spawn_executable, erlsh:fdlink_executable()},
        [stream, exit_status, {line, 16384}, {args, Args}, {cd, Cwd}]).

known_events() ->
    %[renamed, closed, modified, isdir, created, undefined].
    [renamed, closed, modified, isdir, undefined].

line_to_event(Line) ->
    {match, [Dir, Flags1, DirEntry]} = re:run(Line, re(), [{capture, all_but_first, list}]),
    Flags = [convert_flag(F) || F <- string:tokens(Flags1, ",")],
    Path = Dir ++ DirEntry,
    {Path, Flags}.

%convert_flag("CREATE") -> created;
convert_flag("ISDIR") -> isdir;
convert_flag("CLOSE_WRITE") -> modified;
convert_flag("CLOSE") -> closed;
convert_flag("MOVED_TO") -> renamed;
convert_flag(_Other) -> undefined.

re() ->
    case get(inotifywait_re) of
        undefined ->
            {ok, R} = re:compile("^(.*/) ([A-Z_,]+) (.*)$", [unicode]),
            put(inotifywait_re, R),
            R;
        V -> V
    end.




ifeq ($(shell uname), Darwin)
sync:
	fsevent_watch -F src/ | env PERLIO=:raw perl -ne 's#.*\t.*\t$$ENV{"PWD"}/src/.*erl$$#\2# && print "skip_deps=true\n"' | xargs -tn1 rebar compile
else
sync:
	fanotify_watch -c | env PERLIO=:raw perl -ne 's#.*\t.*\t$$ENV{"PWD"}/src/.*erl$$#\2# && print "skip_deps=true\n"' | xargs -tn1 rebar compile
endif

run:
	ERL_LIBS=deps erl -pa ebin -eval '[application:ensure_started(A, permanent) || A <- [gproc,erlfsmon]]'

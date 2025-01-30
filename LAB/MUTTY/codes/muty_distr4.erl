-module(muty).
-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
    register(l4, apply(Lock, start, [4])),
    l4 ! {peers, [l1, l2, l3]},
    register(w4, worker:start("George", l4, Sleep, Work)),
    ok.

stop() ->
    w4 ! stop.

-module(muty).
-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
    register(l1, apply(Lock, start, [1])),
    l1 ! {peers, [l2, l3, l4]},
    register(w1, worker:start("John", l1, Sleep, Work)),
    ok.

stop() ->
    w1 ! stop,

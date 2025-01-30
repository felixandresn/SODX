-module(muty).
-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
    register(l3, apply(Lock, start, [3])),
    l3 ! {peers, [l1, l2, l4]},
    register(w3, worker:start("Paul", l3, Sleep, Work)),
    ok.

stop() ->
    w3 ! stop,

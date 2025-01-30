-module(muty).
-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
    register(l2, apply(Lock, start, [2])),
    l2 ! {peers, [l1, l3, l4]},
    register(w2, worker:start("Ringo", l2, Sleep, Work)),    
    ok.

stop() ->
    w2 ! stop,

-module(lock3).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId, 0) end).

% Inicializar el nodo con un reloj lógico (inicializado en 0)
init(MyId, Clock) ->
    receive
        {peers, Nodes} -> open(MyId, Clock, Nodes);
        stop -> ok
    end.

% Estado abierto, espera solicitudes de bloqueo y solicitudes de otros nodos
open(MyId, Clock, Nodes) ->
    receive
        {take, Master, Ref} ->
            NewClock = Clock + 1,
            Refs = requests(Nodes, NewClock, MyId),
            wait(MyId, NewClock, Nodes, Master, Refs, [], Ref);
        {request, FromId, FromClock, From, Ref} ->
            NewClock = max(Clock, FromClock) + 1,
            From ! {ok, Ref},
            open(MyId, NewClock, Nodes);
        stop -> ok
    end.

% Enviar solicitudes con el reloj actual
requests(Nodes, Clock, MyId) ->
    lists:map(fun(P) ->
        R = make_ref(),
        P ! {request, MyId, Clock, self(), R},
        R
    end, Nodes).

% Estado de espera con control de relojes de Lamport
wait(MyId, Clock, Nodes, Master, [], Waiting, TakeRef) ->
    Master ! {taken, TakeRef},
    held(MyId, Clock, Nodes, Waiting);
wait(MyId, Clock, Nodes, Master, Refs, Waiting, TakeRef) ->
    receive
        {request, FromId, FromClock, From, Ref} ->
            NewClock = max(Clock, FromClock) + 1,
            if
                % Comparación de reloj y prioridad
                (FromClock < Clock) orelse
                (FromClock == Clock andalso FromId > MyId) ->
                    From ! {ok, Ref},
                    wait(MyId, NewClock, Nodes, Master, Refs, Waiting, TakeRef);
                true ->
                    wait(MyId, NewClock, Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef)
            end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(MyId, Clock, Nodes, Master, NewRefs, Waiting, TakeRef);
        release ->
            ok(Waiting),
            open(MyId, Clock, Nodes)
    end.

% Enviar `ok` a todos los nodos en espera
ok(Waiting) ->
    lists:map(fun({F, R}) -> F ! {ok, R} end, Waiting).

% Estado de retención, donde el nodo tiene el bloqueo
held(MyId, Clock, Nodes, Waiting) ->
    receive
        {request, FromId, FromClock, From, Ref} ->
            NewClock = max(Clock, FromClock) + 1,
            held(MyId, NewClock, Nodes, [{From, Ref} | Waiting]);
        release ->
            ok(Waiting),
            open(MyId, Clock, Nodes)
    end.


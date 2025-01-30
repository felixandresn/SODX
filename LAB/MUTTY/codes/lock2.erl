-module(lock2).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} -> open(MyId, Nodes);
        stop -> ok
    end.

% Estado abierto, esperando solicitudes de toma de bloqueo o de otros nodos
open(MyId, Nodes) ->
    receive
        {take, Master, Ref} ->
            Refs = requests(Nodes),
            wait(MyId, Nodes, Master, Refs, [], Ref);
        {request, _FromId, From, Ref} ->  % Eliminamos 'FromId'
            From ! {ok, Ref},
            open(MyId, Nodes);
        stop -> ok
    end.

% Enviar solicitudes de bloqueo a todos los nodos
requests(Nodes) ->
    lists:map(fun(P) ->
        R = make_ref(),
        P ! {request, self(), R},
        R
    end, Nodes).

% Estado de espera, esperando respuestas `ok` o solicitudes de otros nodos
wait(MyId, Nodes, Master, [], Waiting, TakeRef) ->
    Master ! {taken, TakeRef},
    held(MyId, Nodes, Waiting);
wait(MyId, Nodes, Master, Refs, Waiting, TakeRef) ->
    receive
        {request, FromId, From, Ref} ->
            % Si el ID de la solicitud es mayor, enviar `ok` de inmediato
            if
                FromId > MyId ->
                    From ! {ok, Ref},
                    wait(MyId, Nodes, Master, Refs, Waiting, TakeRef);
                true ->
                    wait(MyId, Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef)
            end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(MyId, Nodes, Master, NewRefs, Waiting, TakeRef);
        release ->
            ok(Waiting),
            open(MyId, Nodes)
    end.

% Enviar `ok` a todos los nodos en espera
ok(Waiting) ->
    lists:map(fun({F, R}) -> F ! {ok, R} end, Waiting).

% Estado de retención, esperando liberar el bloqueo
held(MyId, Nodes, Waiting) ->
    receive
        {request, _FromId, From, Ref} ->  % Eliminamos 'FromId'
            held(MyId, Nodes, [{From, Ref} | Waiting]);
        release ->
            ok(Waiting),
            open(MyId, Nodes)
    end.


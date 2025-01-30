-module(gms2).
-export([start/1, start/2]).

% Inicia el líder
start(Name) ->
    Self = self(),
    spawn_link(fun() -> init(Name, Self) end).

init(Name, Master) ->
    % Registrar al líder como "p1"
    global:register_name(p1, self()),
    leader(Name, Master, []).

% Inicia un esclavo
start(Name, Grp) ->
    Self = self(),
    spawn_link(fun() -> init(Name, Grp, Self) end).

init(Name, Grp, Master) ->
    Self = self(),
    Grp ! {join, Self},
    receive
        {view, Leader, Slaves} ->
            Master ! joined,
            erlang:monitor(process, Leader),
            slave(Name, Master, Leader, Slaves, undefined)
    after 1000 ->
        Master ! {error, "no reply from leader"}
    end.

% Comportamiento del líder
leader(Name, Master, Slaves) ->
    receive
        {mcast, Msg} ->
            % Difundir mensajes a los esclavos
            bcast(Name, {msg, Msg}, Slaves),
            Master ! {deliver, Msg},
            leader(Name, Master, Slaves);
        {join, Peer} ->
            % Añadir un nuevo esclavo
            NewSlaves = lists:append(Slaves, [Peer]),
            bcast(Name, {view, self(), NewSlaves}, NewSlaves),
            leader(Name, Master, NewSlaves);
        stop ->
            ok;
        {'DOWN', _, process, _, _} ->
            % El líder no debería recibir 'DOWN' porque no monitorea procesos
            io:format("leader ~s: received unexpected 'DOWN' message~n", [Name]),
            leader(Name, Master, Slaves);
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error]),
            leader(Name, Master, Slaves)
    end.

% Difundir mensajes a los nodos
bcast(_, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

% Comportamiento de un esclavo
slave(Name, Master, Leader, Slaves, Ref) ->
    receive
        {mcast, Msg} ->
            % Reenviar mensajes al líder
            Leader ! {mcast, Msg},
            slave(Name, Master, Leader, Slaves, Ref);
        {join, Peer} ->
            % Informar al líder sobre un nuevo esclavo
            Leader ! {join, Peer},
            slave(Name, Master, Leader, Slaves, Ref);
        {msg, Msg} ->
            % Recibir mensajes del líder
            Master ! {deliver, Msg},
            slave(Name, Master, Leader, Slaves, Ref);
        {view, NewLeader, NewSlaves} ->
            % Actualizar vista del líder y lista de esclavos
            case Ref of
                undefined -> 
                    ok;
                _ ->
                    erlang:demonitor(Ref, [flush])
            end,
            NewRef = erlang:monitor(process, NewLeader),
            slave(Name, Master, NewLeader, NewSlaves, NewRef);
        {'DOWN', Ref, process, Leader, _Reason} when Ref =:= Ref ->
            % Detectar la caída del líder
            io:format("Node ~s detected leader failure: ~p~n", [Name, Leader]),
            election(Name, Master, Slaves);
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error]),
            slave(Name, Master, Leader, Slaves, Ref)
    end.

% Proceso de elección de nuevo líder
election(Name, Master, Slaves) ->
    Self = self(),
    ValidSlaves = [S || S <- Slaves, is_process_alive(S)],
    case ValidSlaves of
        [Self | Rest] ->
            % Convertirse en el nuevo líder
            io:format("Node ~s becoming leader~n", [Name]),
            bcast(Name, {view, Self, Rest}, Rest),
            leader(Name, Master, Rest);
        [NewLeader | Rest] ->
            % Monitorear al nuevo líder y continuar como esclavo
            io:format("Node ~s elects new leader: ~p~n", [Name, NewLeader]),
            NewRef = erlang:monitor(process, NewLeader),
            % Asegurar sincronización con nueva vista
            bcast(Name, {view, NewLeader, Rest}, Rest),
            slave(Name, Master, NewLeader, Rest, NewRef);
        [] ->
            % Sin procesos válidos restantes
            io:format("Node ~s: No valid processes left in group~n", [Name]),
            Master ! {error, "No processes left in group"}
    end.


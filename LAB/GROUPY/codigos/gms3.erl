-module(gms3).
-export([start/1, start/2]).

% Inicia el líder
start(Name) ->
    Self = self(),
    spawn_link(fun() -> init(Name, Self) end).

init(Name, Master) ->
    global:register_name(p1, self()),
    leader(Name, Master, [], 1, none).

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
            slave(Name, Master, Leader, Slaves, 1, none, undefined)
    after 1000 ->
        Master ! {error, "no reply from leader"}
    end.

% Comportamiento del líder
leader(Name, Master, Slaves, N, Last) ->
    receive
        {mcast, Msg} ->
            % Difundir mensajes con número de secuencia
            bcast(Name, {msg, N, Msg}, Slaves),
            Master ! {deliver, Msg},
            leader(Name, Master, Slaves, N + 1, Msg);
        {join, Peer} ->
            % Añadir nuevo esclavo
            NewSlaves = lists:append(Slaves, [Peer]),
            bcast(Name, {view, self(), NewSlaves}, NewSlaves),
            leader(Name, Master, NewSlaves, N, Last);
        stop ->
            ok;
        {'DOWN', _, process, _, _} ->
            io:format("Leader ~s: received unexpected 'DOWN' message~n", [Name]),
            leader(Name, Master, Slaves, N, Last);
        Error ->
            io:format("Leader ~s: strange message ~w~n", [Name, Error]),
            leader(Name, Master, Slaves, N, Last)
    end.

% Difundir mensajes
bcast(_, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

% Comportamiento de un esclavo
slave(Name, Master, Leader, Slaves, N, Last, Ref) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Name, Master, Leader, Slaves, N, Last, Ref);
        {msg, Seq, Msg} when Seq >= N ->
            % Procesar mensaje y descartar duplicados
            if
                Seq > N -> 
                    io:format("Slave ~s: missed message sequence ~p, resyncing~n", [Name, Seq]);
                true ->
                    Master ! {deliver, Msg}
            end,
            slave(Name, Master, Leader, Slaves, Seq + 1, Msg, Ref);
        {view, NewLeader, NewSlaves} ->
            % Actualizar vista del líder y lista de esclavos
            case Ref of
                undefined -> ok;
                _ -> erlang:demonitor(Ref, [flush])
            end,
            NewRef = erlang:monitor(process, NewLeader),
            slave(Name, Master, NewLeader, NewSlaves, N, Last, NewRef);
        {'DOWN', Ref, process, Leader, _Reason} when Ref =:= Ref ->
            io:format("Node ~s detected leader failure: ~p~n", [Name, Leader]),
            election(Name, Master, Slaves, N, Last);
        stop ->
            ok;
        Error ->
            io:format("Slave ~s: strange message ~w~n", [Name, Error]),
            slave(Name, Master, Leader, Slaves, N, Last, Ref)
    end.

% Elección de nuevo líder
election(Name, Master, Slaves, N, Last) ->
    Self = self(),
    ValidSlaves = [S || S <- Slaves, is_process_alive(S)],
    case ValidSlaves of
        [Self | Rest] ->
            io:format("Node ~s becoming leader~n", [Name]),
            bcast(Name, {view, Self, Rest}, Rest),
            leader(Name, Master, Rest, N, Last);
        [NewLeader | Rest] ->
            io:format("Node ~s elects new leader: ~p~n", [Name, NewLeader]),
            NewRef = erlang:monitor(process, NewLeader),
            bcast(Name, {view, NewLeader, Rest}, Rest),
            slave(Name, Master, NewLeader, Rest, N, Last, NewRef);
        [] ->
            io:format("Node ~s: No valid processes left in group~n", [Name]),
            Master ! {error, "No processes left in group"}
    end.


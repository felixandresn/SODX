-module(gms1).
-export([start/1, start/2]).

start(Name) ->
    Self = self(),
    spawn_link(fun() -> init(Name, Self) end).

init(Name, Master) ->
    leader(Name, Master, []).

start(Name, Grp) ->
    Self = self(),
    spawn_link(fun() -> init(Name, Grp, Self) end).

init(Name, Grp, Master) ->
    Self = self(),
    Grp ! {join, Self},
    receive
        {view, Leader, Slaves} ->
            Master ! joined,
            slave(Name, Master, Leader, Slaves)
    end.

leader(Name, Master, Slaves) ->
    receive
        {mcast, Msg} ->
            bcast(Name, {msg, Msg}, [Master | Slaves]),  %% Multicast the message to all nodes
            Master ! {deliver, Msg},  %% Deliver the message to the application layer process
            leader(Name, Master, Slaves);
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),
            bcast(Name, {view, self(), NewSlaves}, [Master | NewSlaves]),  %% Multicast the new view
            leader(Name, Master, NewSlaves);
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error]),
            leader(Name, Master, Slaves)
    end.

bcast(_, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

slave(Name, Master, Leader, Slaves) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},  %% Forward the message to the leader
            slave(Name, Master, Leader, Slaves);
        {join, Peer} ->
            Leader ! {join, Peer},  %% Forward the join request to the leader
            slave(Name, Master, Leader, Slaves);
        {msg, Msg} ->
            Master ! {deliver, Msg},  %% Deliver the multicast message to the application layer process
            slave(Name, Master, Leader, Slaves);
        {view, NewLeader, NewSlaves} ->
            slave(Name, Master, NewLeader, NewSlaves);  %% Update the leader and slaves list
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error]),
            slave(Name, Master, Leader, Slaves)
    end.

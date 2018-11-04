-module(opty_srv).
-export([start/5, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Clients, Entries, Reads, Writes, Time) ->
    %% Remote server
    {ok, Hostname} = inet:gethostname(),
    spawn(list_to_atom(lists:concat(["opty-srv@", Hostname])), fun() -> register(s, server:start(Entries)) end),
    L = startClients(Clients, [], Entries, Reads, Writes),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n",
         [Clients, Entries, Reads, Writes, Time]),
    timer:sleep(Time*1000),
    stop(L).
stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    {ok, Hostname} = inet:gethostname(),
    {s,list_to_atom(lists:concat(["opty-srv@", Hostname]))} ! stop,
    %spawn(list_to_atom(lists:concat(["opty-srv@", Hostname])), fun() -> unregister(s) end),
    io:format("Stopped~n").

startClients(0, L, _, _, _) -> L;
startClients(Clients, L, Entries, Reads, Writes) ->
    {ok, Hostname} = inet:gethostname(),
    Pid = client:start(Clients, Entries, Reads, Writes, {s,list_to_atom(lists:concat(["opty-srv@", Hostname]))}),
    startClients(Clients-1, [Pid|L], Entries, Reads, Writes).

stopClients([]) ->
    ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    stopClients(L).

waitClients([]) ->
    ok;
waitClients(L) ->
    receive
        {done, Pid} ->
            waitClients(lists:delete(Pid, L))
    end.

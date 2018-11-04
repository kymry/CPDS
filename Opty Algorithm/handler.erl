-module(handler).
-export([start/3]).

start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
    handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->
    receive
        {read, Ref, N} ->
            case lists:keyfind(N, 1, Writes) of
                {N, _, Value} ->
                    Client ! {value, Ref, Value},
                    handler(Client, Validator, Store, Reads, Writes);
                false ->
                    Pid = store:lookup(N, Store),
                    Pid ! {read, Ref, self()},
                    handler(Client, Validator, Store, Reads, Writes)
            end;
        {Ref, Entry, Value, Time} ->
            Client ! {value, Ref, Value},
            handler(Client, Validator, Store, [{Entry, Time}|Reads], Writes);
        {write, N, Value} ->
            Pid = store:lookup(N, Store),
            Added = lists:keystore(N, 1, Writes, {N, Pid, Value}),
            handler(Client, Validator, Store, Reads, Added);
        {commit, Ref} ->
            Validator ! {validate, Ref, Reads, Writes, Client},
            rpc:call( node(), erlang, display, [ "handler is running on this node." ]);
        abort ->
            ok
    end.

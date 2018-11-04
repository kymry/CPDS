-module(handler).
-export([start/3]).

start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
    TId = make_ref(),
    handler(Client, Validator, Store, [], [], TId).

handler(Client, Validator, Store, Reads, Writes, TId) ->
    receive
        {read, Ref, N} ->
            case lists:keyfind(N, 1, Writes) of
                {N, _, Value} ->
                    Client ! {value, Ref, Value},
                    handler(Client, Validator, Store, Reads, Writes, TId);
                false ->
                    Pid = store:lookup(N, Store),
                    Pid ! {read, TId, Ref, self()},
                    handler(Client, Validator, Store, Reads, Writes, TId)
            end;
        {entry, Ref, Entry, Value} ->
            Client ! {value, Ref, Value},
            handler(Client, Validator, Store, [Entry|Reads], Writes, TId);
        {write, N, Value} ->
            Pid = store:lookup(N, Store),
            Added = lists:keystore(N, 1, Writes, {N, Pid, Value}),
            handler(Client, Validator, Store, Reads, Added, TId);
        {commit, Ref} ->
            Validator ! {validate, TId, Ref, Reads, Writes, Client};
        abort ->
            ok
    end.

-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

validator() ->
    receive
        {validate, TId, Ref, Reads, Writes, Client} ->
            Tag = make_ref(),
            send_finish_reads(Reads, TId, Tag),
            wait_finish_reads(length(Reads), Tag),
            send_checks(Writes, Tag),
            case receive_check(length(Writes), Tag) of
                ok ->
                    update(Writes),
                    Client ! {Ref, ok};
                abort ->
                    abort(Writes),
                    Client ! {Ref, abort}
            end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.

update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) ->
                  Entry ! {write, Value}
                  end,
                  Writes).

abort(Writes) ->
    lists:foreach(fun({_, Entry, _}) ->
                  Entry ! abortwrite
                  end,
                  Writes).

send_checks(Writes, Tag) ->
    Self = self(),
    lists:foreach(fun({_, Entry, _}) ->
                  Entry ! {check, Tag, Self}
                  end,
                  Writes).

receive_check(0, _) ->
    ok;
receive_check(N, Tag) ->
    receive
        {Tag, ok} ->
            receive_check(N-1, Tag);
        {Tag, abort} ->
            abort
    end.

send_finish_reads(Reads, TId, Tag) ->
    Self = self(),
    lists:foreach(fun(Entry) ->
                  Entry ! {finishread, TId, Tag, Self}
                  end,
                  Reads).

wait_finish_reads(0, _) ->
    ok;
wait_finish_reads(N, Tag) ->
    receive Tag -> wait_finish_reads(N-1, Tag) end.

-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, [], 0).

entry(Value, ActiveReads, NumberOfPendingWrites) ->
    receive
        {read, TId, Ref, From} when NumberOfPendingWrites == 0 ->
            From ! {entry, Ref, self(), Value},
            entry(Value, [TId|ActiveReads], NumberOfPendingWrites);
        {write, New} ->
            entry(New, ActiveReads, NumberOfPendingWrites-1);
        {finishread, TId, Ref, From} ->
            From ! Ref,
            entry(Value, [E || E <- ActiveReads, E /= TId], NumberOfPendingWrites);
        {check, Ref, From} ->
            case ActiveReads of
                [] -> From ! {Ref, ok};
                _ -> From ! {Ref, abort}
            end,
            entry(Value, ActiveReads, NumberOfPendingWrites+1);
        abortwrite -> entry(Value, ActiveReads, NumberOfPendingWrites-1);
        stop ->
            ok
    end.

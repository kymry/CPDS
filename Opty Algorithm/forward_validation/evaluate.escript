#!/usr/bin/env escript

main(_) ->
    run(1).

run(21) -> ok;
run(N) ->
    io:format("N: ~w ~n", [N]),
    opty:start(N * 10, 1000, 2, 2, 5),
    run(N+1).

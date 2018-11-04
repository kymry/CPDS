-mode(compile).
main(_) ->
    register(results, self()),
    paxy:start([10, 10, 10, 10, 10, 10]),
    Results = paxy:wait(),
    paxy:stop(),
    io:format("~p~n", [Results]),
    {_, Rounds} = check_results(Results),
    io:format(standard_error, "~w~n", [Rounds]).

check_results([{_, Decision, Round}]) ->
    {RoundNum, _} = Round,
    {Decision, RoundNum};
check_results([{_, Decision, Round}|T]) ->
    {RoundNum, _} = Round,
    {Decision, MaxRoundT} = check_results(T),
    {Decision, max(RoundNum, MaxRoundT)}.

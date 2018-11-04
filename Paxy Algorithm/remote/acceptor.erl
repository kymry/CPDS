-module(acceptor).
-export([start/2, init/2]).

start(Name, PanelId) ->
    % change here, if the processes should be changed on an other host
    % than localhost
    {ok, Hostname} = inet:gethostname(),
    spawn(list_to_atom(lists:concat(["paxy-acc@", Hostname])),
          ?MODULE, init, [Name, PanelId]).

init(Name, PanelId) ->
    Promised = order:null(),
    Voted = order:null(),
    Value = na,
    acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
    receive
        {prepare, Proposer, Round} ->
            case order:gr(Round, Promised) of
                true ->
                    send(Proposer, {promise, Round, Voted, Value}),
                                                % Update gui
                    if
                        Value == na ->
                            Colour = {0,0,0};
                        true ->
                            Colour = Value
                    end,
                    io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                              [Name, Round, Voted, Value]),
                    PanelId ! {updateAcc, "Voted: "
                               ++ io_lib:format("~p", [Voted]), "Promised: "
                               ++ io_lib:format("~p", [Round]), Colour},
                    acceptor(Name, Round, Voted, Value, PanelId);
                false ->
                    Proposer ! {sorry, {prepare, Round}},
                    io:format("[Acceptor ~w] Phase 1: sorry round ~w ~n",
                              [Name, Round]),
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        {accept, Proposer, Round, Proposal} ->
            case order:goe(Round, Promised) of
                true ->
                    send(Proposer, {vote, Round}),
                    case order:goe(Round, Voted) of
                        true ->
                        %% false ->
                                                % Update gui
                            io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                                      [Name, Promised, Round, Proposal]),
                            PanelId ! {updateAcc, "Voted: "
                                       ++ io_lib:format("~p", [Round]), "Promised: "
                                       ++ io_lib:format("~p", [Promised]), Proposal},
                            acceptor(Name, Promised, Round, Proposal, PanelId);
                        false ->
                        %% true ->
                                                % Update gui
                            io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                                      [Name, Promised, Voted, Value]),
                            PanelId ! {updateAcc, "Voted: "
                                       ++ io_lib:format("~p", [Voted]), "Promised: "
                                       ++ io_lib:format("~p", [Promised]), Value},
                            acceptor(Name, Promised, Voted, Value, PanelId)
                    end;
                false ->
                    Proposer ! {sorry, {accept, Round}},
                    io:format("[Acceptor ~w] Phase 2: sorry round ~w ~n",
                              [Name, Round]),
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        stop ->
            PanelId ! stop,
            ok
    end.

send(Name, Message) ->
    Name ! Message.

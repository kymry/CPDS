-module(acceptor).
-export([start/2]).

-ifndef(preparedelay).
-define(preparedelay, 1).
-endif.
-ifndef(acceptdelay).
-define(acceptdelay, 1).
-endif.
-ifndef(promisedrop).
-define(promisedrop, 0).
-endif.
-ifndef(votedrop).
-define(votedrop, 0).
-endif.

start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
    pers:open(Name),
    {Promised, Voted, Value, ReadPanelId} = pers:read(Name),
    if PanelId == na ->
            UsePanelId = ReadPanelId;
       true ->
            UsePanelId = PanelId
    end,
    pers:store(Name, Promised, Voted, Value, PanelId),
    acceptor(Name, Promised, Voted, Value, UsePanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
    receive
        {prepare, Proposer, Round} ->
            timer:sleep(rand:uniform(?preparedelay)),
            case order:gr(Round, Promised) of
                true ->
                    % must be before send, otherwise we could forget a promise
                    % if the acceptor crashes between sending and storing
                    pers:store(Name, Round, Voted, Value, PanelId),
                    send(Proposer, {promise, Round, Voted, Value}, ?promisedrop),
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
            timer:sleep(rand:uniform(?acceptdelay)),
            case order:goe(Round, Promised) of
                true ->
                    % must be before the send for the same reason as above
                    case order:goe(Round, Voted) of
                        true ->
                            pers:store(Name, Promised, Round, Proposal, PanelId);
                        _ -> ok
                    end,
                    send(Proposer, {vote, Round}, ?votedrop),
                    case order:goe(Round, Voted) of
                        true ->
                                                % Update gui
                            io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                                      [Name, Promised, Round, Proposal]),
                            PanelId ! {updateAcc, "Voted: "
                                       ++ io_lib:format("~p", [Round]), "Promised: "
                                       ++ io_lib:format("~p", [Promised]), Proposal},
                            acceptor(Name, Promised, Round, Proposal, PanelId);
                        false ->
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
            pers:close(Name),
            pers:delete(Name),
            PanelId ! stop,
            ok
    end.

send(Name, Message, Drop) ->
    P = rand:uniform(10),
    if P =< Drop ->
            io:format("message dropped~n");
       true ->
            Name ! Message
    end.

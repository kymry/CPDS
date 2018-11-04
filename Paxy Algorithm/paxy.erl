-module(paxy).
-export([start/1, stop/0, stop/1, wait/0, crash/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(C1, {255,255,0}).
-define(C2, {255,0,255}).
-define(C3, {0,255,0}).
-define(NUM_PROP, 3).
-define(NUM_ACC, 5).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
    AcceptorNames = lists:sublist(["Acceptor a", "Acceptor b", "Acceptor c",
    "Acceptor d", "Acceptor e", "Acceptor f", "Acceptor g", "Acceptor h", "Acceptor i", "Acceptor j"], ?NUM_ACC),
    AccRegister = lists:sublist([a, b, c, d, e, f, g, h, i, j], ?NUM_ACC),
    ProposerNames = lists:sublist([{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN},
                                   {"Proposer willard", ?BLUE}, {"Proposer david", ?C1},
                                   {"Proposer kymry", ?C2}, {"Proposer michael", ?C3}], ?NUM_PROP),
    PropInfo = lists:sublist([{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE},
                              {david, ?C1}, {kymry, ?C2}, {michael, ?C3}], ?NUM_PROP),
    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
    gui ! {reqState, self()},
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State,
            start_acceptors(AccIds, AccRegister),
            start_proposers(PropIds, PropInfo, AccRegister, Sleep)
    end,
    true.

start_acceptors(AccIds, AccReg) ->
    case AccIds of
        [] ->
            ok;
        [AccId|Rest] ->
            [RegName|RegNameRest] = AccReg,
            register(RegName, acceptor:start(RegName, AccId)),
            start_acceptors(Rest, RegNameRest)
    end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep) ->
    case PropIds of
        [] ->
            ok;
        [PropId|Rest] ->
            [{RegName, Colour}|RestInfo] = PropInfo,
            [FirstSleep|RestSleep] = Sleep,
            proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId),
            start_proposers(Rest, RestInfo, Acceptors, RestSleep)
        end.

wait()->
    wait(?NUM_PROP, []).
wait(0, L) -> L;
wait(N, L) ->
    receive
        {decided, Name, Decision, Round} ->
            wait(N-1, [{Name, Decision, Round} | L])
    end.


stop() ->
    stop(a),
    stop(b),
    stop(c),
    stop(d),
    stop(e),
    stop(gui).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

crash(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            pers:open(Name),
            {_, _, _, Pn} = pers:read(Name),
            Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
            dets:close(Name),
            unregister(Name),
            exit(Pid, "crash"),
            timer:sleep(2000),
            register(Name, acceptor:start(Name, na))
    end.

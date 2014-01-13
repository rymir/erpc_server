-module(erpc_acceptor_sys).
-copyright("2013, Erlang Solutions Ltd.").

%% API
-export([start_link/2, init/1]).


%% ===============================================================
%% External functions
%% ===============================================================

start_link(ServerPid, ListenSocket) ->
    proc_lib:start_link(?MODULE, init, [[self(), ServerPid, ListenSocket]]).

init([ParentPid, ServerPid, ListenSocket]) ->
    ok = proc_lib:init_ack(ParentPid, {ok, self()}),
    accept(ServerPid, ListenSocket).

%%%===============================================================
%%% Internal functions
%%%===============================================================

accept(ServerPid, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            ok = gen_tcp:controlling_process(ClientSocket, ServerPid),
            ok = erpc_srv:new_connection(ClientSocket),
            accept(ServerPid, ListenSocket);
        {error, closed} ->
            ok
    end.
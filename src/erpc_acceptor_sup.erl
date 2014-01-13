-module(erpc_acceptor_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-export([start_acceptor/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	RestartSpec = {simple_one_for_one, 10, 100},
    ERPCAcceptorSys = {erpc_acceptor_sys, {erpc_acceptor_sys, start_link, []}, transient, 5000, worker, [erpc_acceptor_sys]},
	ChildrenSpec = [ERPCAcceptorSys],
	{ok, {RestartSpec, ChildrenSpec}}.

start_acceptor(ListenSocket) ->
    supervisor:start_child(?MODULE, [ListenSocket]).

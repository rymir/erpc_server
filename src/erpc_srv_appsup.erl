%%% File    : erpc_srv_appsup.erl
%%% Description :

-module(erpc_srv_appsup).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Application callbacks
%%====================================================================
start(_Type, Args) ->
  case start_link(Args) of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

stop(_State) ->
  ok.


%%====================================================================
%% API functions
%%====================================================================
start_link(Args) ->
  supervisor:start_link(?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
  SupFlags = {one_for_all, 200, 600},
  {ok,Port} = application:get_env(erpc_server,server_port),
  ERPC  = {erpc_srv,{erpc_srv,start_link,[Port]},
	      transient,2000,worker,[erpc_srv]},
  ERPCAcceptorSup = {erpc_acceptor_sup, {erpc_acceptor_sup, start_link, []},
                    transient, infinity, supervisor, [erpc_acceptor_sup]},
  {ok,{SupFlags, [ERPCAcceptorSup, ERPC]}}.

%%====================================================================
%% Internal functions
%%====================================================================


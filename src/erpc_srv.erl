
%%%-------------------------------------------------------------------
%%% File    : erpc_srv.erl
%%% Author  : Rudolph van Graan <>
%%% Description :
%%%  This is the server that accepts connections from a
%%%  erpc client
%%%
%%% Created : 11 Nov 2006 by Rudolph van Graan <>
%%% Copyright: (C) 2006,2007 by Rudolph van Graan
%%%-------------------------------------------------------------------
-module(erpc_srv).

-export([start/1,
         start_link/1,
         new_connection/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {listen_socket, port}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------

start(Port) ->
    gen_server:start({local,?MODULE},?MODULE, [Port], []).

start_link(Port) ->
    gen_server:start_link({local,?MODULE},?MODULE, [Port], []).

new_connection(ClientSocket) ->
    ok = gen_server:call(?MODULE, {new_connection, ClientSocket}).

%%====================================================================
%% Server functions
%%====================================================================

init([Port]) ->
    process_flag(trap_exit,true),
    case gen_tcp:listen(Port,[{active,false},
                              {reuseaddr,true}]) of
        {ok,ListenSocket} ->
            ServerPid = self(),
            {ok, AcceptorPid} = erpc_acceptor_sup:start_acceptor(ListenSocket),
            ok = gen_tcp:controlling_process(ListenSocket, AcceptorPid),
            {ok, #state{listen_socket = ListenSocket,
                        port = Port}};
        {error,Reason} ->
            error_logger:warning_report([{subsystem,"ERPC SRV"},
                                         {description,"ERPC Unable to listen to ERPC Port"},
                                         {pid,self()},
                                         {reason,Reason},
                                         {port,Port}]),
            {ok, #state{port = Port}}
    end.

%%--------------------------------------------------------------------
handle_call({new_connection,ClientSocket}, _From, State) ->
  {ok,Pid} = erpc_connection_endpoint:start_link(ClientSocket),
  ok = gen_tcp:controlling_process(ClientSocket,Pid),
  {reply,ok,State}.

%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
terminate(_Reason, State) ->
  Socket = State#state.listen_socket,
  ok = gen_tcp:close(Socket).

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

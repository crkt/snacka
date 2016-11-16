-module(chat_server).
-behaviour(gen_server).
-export([start_link/1]).

%% Init
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([instance/2]).


start_link(Port) ->
    {ok, Pid} = client_register:start_link(),
    link(Pid),
    gen_server:start_link(?MODULE, [Port], []).

init([Port]) ->
    {ok, Sock} = gen_tcp:listen(Port, [binary, 
                                       {reuseaddr, true}, 
                                       {active, true}]),
    {ok, #{socket => Sock, acceptor => accept(Sock), workers => []}}.

handle_call(_Call, _Ref, State) ->
    {reply, ok, State}.

handle_cast({accepted, Pid}, #{socket := Sock, acceptor := Pid, workers := Ws} = State) -> 
    {noreply, State#{acceptor := accept(Sock), workers := [Pid | Ws]}};
handle_cast({closed, Pid}, #{workers := Ws} = State) ->
    {noreply, State#{workers := lists:delete(Pid, Ws)}};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _NewVsn, State) ->
    {ok, State}.
	

%% Start an acceptor.
accept(Sock) ->
    spawn_link(?MODULE, instance, [Sock, self()]).

instance(Sock, Controller) ->
    {ok, Client} = gen_tcp:accept(Sock),
    {ok, {Ip, Port}} = inet:peername(Client),
    gen_server:cast(Controller, {accepted, self()}),
    io:format("Accepted connection from: ~p/~p (~p)~n", [Ip, Port, Client]),
    client_register:register_client(Port, Client),
    loop(Client, Port, Controller).

loop(Client, Port, Controller) ->
    receive
        {tcp, Client, <<"quit\r\n">>} ->
            io:format("Quit was sent~n"),
            gen_server:cast(Controller, {closed, self()});
        {tcp, Client, Packet} ->
            io:format("Message ~p from ~p~n", [Packet, Client]),
            Registered = client_register:registered_clients(),
            [gen_tcp:send(Socket, Packet) || {_, Socket} <- Registered,
                                             Socket =/= Client],
            loop(Client, Port, Controller);
        {tcp_error, Client, _Reason} ->
            loop(Client, Port, Controller);
        {tcp_closed, Client} ->
            io:format("Closed ~p~n", [Client]),
            gen_server:cast(Controller, {closed, self()})
    end.

-module(snacka_sock_serv).
-behaviour(gen_server).

-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, Socket}.

handle_call(_E,_From,State) ->
    {noreply, State}.

handle_cast(accept, LSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(LSocket),
    snacka_sup:start_socket(),
    send(AcceptSocket, "Connected to snacka!", []),
    {noreply, AcceptSocket};
handle_cast({echo, Text}, Socket) ->
    send(Socket, Text, []),
    {noreply, Socket}.


handle_info({tcp, Socket, "quit"++_}, State) ->
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_info({tcp, _Socket, Str}, ASocket) ->
    Text = line(Str),
    gen_server:cast(self(), {echo, Text}),
    {noreply, ASocket};
handle_info({tcp_closed, _Socket}, S) ->
    {stop, normal, S};
handle_info({tcp_error, _Socket, _}, S) ->
    {stop, normal, S};
handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    ok.

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.

line(Str) ->
    Tokens = string:tokens(Str, "\r\n"),
    case Tokens of
        [] ->
            "";
        _ ->
            hd(Tokens)
    end.

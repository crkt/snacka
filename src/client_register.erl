-module(client_register).
-export([start_link/0,
         init/0,
         stop/0]).
-export([register_client/2,
         registered_clients/0,
         unregister_client/1,
         whereis_client/1]).

start_link() ->
    Pid = spawn(?MODULE, init, []),
    register(client_register, Pid),
    {ok, Pid}.

init() ->
    loop([]).

stop() ->
    exit(client_register).

%% Private loop for storage
loop(Clients) ->
    receive
        {register, Name, Socket, From} ->
            From ! {register_reply, ok},
            loop([{Name, Socket}] ++ Clients);
        {registered, From} ->
            From ! {registered_reply, Clients},
            loop(Clients);
        {whereis, Name, From} ->
            Client = proplists:get_value(Name, Clients),
            From ! {whereis_reply, Client},
            loop(Clients);                
        {unregister, Name, From} ->
            NewClients = proplists:delete(Name, Clients),
            From ! {unregister_reply, ok},
            loop(NewClients)
    end.
    

%% External API
register_client(Name, Socket) ->
    client_register ! {register, Name, Socket, self()},
    receive 
        {register_reply, Reply} ->
            Reply
    end.

registered_clients() ->
    client_register ! {registered, self()},
    receive 
        {registered_reply, Clients} ->
            Clients
    end.

whereis_client(Name) ->
    client_register ! {whereis, Name, self()},
    receive
        {whereis_reply, Client} ->
            Client
    end.

unregister_client(Name) ->
    client_register ! {unregister, Name, self()},
    receive
        {unregister_reply, Reply} ->
            Reply
    end.

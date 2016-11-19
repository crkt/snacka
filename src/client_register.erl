-module(client_register).
-behaviour(gen_server).

-export([start_link/0]).

%% Init && Callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% API
-export([register_client/2,
	 registered_clients/0,
	 whereis_client/1,
	 unregister_client/1]).
	 

start_link() ->
    gen_server:start_link({local, client_register}, ?MODULE, [], []).

init(_Args) ->
    {ok, []}.

handle_call({register, Name, Socket}, _From, State) ->
    {reply, ok, [{Name, Socket}] ++ State};
handle_call(registered, _From, State) ->
    {reply, State, State};
handle_call({whereis, Name}, _From, State) ->
    {reply, proplists:get_value(Name, State), State};
handle_call({unregister, Name}, _From, State) ->
    {reply, ok, proplists:delete(Name, State)}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _NewVsn, State) ->
    {ok, State}.


%% External API
register_client(Name, Socket) ->
    gen_server:call(client_register, {register, Name, Socket}).

registered_clients() ->
    gen_server:call(client_register, registered).

whereis_client(Name) ->
    gen_server:call(client_register, {whereis, Name}).

unregister_client(Name) ->
    gen_server:call(client_register, {unregister, Name}).

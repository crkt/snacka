%%%-------------------------------------------------------------------
%% @doc snacka top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(snacka_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
        start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),
    spawn_link(fun empty_listeners/0),
    Flags = #{strategy => simple_one_for_one,
             intensity => 60,
             period => 3600},
    Children = [child(snacka_sock_serv, [snacka_sock_serv], worker, [ListenSocket])],
    {ok, {Flags, Children}}.


%%====================================================================
%% Internal functions
%%====================================================================

child(Module, Modules, Type, Args) ->
    #{id => Module,
      start => {Module, start_link, Args},
      restart => permanent,
      shutdown => infinity,
      type => Type,
      modules => Modules}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.
    

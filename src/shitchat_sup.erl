-module(shitchat_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link(shitchat_sup, []).

init(_Args) ->
    {ok, Port} = application:get_env(port),
    Flags = #{strategy => one_for_one, intensity => 1, period => 5},
    Children = [child(client_register, worker, [], []),
		child(chat_server, worker, [Port], [])],
    {ok, {Flags, Children}}.


child(Module, Type, Args, Modules) ->
    #{id => Module,
      start => {Module, start_link, Args},
      restart => permanent,
      shutdown => 5000,
      type => Type,
      modules => Modules}.
    
    

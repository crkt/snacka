-module(data_access).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-export([persist_message/4]).

start_link(_) ->
    gen_server:start_link(?MODULE, [], []).

% Connects to database
init(_) ->
    {ok, Client} = cqerl:get_client({}),
    {ok, #{client => Client}}.


handle_call({persist_message, Room, Id, User, Data}, _Ref, _State) ->
    {ok, Client} = cqerl:get_client({}, [{{text_uuids, true}}]),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = "INSERT INTO users(id, name, password) VALUES(?, ?, ?);",
        values = [
            {id, new},
            {name, "matt"},
            {password, "qwerty"}
        ]
    });
handle_call(_Call, _Ref, State) ->
    {reply, ok, State}.

handle_cast(_Patt, _State) ->
    {noreply, ok}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, _NewVsn, State) ->
    {ok, State}.



%% External API
persist_message(Room, Id, User, Data) ->
    gen_server:call(data_access, {persist_message, Room, Id, User, Data}).

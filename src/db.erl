-module(db).
-include("db.hrl").

-export([insert_message/1]).
-export([select_all_messages/0]).

-include_lib("cqerl/include/cqerl.hrl").


insert_message(#s_message{} = Message) ->
    io:format("Inserting message to Database ~p", [Message]),
    {ok, Client} = cqerl:get_client({}),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = "INSERT INTO message(room_id, id, user, data) VALUES (?, ?, ?, ?)",
        values = [
            {room_id, Message#s_message.room_id},
            {id, Message#s_message.id},
            {user, Message#s_message.user_id},
            {data, Message#s_message.data}
        ]
    }).
    
select_all_messages() ->
    {ok, Client} = cqerl:get_client({}),
    {ok, Result} = cqerl:run_query(Client, "SELECT * FROM message;"),
    Result.
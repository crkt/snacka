-module(db).
-include("db.hrl").

-export([insert_message/1]).
-export([select_all_messages/0]).

-include_lib("cqerl/include/cqerl.hrl").


insert_message(#s_message{} = Message) ->        
    {ok, Client} = cqerl:get_client("127.0.0.1", [{keyspace, snacka}]),
    {ok, _} = cqerl:run_query(Client, #cql_query{
        statement = "INSERT INTO message(room_id, timestamp, id, user_id, data) VALUES (?, ?, ?, ?, ?)",
        values = [
            {room_id, Message#s_message.room_id},
            {timestamp, now},
            {id, Message#s_message.id},
            {user_id, Message#s_message.user_id},
            {data, Message#s_message.data}
        ]
    }).
    
select_all_messages() ->
    {ok, Client} = cqerl:get_client({}),
    {ok, Result} = cqerl:run_query(Client, "SELECT * FROM message;"),
    Result.
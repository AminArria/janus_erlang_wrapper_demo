-module(ws_handler).

-export([init/2,
         websocket_handle/2,
         websocket_info/2,
         websocket_init/1]).

init(Req, State) ->
  {cowboy_websocket, Req, State}.

websocket_init(State) ->
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  DecodedMsg = jsx:decode(Msg, [return_maps]),
  process(DecodedMsg, State);
websocket_handle(_, State) ->
  {ok, State}.

websocket_info({reply, Msg}, State) ->
  EncodedMsg = jsx:encode(Msg),
  {reply, {text, EncodedMsg}, State}.

% Internal
process(#{<<"request">> := <<"create_session">>}, State) ->
  {ok, JanusWs} = janus_sup:create_janus_connection(),
  JanusWs ! create_session,
  {ok, State#{janus_ws => JanusWs}};

process(#{<<"request">> := <<"create_handle">>,
          <<"session_id">> := SessionId}, #{janus_ws := JanusWs} = State) ->
  JanusWs ! {create_handle, SessionId},
  {ok, State};

process(#{<<"request">> := <<"create_room">>,
          <<"session_id">> := SessionId,
          <<"handle_id">> := HandleId}, #{janus_ws := JanusWs} = State) ->
  JanusWs ! {create_room, SessionId, HandleId},
  {ok, State};

process(#{<<"request">> := <<"join_publisher">>,
          <<"session_id">> := SessionId,
          <<"handle_id">> := HandleId,
          <<"room_id">> := RoomId}, #{janus_ws := JanusWs} = State) ->
  JanusWs ! {join_publisher, SessionId, HandleId, RoomId},
  {ok, State};

process(#{<<"request">> := <<"join_subscriber">>,
          <<"session_id">> := SessionId,
          <<"handle_id">> := HandleId,
          <<"room_id">> := RoomId,
          <<"feed_id">> := FeedId}, #{janus_ws := JanusWs} = State) ->
  JanusWs ! {join_subscriber, SessionId, HandleId, RoomId, FeedId},
  {ok, State};

process(#{<<"request">> := <<"publish">>,
          <<"session_id">> := SessionId,
          <<"handle_id">> := HandleId,
          <<"jsep">> := Offer}, #{janus_ws := JanusWs} = State) ->
  JanusWs ! {publish, SessionId, HandleId, Offer},
  {ok, State};

process(#{<<"request">> := <<"listen">>,
          <<"session_id">> := SessionId,
          <<"handle_id">> := HandleId,
          <<"jsep">> := Answer}, #{janus_ws := JanusWs} = State) ->
  JanusWs ! {listen, SessionId, HandleId, Answer},
  {ok, State};

process(#{<<"request">> := <<"trickle">>,
          <<"session_id">> := SessionId,
          <<"handle_id">> := HandleId,
          <<"candidate">> := Candidate}, #{janus_ws := JanusWs} = State) ->
  JanusWs ! {trickle, SessionId, HandleId, Candidate},
  {ok, State};

process(#{<<"request">> := <<"keepalive">>,
          <<"session_id">> := SessionId}, #{janus_ws := JanusWs} = State) ->
  JanusWs ! {keepalive, SessionId},
  {ok, State}.

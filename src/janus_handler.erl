-module(janus_handler).

-behaviour(websocket_client_handler).

-export([init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3,
         start_link/1]).

start_link(From) ->
  Opts = [{extra_headers, [{<<"Sec-WebSocket-Protocol">>, <<"janus-protocol">>}]}],
  websocket_client:start_link("ws://localhost:8188", ?MODULE, From, Opts).

% Websocket Handler
init(From, _ConnState) ->
  {ok, #{from => From}}.

websocket_handle({text, Msg}, _ConnState, State) ->
  DecodedMsg = jsx:decode(Msg, [return_maps]),
  process(DecodedMsg, State).

websocket_info(create_session, _ConnState, State) ->
  Msg = jsx:encode(#{janus => create, transaction => create_session}),
  {reply, {text, Msg}, State};

websocket_info({create_handle, SessionId}, _ConnState, State) ->
  Msg = jsx:encode(#{janus => attach,
                     plugin => <<"janus.plugin.videoroom">>,
                     transaction => create_handle,
                     session_id => SessionId}),
  {reply, {text, Msg}, State};

websocket_info({create_room, SessionId, HandleId}, _ConnState, State) ->
  Msg = jsx:encode(#{janus => message,
                     transaction => create_room,
                     body => #{request => create},
                     session_id => SessionId,
                     handle_id => HandleId}),
  {reply, {text, Msg}, State};

websocket_info({join_publisher, SessionId, HandleId, RoomId}, _ConnState, State) ->
  Body = #{request => join, ptype => publisher, room => RoomId},
  Msg = jsx:encode(#{janus => message,
                     transaction => join_publisher,
                     body => Body,
                     session_id => SessionId,
                     handle_id => HandleId}),
  {reply, {text, Msg}, State};

websocket_info({join_subscriber, SessionId, HandleId, RoomId, FeedId}, _ConnState, State) ->
  Body = #{request => join, ptype => subscriber, room => RoomId, feed => FeedId},
  Msg = jsx:encode(#{janus => message,
                     transaction => join_subscriber,
                     body => Body,
                     session_id => SessionId,
                     handle_id => HandleId}),
  {reply, {text, Msg}, State};

websocket_info({publish, SessionId, HandleId, Offer}, _ConnState, State) ->
  Msg = jsx:encode(#{janus => message,
                     transaction => publish,
                     body => #{request => publish},
                     jsep => Offer,
                     session_id => SessionId,
                     handle_id => HandleId}),
  {reply, {text, Msg}, State};

websocket_info({listen, SessionId, HandleId, Answer}, _ConnState, State) ->
  Msg = jsx:encode(#{janus => message,
                     transaction => listen,
                     body => #{request => start},
                     jsep => Answer,
                     session_id => SessionId,
                     handle_id => HandleId}),
  {reply, {text, Msg}, State};

websocket_info({trickle, SessionId, HandleId, Candidate}, _ConnState, State) ->
  Msg = jsx:encode(#{janus => trickle,
                     transaction => trickle,
                     candidate => Candidate,
                     session_id => SessionId,
                     handle_id => HandleId}),
  {reply, {text, Msg}, State};

websocket_info({keepalive, SessionId}, _ConnState, State) ->
  Msg = jsx:encode(#{janus => keepalive,
                     transaction => keepalive,
                     session_id => SessionId}),
  {reply, {text, Msg}, State}.

websocket_terminate(_Reason, _ConnState, _State) ->
  ok.

% Internal
process(#{<<"transaction">> := <<"create_session">>,
          <<"data">> := #{<<"id">> := SessionId}}, State) ->
  Msg = #{session_id => SessionId},
  send_msg(Msg, State);
process(#{<<"transaction">> := <<"create_handle">>,
          <<"data">> := #{<<"id">> := HandleId}}, State) ->
  Msg = #{handle_id => HandleId},
  send_msg(Msg, State);
process(#{<<"transaction">> := <<"create_room">>,
          <<"plugindata">> := #{<<"data">> := #{<<"room">> := RoomId}}}, State) ->
  Msg = #{room_id => RoomId},
  send_msg(Msg, State);
process(#{<<"transaction">> := <<"join_publisher">>,
          <<"plugindata">> := #{<<"data">> := #{<<"id">> := PubId}}}, State) ->
  Msg = #{publisher_id => PubId},
  send_msg(Msg, State);
process(#{<<"transaction">> := <<"join_subscriber">>,
          <<"jsep">> := Offer}, State) ->
  Msg = #{jsep => Offer},
  send_msg(Msg, State);
process(#{<<"transaction">> := <<"publish">>,
          <<"jsep">> := Answer}, State) ->
  Msg = #{jsep => Answer},
  send_msg(Msg, State);
process(_Msg, State) ->
  {ok, State}.

send_msg(Msg, #{from := From} = State) ->
  From ! {reply, Msg},
  {ok, State}.

-module(janus_handler).

-behaviour(websocket_client_handler).

-export([init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3,
         start_link/0]).

start_link() ->
  Opts = [{extra_headers, [{<<"Sec-WebSocket-Protocol">>, <<"janus-protocol">>}]}],
  websocket_client:start_link("ws://localhost:8188", ?MODULE, self(), Opts).

% Websocket Handler
init(From, _ConnState) ->
  {ok, #{from => From}}.

websocket_handle({text, Msg}, _ConnState, State) ->
  DecodedMsg = jsx:decode(Msg, [return_maps]),
  process(DecodedMsg, State).

websocket_info(create_session, _ConnState, State) ->
  Msg = jsx:encode(#{janus => create, transaction => create_session}),
  {reply, {text, Msg}, State};

websocket_info({create_handler, SessionId}, _ConnState, State) ->
  Msg = jsx:encode(#{janus => attach,
                     plugin => <<"janus.plugin.videoroom">>,
                     transaction => create_handler,
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

websocket_info({send_offer, SessionId, HandleId, Offer}, _ConnState, State) ->
  Msg = jsx:encode(#{janus => message,
                     transaction => send_offer,
                     body => #{request => publish},
                     jsep => Offer,
                     session_id => SessionId,
                     handle_id => HandleId}),
  {reply, {text, Msg}, State};

websocket_info({send_answer, SessionId, HandleId, Answer}, _ConnState, State) ->
  Msg = jsx:encode(#{janus => message,
                     transaction => send_answer,
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
          <<"data">> := #{<<"id">> := SessionId}}, #{from := From} = State) ->
  From ! {session_created, SessionId},
  {ok, State};
process(#{<<"transaction">> := <<"create_handle">>,
          <<"data">> := #{<<"id">> := HandleId}}, #{from := From} = State) ->
  From ! {handle_created, HandleId},
  {ok, State};
process(#{<<"transaction">> := <<"create_room">>,
          <<"plugindata">> := #{<<"data">> := #{<<"room">> := RoomId}}}, #{from := From} = State) ->
  From ! {room_created, RoomId},
  {ok, State};
process(#{<<"transaction">> := <<"join_publisher">>,
          <<"plugindata">> := #{<<"data">> := #{<<"id">> := PubId}}}, #{from := From} = State) ->
  From ! {publisher_joined, PubId},
  {ok, State};
process(#{<<"transaction">> := <<"join_subscriber">>,
          <<"jsep">> := Offer}, #{from := From} = State) ->
  From ! {jsep_offer, Offer},
  {ok, State};
process(#{<<"transaction">> := <<"publish">>,
          <<"jsep">> := Answer}, #{from := From} = State) ->
  From ! {jsep_answer, Answer},
  {ok, State};
process(#{<<"janus">> := <<"ack">>}, State) ->
  {ok, State}.

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
process(#{<<"request">> := <<"create_connection">>}, State) ->
  JanusWs = janus_handler:create_connection(),
  {ok, State#{janus_ws => JanusWs}}.

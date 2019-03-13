-module(janus_erlang_wrapper_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile(
    [{'_',
      [{"/websocket", ws_handler, #{}},
       {"/janus-client", cowboy_static, {priv_file, janus_erlang_wrapper, "static/janus-client/index.html"}},
       {"/api-client", cowboy_static, {priv_file, janus_erlang_wrapper, "static/api-client/index.html"}},
       {"/assets/[...]", cowboy_static, {priv_dir, janus_erlang_wrapper, "static"}}]
    }]),
    {ok, _} = cowboy:start_clear(my_http_listener,
      [{port, 8080}],
      #{env => #{dispatch => Dispatch}}
    ),
    janus_erlang_wrapper_sup:start_link().

stop(_State) ->
    ok.

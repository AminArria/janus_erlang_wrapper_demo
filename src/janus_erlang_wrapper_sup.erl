-module(janus_erlang_wrapper_sup).

-behaviour(supervisor).

-export([init/1,
         start_link/0]).

-export([]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  JanusSup =
    {janus_sup, {janus_sup, start_link, []},
     permanent, infinity, supervisor, [janus_sup]
    },
  {ok, {{one_for_all, 10, 10}, [JanusSup]}}.

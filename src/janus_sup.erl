-module(janus_sup).

-behaviour(supervisor).

-export([create_janus_connection/0,
         init/1,
         start_link/0]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create_janus_connection() ->
  supervisor:start_child(?MODULE, [self()]).

init(_) ->
  JanusHandler =
    {janus_handler, {janus_handler, start_link, []},
     transient, 5000, worker, [janus_handler]
    },
  {ok, {{simple_one_for_one, 10, 10}, [JanusHandler]}}.

-module(coding_challenge_app).

-behaviour(application).
-include("coding_challenge.hrl").
-define(API_LISTENER, ?APP_NAME).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  start_http().

stop(_State) ->
  ok.

%% internal functions

start_http() ->
  ApiPort = application:get_env(?APP_NAME, api_port, 8080),
  Dispatch =
    cowboy_router:compile(
      [
        {'_', [
          {<<"/api/1/json/[...]">>, api_h, []}
        ]}
      ]),

  {ok, _} =
    cowboy:start_clear(
      ?API_LISTENER,
      [{port, ApiPort}],
      #{
        env => #{dispatch => Dispatch}
      }
    ).


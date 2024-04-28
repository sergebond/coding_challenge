-module(coding_challenge_app).

-behaviour(application).
-include("coding_challenge.hrl").
-define(API_LISTENER, ?APP_NAME).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  case coding_challenge_sup:start_link() of
    {ok, _Pid} ->
      ok = start_http(),
      {ok, _Pid};
    {error, Reason} ->
      {error, Reason}
  end.

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
    ),
  ok.


-module(api_h).

-export([init/2]).

-define(DEFAULT_RESP_HEADER, #{<<"content-type">> => <<"application/json; charset=utf-8">>}).

init(Req0, Opts) ->
  Method = cowboy_req:method(Req0),
  Path = cowboy_req:path_info(Req0),
  {ok, Body, Req1} = cowboy_req:read_body(Req0),
  Req =
    case catch process(Method, Path, Body) of
      {ok, Result} ->
        RespBody = response_ok(Result),
        cowboy_req:reply(200, ?DEFAULT_RESP_HEADER, RespBody, Req1);
      {error, Code, Result} ->
        RespBody = response_error(Result),
        cowboy_req:reply(Code, ?DEFAULT_RESP_HEADER, RespBody, Req1);
      {'EXIT', Stack} ->
        Time = get_timestamp_in_millisec(),
        ErrorId = <<"#", (integer_to_binary(Time))/binary>>,
        logger:error("Server error ~s~n~p", [ErrorId, Stack]),
        cowboy_req:reply(500, ?DEFAULT_RESP_HEADER, Body, Req1)
    end,
  {ok, Req, Opts}.

process(<<"POST">>, Path, Body) ->
  BodyAsMap = jsx:decode(Body),
  api_engine:command(Path, BodyAsMap);
process(_, _Path, _Body) ->
  {ok, 405, [{<<"description">>, <<"metod_not_allowed">>}]}.

response_ok(Result) when is_binary(Result) -> Result;

response_ok(Result0) ->
  jsx:encode([{<<"status">>, <<"ok">>} | Result0]).
response_error(Result) ->
  jsx:encode([{<<"status">>, <<"error">>} | Result]).


get_timestamp_in_millisec() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).
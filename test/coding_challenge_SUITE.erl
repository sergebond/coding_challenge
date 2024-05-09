-module(coding_challenge_SUITE).
-author("sergeybondarchuk").
-include("coding_challenge.hrl").

-compile(export_all).

-define(JSON_BODY, <<"{\"tasks\": [
{\"name\": \"task-1\",\"command\": \"touch /tmp/file1\"},
{\"name\": \"task-2\",\"command\": \"cat /tmp/file1\",\"requires\":[\"task-3\"]},
{\"name\": \"task-3\",\"command\": \"echo 'Hello World!' > /tmp/file1\",\"requires\": [\"task-1\"]},
{\"name\": \"task-4\",\"command\": \"rm /tmp/file1\",\"requires\": [\"task-2\",\"task-3\"]}]}">>).

-define(JSON_BODY_CYCLE, <<"{\"tasks\":[
{\"command\":\"touch /tmp/file1\",\"name\":\"task-1\"},
{\"command\":\"cat /tmp/file1\",\"name\":\"task-2\",\"requires\":[\"task-3\"]},
{\"command\":\"echo 'Hello World!' > /tmp/file1\",\"name\":\"task-3\",\"requires\":[\"task-1\"]},
{\"command\":\"rm /tmp/file1\",\"name\":\"task-4\",\"requires\":[\"task-2\",\"task-3\"]},
{\"command\":\"rm /tmp/file1\",\"name\":\"task-5\",\"requires\":[\"task-6\"]},
{\"command\":\"rm /tmp/file1\",\"name\":\"task-6\",\"requires\":[\"task-7\",\"task-1\"]},
{\"command\":\"rm /tmp/file1\",\"name\":\"task-7\",\"requires\":[\"task-5\"]}]}">>).

-define(SORT_RESPONSE_EXAMPLE, "{\"status\":\"ok\",\"tasks\":[{\"command\":\"touch /tmp/file1\",\"name\":\"task-1\"},{\"command\":\"echo 'Hello World!' > /tmp/file1\",\"name\":\"task-3\"},{\"command\":\"cat /tmp/file1\",\"name\":\"task-2\"},{\"command\":\"rm /tmp/file1\",\"name\":\"task-4\"}]}").

-define(FOLD_RESPONSE_EXAMPLE, "#!/usr/bin/env bash\ntouch /tmp/file1\necho 'Hello World!' > /tmp/file1\ncat /tmp/file1\nrm /tmp/file1").

-define(CYCLE_RESP_EXAMPLE, "{\"status\":\"error\",\"description\":\"cycle\",\"cycle\":[\"task-5\",\"task-6\",\"task-7\",\"task-5\"]}").

-define(SORT_URL, "http://localhost:8080/api/1/json/sort").
-define(FOLD_URL, "http://localhost:8080/api/1/json/fold").


%% API
suite() ->
  [{timetrap, {seconds, 60}}].

all() ->
  [
    sort,
    fold,
    cycle
  ].

init_per_suite(Config) ->
  {ok, _Res} = application:ensure_all_started(?APP_NAME),
  Config.

end_per_suite(_Config) ->
  ok.

sort(_Config) ->
  case request(?SORT_URL, ?JSON_BODY) of
    {ok, Body} ->
      case Body of
        ?SORT_RESPONSE_EXAMPLE -> ok;
        _R ->
          throw({error, <<"wrong response body">>})
      end;
    {error, R} ->
      throw({error, R})
  end.

fold(_Config) ->
  case request(?FOLD_URL, ?JSON_BODY) of
    {ok, Body} ->
      case Body of
        ?FOLD_RESPONSE_EXAMPLE -> ok;
        _ ->
          throw({error, <<"wrong response body">>})
      end;
    {error, R} ->
      throw({error, R})
  end.

cycle(_Config) ->
  case request(?FOLD_URL, ?JSON_BODY_CYCLE) of
    {ok, _Body} ->
      throw({error, <<"wrong response body">>});
    {error, _R, ?CYCLE_RESP_EXAMPLE} -> ok
  end.


request(Url, Body) ->
  Headers = [],
  case httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
    {ok, {{_, 200, _}, _RespHeaders, RespBody}} ->
      ct:pal("Resp ~p", [RespBody]),
      {ok, RespBody};
    {ok, {{_, _Code, _Reas} = R, _RespHeaders, RespBody}} ->
      ct:pal("Reason: ~p", [R]),
      {error, R, RespBody};
    {error, R} ->
      ct:pal("Reason: ~p", [R]),
      {error, R}
  end.
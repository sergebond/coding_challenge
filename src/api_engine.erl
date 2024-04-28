-module(api_engine).
-author("sergeybondarchuk").

%% API
-export([command/2]).

command([<<"sort">>], BodyAsMap) ->
  Tasks = maps:get(<<"tasks">>, BodyAsMap),
  SortedTasks = sort_tasks(Tasks),
  {ok, [{<<"tasks">>, SortedTasks}]};

command([<<"fold">>], BodyAsMap) ->
  Tasks = maps:get(<<"tasks">>, BodyAsMap),
  SortedTasks = sort_tasks(Tasks),
  FoldedTasks = fold_tasks(SortedTasks),
  Script = as_bash_script(FoldedTasks),
  {ok, Script};

command(_, _BodyAsMap) ->
  {error, 405, [{<<"description">>, <<"command_not_found">>}]}.

%% private
sort_tasks(Tasks) ->
  sort_tasks(Tasks, []).
sort_tasks([], Queue) -> Queue;
sort_tasks([Task | Tasks], []) ->
  sort_tasks(Tasks, [Task]);
sort_tasks([Task0 | Tasks], Queue0) ->
  case maps:take(<<"requires">>, Task0) of
    error ->
      sort_tasks(Tasks, [Task0 | Queue0]);
    {Requires, Task} ->
      Acc = put_to_queue(Requires, Task, Queue0),
      sort_tasks(Tasks, Acc)
  end.

put_to_queue(Requires, Task, QueueTail) ->
  put_to_queue(Requires, Task, [], QueueTail).
put_to_queue([], Task, Head, Tail) ->
  lists:flatten([lists:reverse(Head), Task, Tail]);
put_to_queue(_Requires, Task, Head, []) ->
  lists:flatten([lists:reverse(Head), Task]);

put_to_queue(Requires0, Task, Head, [#{<<"name">> := Name} = Current | Tail]) ->
  Requires =
    case lists:member(Name, Requires0) of
      true ->
        lists:delete(Name, Requires0);
      false ->
        Requires0
    end,
  put_to_queue(Requires, Task, [Current | Head], Tail).

fold_tasks(Tasks) ->
  fold_tasks(Tasks, []).
fold_tasks([], Acc) ->
  bjoin(lists:reverse(Acc), "\n");
fold_tasks([#{<<"command">> := Command} | Tasks], Acc) ->
  fold_tasks(Tasks, [Command | Acc]).

bjoin([], _) -> <<>>;
bjoin([H | T], Separator) ->
  List = [H | [[Separator, X] || X <- T]],
  iolist_to_binary(List).

as_bash_script(FoldedTasks) ->
  iolist_to_binary([<<"#!/usr/bin/env bash">>, <<"\n">>, FoldedTasks]).
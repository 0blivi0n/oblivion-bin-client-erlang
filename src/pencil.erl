%%
%% Copyright 2015 Joaquim Rocha <jrocha@gmailbox.org>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(pencil).

-include("pencil.hrl").
-include_lib("mec/include/mec.hrl").

-export([start_link/1]).

-export([put/3, put/4]).
-export([get/2]).
-export([delete/2, delete/3]).
-export([version/2]).
-export([size/1]).
-export([flush/1]).
-export([caches/0]).
-export([keys/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link(Args) ->
  Server = proplists:get_value(server, Args),
  Port = proplists:get_value(port, Args),
  mec:open(Server, Port).

-spec put(Cache :: binary(), Key :: binary(), Value :: jsondoc:jsondoc()) ->
  {error, Reason :: any()} | {ok, Version :: integer()}.
put(Cache, Key, Value) when is_binary(Cache) andalso is_binary(Key) ->
  Json = jsondoc:ensure(Value),
  Response = call(?PUT_CMD, [<<"caches">>, Cache, <<"keys">>, Key], [], Json),
  process(?PUT_CMD, Response).

-spec put(Cache :: binary(), Key :: binary(), Value :: jsondoc:jsondoc(), Version :: integer()) ->
  {error, Reason :: any()} | {ok, NewVersion :: integer()}.
put(Cache, Key, Value, Version) when is_binary(Cache) andalso is_binary(Key) ->
  Params = #{?VERSION_TAG => Version},
  Json = jsondoc:ensure(Value),
  Response = call(?PUT_CMD, [<<"caches">>, Cache, <<"keys">>, Key], Params, Json),
  process(?PUT_CMD, Response).

-spec get(Cache :: binary(), Key :: binary()) ->
  {error, Reason :: any()} | {ok, Value :: jsondoc:jsondoc(), Version :: integer()}.
get(Cache, Key) when is_binary(Cache) andalso is_binary(Key) ->
  Response = call(?GET_CMD, [<<"caches">>, Cache, <<"keys">>, Key]),
  process(?GET_CMD, Response).

-spec delete(Cache :: binary(), Key :: binary()) ->
  {error, Reason :: any()} | ok.
delete(Cache, Key) when is_binary(Cache) andalso is_binary(Key) ->
  Response = call(?DELETE_CMD, [<<"caches">>, Cache, <<"keys">>, Key]),
  process(?DELETE_CMD, Response).

-spec delete(Cache :: binary(), Key :: binary(), Version :: integer()) ->
  {error, Reason :: any()} | ok.
delete(Cache, Key, Version) when is_binary(Cache) andalso is_binary(Key) ->
  Params = #{?VERSION_TAG => Version},
  Response = call(?DELETE_CMD, [<<"caches">>, Cache, <<"keys">>, Key], Params),
  process(?DELETE_CMD, Response).

-spec version(Cache :: binary(), Key :: binary()) ->
  {error, Reason :: any()} | {ok, Version :: integer()}.
version(Cache, Key) when is_binary(Cache) andalso is_binary(Key) ->
  Response = call(?VERSION_CMD, [<<"caches">>, Cache, <<"keys">>, Key]),
  process(?VERSION_CMD, Response).

-spec size(Cache :: binary()) ->
  {error, Reason :: any()} | {ok, Size :: integer()}.
size(Cache) when is_binary(Cache) ->
  Response = call(?GET_CMD, [<<"caches">>, Cache, <<"keys">>]),
  process(?SIZE_CMD, Response).

-spec flush(Cache :: binary()) ->
  {error, Reason :: any()} | ok.
flush(Cache) when is_binary(Cache) ->
  Response = call(?DELETE_CMD, [<<"caches">>, Cache, <<"keys">>]),
  process(?FLUSH_CMD, Response).

-spec caches() ->
  {error, Reason :: any()} | {ok, Caches :: list()}.
caches() ->
  Response = call(?GET_CMD, [<<"caches">>]),
  process(?CACHES_CMD, Response).

-spec keys(Cache :: binary()) ->
  {error, Reason :: any()} | {ok, Keys :: list()}.
keys(Cache) when is_binary(Cache) ->
  Params = #{?LIST_TAG => true},
  Response = call(?GET_CMD, [<<"caches">>, Cache, <<"keys">>], Params),
  process(?KEYS_CMD, Response).

%% ====================================================================
%% Internal functions
%% ====================================================================

process(_Cmd, Error = {error, _Reason}) -> Error;
process(Cmd, {ok, #mercury_reply{status = Status, params = Params, payload = Payload}}) ->
  process_response(Cmd, Status, Params, Payload).

% GET
process_response(?GET_CMD, 200, #{?VERSION_TAG := Version}, Payload) ->
  {ok, Payload, Version};
% PUT
process_response(?PUT_CMD, 201, #{?VERSION_TAG := Version}, _Payload) ->
  {ok, Version};
% DELETE
process_response(?DELETE_CMD, 200, _Params, _Payload) -> ok;
% VERSION
process_response(?VERSION_CMD, 200, #{?VERSION_TAG := Version}, _Payload) ->
  {ok, Version};
% SIZE
process_response(?SIZE_CMD, 200, _Params, Size) ->
  {ok, Size};
% FLUSH
process_response(?FLUSH_CMD, 202, _Params, _Payload)  -> ok;
% KEY LIST
process_response(?KEYS_CMD, 200, _Params, Payload) ->
  List = jsondoc:get_value(<<"keys">>, Payload),
  {ok, List};
% CACHE LIST
process_response(?CACHES_CMD, 200, _Params, Payload) ->
  CacheList = jsondoc_query:select(Payload, [<<"caches">>, <<"cache">>]),
  {ok, CacheList};
% -else-
process_response(_Cmd, _Status, _Params, empty)       -> {error, invalid_response};
process_response(_Cmd, _Status, _Params, Payload)     -> process_error(Payload).

process_error(Error) ->
  Reason = jsondoc:get_value(?ERROR_REASON_TAG, Error),
  {error, Reason}.

call(Operation, Resource) ->
  poolboy:transaction(?MODULE, fun(Worker) ->
    Request = #mercury_request{
      operation = Operation,
      resource = Resource
    },
    mec:call(Worker, Request)
  end).

call(Operation, Resource, Params) ->
  poolboy:transaction(?MODULE, fun(Worker) ->
    Request = #mercury_request{
      operation = Operation,
      resource = Resource,
      params = Params
    },
    mec:call(Worker, Request)
  end).

call(Operation, Resource, Params, Payload) ->
  poolboy:transaction(?MODULE, fun(Worker) ->
    Request = #mercury_request{
      operation = Operation,
      resource = Resource,
      params = Params,
      payload = Payload
    },
    mec:call(Worker, Request)
  end).
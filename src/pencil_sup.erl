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
-module(pencil_sup).

-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
	supervisor:start_link(?MODULE, []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
	Server = application:get_env(pencil, oblivion_server, "127.0.0.1"),
	Port = application:get_env(pencil, oblivion_port, 12521),
	WorkerCount = erlang:system_info(schedulers),
	PoolArgs = [
			{name,{local, pencil}}, 
			{worker_module, pencil}, 
			{size, WorkerCount}, 
			{max_overflow, WorkerCount * 2}],
	WorkerArgs = [
			{server, Server},
			{port, Port}],
	PoolSpecs =  poolboy:child_spec(pencil, PoolArgs, WorkerArgs),
	{ok, {{one_for_one, 5, 60}, [PoolSpecs]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================



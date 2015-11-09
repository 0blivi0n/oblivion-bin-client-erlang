**PENCIL**
==========
*0blivi0n's binary protocol client*


Installation
------------

Using rebar:

```erlang
{deps, [
	{pencil, ".*", {git, "https://github.com/0blivi0n/pencil.git", "master"}}
]}.
```


Configuration
-------------

PENCIL provides two parameters to specify the Oblivion's server IP and port.

To change the default values, set value of the properties "oblivion_server" and "oblivion_port" on your config file.

```erlang
[
{pencil, [
		{oblivion_server, "127.0.0.1"},
		{oblivion_port, 12521}
	]
}
].
```


Starting the client
-------------------

PENCIL depends on three apps [MEC](https://github.com/0blivi0n/mec), [jsondoc](https://github.com/jjmrocha/jsondoc) and 
[poolboy](https://github.com/devinus/poolboy), the 3 apps must be running before starting PENCIL.

```erlang
1> application:start(mec).  
ok
2> application:start(poolboy).
ok
3> application:start(jsondoc).
ok
4> application:start(pencil).    
ok
```


API
---

The module ```pencil``` provides the following functions to interact with the 0blivi0n's server.

```erlang
put(Cache :: binary(), Key :: binary(), Value :: jsondoc:jsondoc()) -> 
	{error, Reason :: any()} | {ok, Version :: integer()}.
	
put(Cache :: binary(), Key :: binary(), Value :: jsondoc:jsondoc(), Version :: integer()) -> 
	{error, Reason :: any()} | {ok, NewVersion :: integer()}.
	
get(Cache :: binary(), Key :: binary()) ->
	{error, Reason :: any()} | {ok, Value :: jsondoc:jsondoc(), Version :: integer()}.
	
delete(Cache :: binary(), Key :: binary()) ->
	{error, Reason :: any()} | ok.
	
delete(Cache :: binary(), Key :: binary(), Version :: integer()) ->
	{error, Reason :: any()} | ok.
	
version(Cache :: binary(), Key :: binary()) -> 
	{error, Reason :: any()} | {ok, Version :: integer()}.
	
size(Cache :: binary()) -> 
	{error, Reason :: any()} | {ok, Size :: integer()}.
	
flush(Cache :: binary()) -> 
	{error, Reason :: any()} | ok.
	
caches() -> 
	{error, Reason :: any()} | {ok, Caches :: list()}.
	
keys(Cache :: binary()) -> 
	{error, Reason :: any()} | {ok, Keys :: list()}.
```


Examples
--------

### List of caches

```erlang
5> pencil:caches().
{ok,[<<"test">>]}
```

### Store data

```erlang
6> pencil:put(<<"test">>, <<"jr">>, <<"Joaquim Rocha">>).
{ok,1447028787208699}
7> pencil:put(<<"test">>, <<"jr">>, <<"Joaquim JR Rocha">>, 1447028787208699).
{ok,1447028814375356}
8> pencil:put(<<"test">>, <<"array">>, [1, 3, true, <<"Text">>, 123.4]).      
{ok,1447028874693208}
```

### Retrieve data

```erlang
9> pencil:get(<<"test">>, <<"jr">>). 
{ok,<<"Joaquim JR Rocha">>,1447028814375356}
10> pencil:version(<<"test">>, <<"array">>).
{ok,1447028874693208}
11> pencil:keys(<<"test">>).                
{ok,[<<"array">>,<<"jr">>]}
```

### Delete data

```erlang
12> pencil:delete(<<"test">>, <<"jr">>).
ok
13> pencil:delete(<<"test">>, <<"array">>, 1447028874693208).
ok
14> pencil:flush(<<"test">>).
ok
```


License
-------
[Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)

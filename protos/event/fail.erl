-module(fail).

-export([run/0]).

run() ->
	throw(howdy).
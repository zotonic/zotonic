-module(iconv_app).

-export([start/2, stop/0]).


start(normal, []) ->
	iconv:start().

stop() ->
	iconv:stop().

%% @doc 'trim' filter, removes whitespace at the start and end of a string
-module(filter_trim).

-export([trim/2]).

trim(Input, _Context) ->
    z_string:trim(Input).
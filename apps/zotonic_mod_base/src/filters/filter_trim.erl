%% @doc 'trim' filter, removes whitespace at the start and end of a string
-module(filter_trim).

-export([trim/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

trim(undefined, _Context) ->
    <<>>;
trim(#trans{} = Tr, Context) ->
    trim(z_trans:lookup_fallback(Tr, Context), Context);
trim(Input, _Context) ->
    z_string:trim(Input).

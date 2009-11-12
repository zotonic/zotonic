%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-17
%%
%% @doc Utility functions for html processing.

%% Copyright 2009 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_html).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    escape_props/1,
    escape/1,
    unescape/1,
    strip/1
]).

-include_lib("zotonic.hrl").


%% @doc Escape all properties used for an update statement. Only leaves the body property in tact.
%% @spec escape_props(PropertyList) -> PropertyList
escape_props(Props) ->
    escape_props(Props, []).

    escape_props([], Acc) ->
        Acc;
    escape_props([{_K,V} = Prop|T], Acc) when is_float(V); is_integer(V); is_atom(V) -> 
        escape_props(T, [Prop|Acc]);
    escape_props([{body, V}|T], Acc) ->
        escape_props(T, [{body, V} | Acc]);
    escape_props([{K, V}|T], Acc) ->
        escape_props(T, [{K, escape_value(V)} | Acc]).

    escape_value({trans, Texts}) ->
        {trans, escape_props(Texts)};
    escape_value(V) when is_list(V) ->
        try
            escape_value(iolist_to_binary(V))
        catch _:_ ->
            V
        end;
    escape_value(B) when is_binary(B) ->
        escape(B);
    escape_value(V) -> 
        V.


%% @doc Escape a string so that it is valid within HTML/ XML.
%% @spec escape(iolist()) -> iolist()
escape(undefined) -> 
    undefined;
escape(<<>>) -> 
    <<>>;
escape([]) ->
    [];
escape(L) when is_list(L) ->
    escape(list_to_binary(L));
escape(B) when is_binary(B) ->
    escape(B, <<>>).

    escape(<<>>, Acc) -> 
        Acc;
    escape(<<"&euro;", T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "€">>);
    escape(<<$&, T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "&amp;">>);
    escape(<<$<, T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "&lt;">>);
    escape(<<$>, T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "&gt;">>);
    escape(<<$", T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "&quot;">>);
    escape(<<$', T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "&#39;">>);
    escape(<<C, T/binary>>, Acc) ->
        escape(T, <<Acc/binary, C>>).


%% @doc Unescape - reverses the effect of escape.
%% @spec escape(iolist()) -> iolist()
unescape(undefined) -> 
    undefined;
unescape(<<>>) -> 
    <<>>;
unescape([]) ->
    [];
unescape(L) when is_list(L) ->
    unescape(list_to_binary(L));
unescape(B) when is_binary(B) ->
    unescape(B, <<>>).

    unescape(<<>>, Acc) -> 
        Acc;
    unescape(<<"&amp;", T/binary>>, Acc) ->
        unescape(T, <<Acc/binary, "&">>);
    unescape(<<"&quot;", T/binary>>, Acc) ->
        unescape(T, <<Acc/binary, "\"">>);
    unescape(<<"&#39;", T/binary>>, Acc) ->
        unescape(T, <<Acc/binary, "'">>);
    unescape(<<"&lt;", T/binary>>, Acc) ->
        unescape(T, <<Acc/binary, "<">>);
    unescape(<<"&gt;", T/binary>>, Acc) ->
        unescape(T, <<Acc/binary, ">">>);
    unescape(<<"&euro;", T/binary>>, Acc) ->
        unescape(T, <<Acc/binary, "€">>);
    unescape(<<C, T/binary>>, Acc) ->
        unescape(T, <<Acc/binary, C>>).


%% @doc Strip all html elements from the text. Simple parsing is applied to find the elements. Does not escape the end result.
%% @spec strip(iolist()) -> iolist()
strip(undefined) ->
    [];
strip(<<>>) ->
    <<>>;
strip([]) ->
    [];
strip(Html) when is_binary(Html) ->
    strip(Html, in_text, <<>>);
strip(L) when is_list(L) ->
    strip(list_to_binary(L)).

strip(<<>>, _, Acc) -> Acc;
strip(<<$<,T/binary>>, in_text, Acc) ->
    strip(T, in_tag, Acc);
strip(<<$>,T/binary>>, in_tag, Acc) ->
    strip(T, in_text, Acc);
strip(<<$>,T/binary>>, State, Acc) ->
    strip(T, State, Acc);
strip(<<$<,T/binary>>, State, Acc) ->
    strip(T, State, Acc);
strip(<<$\\,_,T/binary>>, in_dstring, Acc) ->
    strip(T, in_dstring, Acc);
strip(<<$\\,_,T/binary>>, in_sstring, Acc) ->
    strip(T, in_sstring, Acc);
strip(<<$",T/binary>>, in_tag, Acc) ->
    strip(T, in_dstring, Acc);
strip(<<$",T/binary>>, in_dstring, Acc) ->
    strip(T, in_tag, Acc);
strip(<<$',T/binary>>, in_tag, Acc) ->
    strip(T, in_sstring, Acc);
strip(<<$',T/binary>>, in_sstring, Acc) ->
    strip(T, in_tag, Acc);
strip(<<H,T/binary>>, in_text, Acc) ->
    strip(T, in_text, <<Acc/binary, H>>);
strip(<<_,T/binary>>, State, Acc) ->
    strip(T, State, Acc).

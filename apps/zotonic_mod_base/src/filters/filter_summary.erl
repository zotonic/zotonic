%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2022 Arjan Scherpenisse
%% @doc Give a plaintext summary of the resource. Takes either the summary or, if non existent, a part of the body text.

%% Copyright 2010-2022 Arjan Scherpenisse
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

-module(filter_summary).
-export([summary/2, summary/3, strip/2]).

summary(undefined, _Context) ->
    undefined;
summary(RId, Context) ->
    summary(RId, 200, Context).

summary(undefined, _N, _Context) ->
    undefined;
summary(RId, N, Context) ->
    case m_rsc:rid(RId, Context) of
        undefined ->
            undefined;
        Id ->
            Len = z_convert:to_integer(N),
            S = get_summary_text(Id, Context),
            S1 = case z_utils:is_empty(S) of
                    true ->
                        Body = m_rsc:p(Id, body, Context),
                        Body1 = z_trans:lookup_fallback(Body, Context),
                        z_string:trim_left(strip(Body1, Len + 50));
                    false ->
                        S
                end,
            z_string:trim(z_string:truncate(S1, Len))
    end.

get_summary_text(Id, Context) ->
    case m_rsc:p(Id, summary, Context) of
        {trans, _} = T -> z_trans:lookup_fallback(T, Context);
        Other -> Other
    end.

%% @doc Strip all html elements from the text. Simple parsing is applied to find the elements.
%%      All aside content a deleted. Does not escape the end result.
-spec strip( binary() | undefined, integer() ) -> binary().
strip(undefined, _N) ->
    <<>>;
strip(<<>>, _N) ->
    <<>>;
strip(Html, N) when is_binary(Html) ->
    strip(Html, <<>>, N).

strip(_, Acc, N) when N =< 0 ->
    Acc;
strip(<<>>, Acc, _N) ->
    Acc;
strip(<<"<wbr>",T/binary>>, Acc, N) ->
    strip(T, Acc, N);
strip(<<"</span>",T/binary>>, Acc, N) ->
    strip(T, Acc, N);
strip(<<"</a>",T/binary>>, Acc, N) ->
    strip(T, Acc, N);
strip(<<"<aside",T/binary>>, Acc, N) ->
    strip_aside(T, Acc, N);
strip(<<"<",T/binary>>, Acc, N) ->
    strip_tag(T, Acc, N);
strip(<<H/utf8,T/binary>>, Acc, N) ->
    strip(T, <<Acc/binary, H/utf8>>, N-1);
strip(<<_,T/binary>>, Acc, N) ->
    % Drop non-utf8 data
    strip(T, Acc, N).


strip_tag(<<>>, Acc, _N) ->
    Acc;
strip_tag(<<">">>, Acc, _N) ->
    Acc;
strip_tag(<<">", WS, T/binary>>, Acc, N) when WS =< 32 ->
    strip(T, <<Acc/binary, WS>>, N-1);
strip_tag(<<">", T/binary>>, <<>>, N) ->
    strip(T, <<>>, N);
strip_tag(<<">", T/binary>>, Acc, N) ->
    case T of
        <<"</", _/binary>> ->
            strip(T, Acc, N);
        _ ->
            case binary:last(Acc) of
                C when C =< 32 ->
                    strip(T, Acc, N);
                _ ->
                    strip(T, <<Acc/binary, " ">>, N-1)
            end
    end;
strip_tag(<<$",T/binary>>, Acc, N) ->
    strip_dstring(T, Acc, N);
strip_tag(<<$',T/binary>>, Acc, N) ->
    strip_sstring(T, Acc, N);
strip_tag(<<_, T/binary>>, Acc, N) ->
    strip_tag(T, Acc, N).

strip_dstring(<<>>, Acc, _) ->
    Acc;
strip_dstring(<<$\\, _, T/binary>>, Acc, N) ->
    strip_dstring(T, Acc, N);
strip_dstring(<<$",T/binary>>, Acc, N) ->
    strip_tag(T, Acc, N);
strip_dstring(<<_,T/binary>>, Acc, N) ->
    strip_dstring(T, Acc, N).

strip_sstring(<<>>, Acc, _) ->
    Acc;
strip_sstring(<<$\\, _, T/binary>>, Acc, N) ->
    strip_sstring(T, Acc, N);
strip_sstring(<<$',T/binary>>, Acc, N) ->
    strip_tag(T, Acc, N);
strip_sstring(<<_,T/binary>>, Acc, N) ->
    strip_sstring(T, Acc, N).

strip_aside(<<>>, Acc, _N) ->
    Acc;
strip_aside(<<"</aside>", T/binary>>, Acc, N) ->
    strip(T, Acc, N);
strip_aside(<<_, T/binary>>, Acc, N) ->
    strip_aside(T, Acc, N).

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Model for dispatching requests.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(m_dispatch).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

-export([
    m_get/3,

    dispatch_url/2,
    dispatch_path/2
]).

m_get([ <<"url">> ], #{ payload := #{ <<"url">> := Url } }, Context) ->
    dispatch_url(Url, Context);
m_get([ <<"url">>, Url ], _Msg, Context) ->
    dispatch_url(Url, Context);
m_get([ <<"path">> ], #{ payload := #{ <<"path">> := Path } }, Context) ->
    dispatch_path(Path, Context);
m_get([ <<"path">> | Path ], _Msg, Context) ->
    dispatch_path(Path, Context);
m_get([ <<"url_for">>, Name | Rest ], #{ payload := Payload }, Context) ->
    case url_for(Name, Payload, Context) of
        undefined -> {error, enoent};
        Url -> {ok, {Url, Rest}}
    end;
m_get([ <<"abs_url_for">>, Name | Rest ], #{ payload := Payload }, Context) ->
    case url_for(Name, Payload, Context) of
        undefined -> {error, enoent};
        Url -> {ok, {z_context:abs_url(Url, Context), Rest}}
    end;
m_get(_, _Msg, _Context) ->
    {error, enoent}.

url_for(Name, undefined, Context) ->
    z_dispatcher:url_for(Name, Context);
url_for(Name, Args, Context) when is_map(Args) ->
    url_for(Name, maps:to_list(Args), Context);
url_for(Name, Args, Context) when is_list(Args) ->
    ArgsList = lists:map(
        fun
            ({K, V} = KV) when is_binary(K) ->
                try
                    {binary_to_existing_atom(K, utf8), V}
                catch
                    _:_ -> KV
                end;
            ({K, _V} = KV) when is_atom(K) ->
                KV;
            (K) when is_atom(K) ->
                {K, true};
            (K) when is_binary(K) ->
                try
                    {binary_to_existing_atom(K, utf8), true}
                catch
                    _:_ -> {K, true}
                end
        end,
        Args),
    z_dispatcher:url_for(Name, ArgsList, none, Context).

dispatch_url(undefined, _Context) ->
    {error, enoent};
dispatch_url(Url, Context) when is_binary(Url) ->
    case z_sites_dispatcher:dispatch_url(Url) of
        {ok, #{
            context := UrlContext
        } = Disp} ->
            Site = z_context:site(Context),
            UrlSite = z_context:site(UrlContext),
            if
                Site =:= UrlSite -> cleanup_disp(Disp);
                true -> {error, enoent}
            end;
        {error, _} ->
            {error, enoent}
    end;
dispatch_url(_, _Context) ->
    {error, enoent}.

dispatch_path(undefined, _Context) ->
    {error, enoent};
dispatch_path(Path, Context) when is_list(Path) ->
    Path1 = iolist_to_binary([ [ <<"/">>, z_url:percent_encode(P) ] || P <- Path ]),
    dispatch_path(Path1, Context);
dispatch_path(Path, Context) when is_binary(Path) ->
    case z_sites_dispatcher:dispatch_path(Path, Context) of
        {ok, Disp} -> cleanup_disp(Disp);
        {error, _} -> {error, enoent}
    end;
dispatch_path(_, _Context) ->
    {error, enoent}.

cleanup_disp(#{ context := DispContext} = Disp) ->
    Disp1 = maps:without([ context ], Disp),
    Disp2 = Disp1#{
        language => z_context:language(DispContext)
    },
    Disp3 = maybe_encode_strings(Disp2),
    {ok, {Disp3, []}}.

maybe_encode_strings(#{ controller_options := Opts } = Disp) ->
    Opts1 = lists:map(
        fun
            ({K, [ C | _ ] = V}) when is_integer(C) ->
                try
                    {K, unicode:characters_to_binary(V, utf8)}
                catch
                    _:_ -> {K, V}
                end;
            ({template, {cat, Tpl}}) ->
                {template, #{
                    is_catinclude => true,
                    template => unicode:characters_to_binary(Tpl, utf8)
                }};
            (KV) -> KV
        end,
        Opts),
    Disp#{
        controller_options => Opts1
    };
maybe_encode_strings(Disp) ->
    Disp.


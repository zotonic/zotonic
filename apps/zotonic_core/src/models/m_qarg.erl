%% @author Driebit <tech@driebit.nl>
%% @copyright 2025 Driebit
%% @doc Model for safely accessing query arguments.
%% @end

-module(m_qarg).

-behaviour(zotonic_model).

-export([
    m_get/3
]).

-include_lib("zotonic.hrl").

%% @doc This model provides a safer alternative to 'q' and 'm.req' for obtaining
%% the request's query arguments from templates.
%% It does so by implementing convenient syntax for the recommendations in the
%% "Special care" of the XSS prevention documentation:
%% https://zotonic.com/cookbook/2325/security-templates-and-xss-prevention
%%
%% NOTE: this model differs from 'q' in that it _only_ looks at the query
%% arguments in the URL (and not at arguments from API calls or other sources).
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().

% Template syntax: m.qarg.<arg_name>
% Replacement for: q.<arg_name>|escape
%
% Get the escaped value for the given key from the query arguments.
m_get([ ArgName ], _Msg, Context) when is_binary(ArgName) ->
    Qs = req_qs(Context),
    Result = case proplists:get_value(ArgName, Qs, undefined) of
        Value when is_binary(Value) ->
            filter_escape:escape(Value, Context);
        _ ->
            undefined
    end,
    {ok, {Result, []}};

% Template syntax: m.qarg.<arg_name>.all
%
% Get all the escaped values for the given key from the query arguments.
m_get([ ArgName, <<"all">> | Rest ], _Msg, Context) when is_binary(ArgName) ->
    Qs = req_qs(Context),
    All = proplists:get_all_values(ArgName, Qs),
    Result = lists:map(fun(Val) -> filter_escape:escape(Val, Context) end, All),
    {ok, {Result, Rest}};

% Template syntax: m.qarg.<arg_name>.id
% Replacement for: m.rsc[q.<arg_name>].id
%
% Get the resource ID from the value of the given key in the query arguments.
% This can be chained to access the properties of the resource.
m_get([ ArgName, <<"id">> | Rest ], _Msg, Context) when is_binary(ArgName) ->
    Qs = req_qs(Context),
    ArgValue = proplists:get_value(ArgName, Qs, undefined),
    m_rsc:m_get([ArgValue, <<"id">> | Rest], undefined, Context);

m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


-spec req_qs( z:context() ) -> list({binary(), binary()}).
req_qs(Context) ->
    case m_req:get(qs, Context) of
        Qs when is_list(Qs) -> Qs;
        _ -> []
    end.

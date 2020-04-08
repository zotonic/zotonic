%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Model for server side data storage.

%% Copyright 2020 Marc Worrell
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

-module(m_server_storage).

-behaviour(zotonic_model).

-export([
    m_get/3,
    m_post/3,
    m_delete/3,

    lookup/2,
    store/3,
    delete/2
    ]).

m_get([ Key | Rest ], _Msg, Context) ->
    case lookup(Key, Context) of
        {ok, Value} ->
            {ok, {Value, Rest}};
        {error, _} ->
            {ok, {undefined, Rest}}
    end.

-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | {error, term()}.
m_post([ Key ], #{ payload := Payload }, Context) ->
    store(Key, Payload, Context).

-spec m_delete( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | {error, term()}.
m_delete([ Key ], _Msg, Context) ->
    delete(Key, Context).


-spec store( term(), term(), z:context() ) -> ok | {error, no_session | not_found | term()}.
store(Key, Value, Context) ->
    case session_id(Context) of
        {ok, Sid} ->
            case z_server_storage:store(Sid, Key, Value, Context) of
                ok ->
                    ok;
                {error, no_session} ->
                    _ = mod_server_storage:start_session(Sid, Context),
                    z_server_storage:store(Sid, Key, Value, Context)
            end;
        {error, _} = Error ->
            Error
    end.


-spec lookup( term(), z:context() ) -> ok | {error, no_session | not_found | term()}.
lookup(Key, Context) ->
    case session_id(Context) of
        {ok, Sid} ->
            z_server_storage:lookup(Sid, Key, Context);
        {error, _} = Error ->
            Error
    end.

-spec delete( term(), z:context() ) -> ok | {error, no_session | not_found | term()}.
delete(Key, Context) ->
    case session_id(Context) of
        {ok, Sid} ->
            z_server_storage:delete(Sid, Key, Context);
        {error, _} = Error ->
            Error
    end.

session_id(Context) ->
    case z_context:get(auth_options, Context) of
        #{ sid := Sid } ->
            {ok, Sid};
        _ ->
            {error, no_session}
    end.

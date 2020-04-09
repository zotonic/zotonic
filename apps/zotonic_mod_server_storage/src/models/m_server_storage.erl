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

    ping/1,
    lookup/1,
    lookup/2,
    store/3,
    delete/2,
    delete/1,

    secure_lookup/1,
    secure_lookup/2,
    secure_store/3,
    secure_delete/2,
    secure_delete/1
    ]).


-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([], _Msg, Context) ->
    case lookup(Context) of
        {ok, Values} ->
            {ok, {Values, []}};
        {error, _} = Error ->
            Error
    end;
m_get([ Key | Rest ], _Msg, Context) ->
    case lookup(z_convert:to_binary(Key), Context) of
        {ok, Value} ->
            {ok, {Value, Rest}};
        {error, _} = Error ->
            Error
    end.

-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | {error, term()}.
m_post([ Key ], #{ payload := Payload }, Context) ->
    store(Key, Payload, Context);
m_post(_Path, _Msg, _Context) ->
    {error, unknown_path}.

-spec m_delete( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | {error, term()}.
m_delete([ Key ], _Msg, Context) ->
    delete(Key, Context);
m_delete([], _Msg, Context) ->
    delete(Context);
m_delete(_Path, _Msg, _Context) ->
    {error, unknown_path}.



%% @doc Ping the session to keep it alive.
-spec ping( z:context() ) -> ok | {error, no_session | term()}.
ping(Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            z_server_storage:ping(Sid, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Store a key in the session.
-spec store( term(), term(), z:context() ) -> ok | {error, no_session | not_found | full | term()}.
store(Key, Value, Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            case z_server_storage:store(Sid, Key, Value, Context) of
                ok ->
                    event(Sid, #{ op => store, key => Key, value => Value }, Context);
                {error, no_session} ->
                    _ = mod_server_storage:start_session(Sid, Context),
                    case z_server_storage:store(Sid, Key, Value, Context) of
                        ok ->
                            event(Sid, #{ op => store, key => Key, value => Value }, Context);
                        R ->
                            R
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Find all keys in the session.
-spec lookup( z:context() ) -> {ok, map()}| {error, no_session | not_found | term()}.
lookup(Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            z_server_storage:lookup(Sid, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Find a key in the session.
-spec lookup( term(), z:context() ) -> {ok, term()} | {error, no_session | not_found | term()}.
lookup(Key, Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            z_server_storage:lookup(Sid, Key, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Delete a key from the session.
-spec delete( term(), z:context() ) -> ok | {error, no_session | not_found | term()}.
delete(Key, Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            case z_server_storage:delete(Sid, Key, Context) of
                ok ->
                    event(Sid, #{ op => delete }, Context);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Delete all keys from the session.
-spec delete( z:context() ) -> ok | {error, no_session | not_found | term()}.
delete(Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            case z_server_storage:delete(Sid, Context) of
                ok ->
                    event(Sid, #{ op => delete }, Context);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Store a key/valye in the session, without access via the model access functions.
-spec secure_store( term(), term(), z:context() ) -> ok | {error, no_session | not_found | term()}.
secure_store(Key, Value, Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            case z_server_storage:secure_store(Sid, Key, Value, Context) of
                ok ->
                    event(Sid, #{ op => secure_store }, Context);
                {error, no_session} ->
                    _ = mod_server_storage:start_session(Sid, Context),
                    case z_server_storage:secure_store(Sid, Key, Value, Context) of
                        ok ->
                            event(Sid, #{ op => secure_store }, Context);
                        {error, _} = Error ->
                            Error
                    end
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Find all keys in the secure session, without access via the model access functions.
-spec secure_lookup( z:context() ) -> ok | {error, no_session | not_found | term()}.
secure_lookup(Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            z_server_storage:secure_lookup(Sid, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Find a key in the secure session, without access via the model access functions.
-spec secure_lookup( term(), z:context() ) -> ok | {error, no_session | not_found | term()}.
secure_lookup(Key, Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            z_server_storage:secure_lookup(Sid, Key, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Delete a key from the session, without access via the model access functions.
-spec secure_delete( term(), z:context() ) -> ok | {error, no_session | not_found | term()}.
secure_delete(Key, Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            case z_server_storage:secure_delete(Sid, Key, Context) of
                ok ->
                    event(Sid, #{ op => secure_delete }, Context);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Delete all keys from the session, without access via the model access functions.
-spec secure_delete( z:context() ) -> ok | {error, no_session | not_found | term()}.
secure_delete(Context) ->
    case z_context:session_id(Context) of
        {ok, Sid} ->
            case z_server_storage:secure_delete(Sid, Context) of
                ok ->
                    event(Sid, #{ op => secure_delete }, Context);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

event(_Sid, _Payload, _Context) ->
    % @todo first add some security checks to not leak the Sid, or find a method
    % to publish to a client routing topic wihout using the Sid
    % z_mqtt:publish([ <<"model">>, <<"server_storage">>, <<"event">>, Sid ], Payload, Context).
    ok.

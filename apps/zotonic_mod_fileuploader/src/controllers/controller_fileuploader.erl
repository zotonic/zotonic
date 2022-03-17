%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Controller accepting blocks for uploaded files.

%% Copyright 2021 Marc Worrell
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

-module(controller_fileuploader).

-export([
    resource_exists/1,
    allowed_methods/1,
    content_types_provided/1,
    content_types_accepted/1,
    charsets_provided/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

resource_exists(Context) ->
    Name = z_context:get_q(<<"name">>, Context),
    {z_fileuploader:exists(Name), Context}.

allowed_methods(Context) ->
    {[<<"GET">>, <<"POST">>], Context}.

content_types_provided(Context) ->
    {[
        {<<"application">>, <<"json">>, []}
    ], Context}.

content_types_accepted(Context) ->
    {[
        {<<"application">>, <<"octet-stream">>, []}
    ], Context}.

charsets_provided(Context) ->
    {[ <<"utf-8">> ], Context}.

process(<<"GET">>, _, Provided, Context) ->
    Resp = case m_fileuploader:status(z_context:get_q(<<"name">>, Context), Context) of
        {ok, Status} ->
            #{
                status => <<"ok">>,
                result => Status
            };
        {error, _} ->
            #{
                status => <<"error">>
            }
    end,
    {z_controller_helper:encode_response(Provided, Resp), Context};
process(<<"POST">>, _, Provided, Context) ->
    Name = z_context:get_q(<<"name">>, Context),
    QOffset = z_context:get_q(<<"offset">>, Context),
    case z_utils:only_digits(QOffset) of
        true ->
            Offset = binary_to_integer(QOffset),
            {Data, Context1} = z_controller_helper:req_body(Context),
            Resp = case m_fileuploader:upload(Name, Offset, Data, Context1) of
                {ok, Status} ->
                    ?LOG_DEBUG(#{
                        text => <<"Accepted block for fileuploader">>,
                        result => ok,
                        name => Name,
                        offset => Offset
                    }),
                    #{
                        status => <<"ok">>,
                        result => Status
                    };
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        text => <<"Error accepting block for fileuploader">>,
                        result => error,
                        reason => Reason,
                        name => Name,
                        offset => Offset
                    }),
                    #{
                        status => <<"error">>,
                        error => <<"upload">>
                    }
            end,
            {z_controller_helper:encode_response(Provided, Resp), Context1};
        false ->
            ?LOG_ERROR(#{
                text => <<"Error accepting block for fileuploader">>,
                result => error,
                reason => illegal_offset,
                name => Name,
                offset => QOffset
            }),
            Resp = #{
                status => <<"error">>,
                error => <<"offset">>
            },
            {z_controller_helper:encode_response(Provided, Resp), Context}
    end.


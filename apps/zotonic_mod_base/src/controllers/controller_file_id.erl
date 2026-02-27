%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2022 Marc Worrell
%% @doc Redirect the path associated with the media of a file.
%% @end

%% Copyright 2014-2022 Marc Worrell
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

-module(controller_file_id).
-moduledoc("
Redirect to the controller controller_file.

This controller maps a resource id to the filename of the medium associated with the resource.

For the redirect it uses the dispatch rule defined in the dispatch options.

Examples from mod_base:


```erlang
{media_inline, [\"media\",\"inline\",\"id\",id], controller_file_id, [ {dispatch, media_inline}, {ssl, any} ]},
{media_inline, [\"media\",\"inline\",'*'], controller_file, [ {content_disposition, inline}, {ssl, any} ]},
```

The first dispatch rule will redirect to the second. If no associated file was found, then a 404 is returned.

See also

[controller_file](/id/doc_controller_controller_file)
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    resource_exists/1,
    forbidden/1,
    previously_existed/1,
    moved_temporarily/1,
    moved_permanently/1
]).


service_available(Context) ->
    Context1 = z_context:ensure_qs(Context),
    z_context:logger_md(Context1),
    Id = get_id(Context1),
    Medium = m_media:get(Id, Context1),
    {true, z_context:set(?MODULE, {Id, Medium}, Context1)}.

resource_exists(Context) ->
    case z_context:get(?MODULE, Context) of
        {undefined, _Medium} ->
            {false, Context};
        {_Id, undefined} ->
            {false, Context};
        {Id, _Medium} ->
            {not m_rsc:exists(Id, Context), Context}
    end.

previously_existed(Context) ->
    case z_context:get(?MODULE, Context) of
        {undefined, _M} ->
            {false, Context};
        {_Id, undefined} ->
            {false, Context};
        {Id, _M} ->
            case m_rsc:exists(Id, Context) of
                true ->
                    {true, Context};
                false ->
                    case m_rsc_gone:is_gone(Id, Context) of
                        true ->
                            {true, z_context:set(?MODULE, {Id, gone}, Context)};
                        false -> {false, Context}
                    end
            end
    end.

forbidden(Context) ->
    case z_context:get(?MODULE, Context) of
        {undefined, _M} ->
            {false, Context};
        {_Id, undefined} ->
            {false, Context};
        {Id, _M} ->
            {not z_acl:rsc_visible(Id, Context), Context}
    end.

moved_temporarily(Context) ->
    case z_context:get(?MODULE, Context) of
        {Id, gone} ->
            case m_rsc_gone:get_new_location(Id, Context) of
                undefined -> {false, Context};
                Location -> {{true, Location}, Context}
            end;
        {Id, Medium} ->
            case z_context:get(is_permanent, Context, false) of
                true -> {false, Context};
                false -> do_redirect(Id, Medium, Context)
            end
    end.

moved_permanently(Context) ->
    {Id, Medium} = z_context:get(?MODULE, Context),
    case z_context:get(is_permanent, Context, false) of
        true -> do_redirect(Id, Medium, Context);
        false -> {false, Context}
    end.

do_redirect(_Id, undefined, Context) ->
    {false, Context};
do_redirect(undefined, _Medium, Context) ->
    {false, Context};
do_redirect(_Id, #{ <<"filename">> := Filename }, Context)
    when is_binary(Filename), Filename =/= <<>> ->
    Dispatch = z_context:get(dispatch, Context, media_inline),
    Args = z_context:get_all(Context),
    Args1 = case z_context:get(qargs, Context) of
                undefined ->
                    Args;
                true ->
                    z_context:get_q_all_noz(Context) ++ Args;
                ArgList when is_list(ArgList) ->
                    [{K, z_context:get_q(K, Context)} || K <- ArgList ] ++ Args
            end,
    Args2 = lists:foldl(fun(K, Acc) ->
                            proplists:delete(K, Acc)
                        end,
                        Args1,
                        [ id, star, ?MODULE, is_permanent | z_dispatcher:dispatcher_args() ]),
    Location = z_dispatcher:url_for(Dispatch, [{star, Filename}|Args2], Context),
    {{true, z_context:abs_url(Location, Context)}, Context};
do_redirect(_Id, _Medium, Context) ->
    {false, Context}.

get_id(Context) ->
    case z_context:get(id, Context) of
        undefined -> m_rsc:rid(z_context:get_q(<<"id">>, Context), Context);
        ArgId -> m_rsc:rid(ArgId, Context)
    end.


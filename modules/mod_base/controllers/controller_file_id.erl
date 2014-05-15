%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Redirect the path associated with the media of a file.

%% Copyright 2014 Marc Worrell
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
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    resource_exists/2,
    forbidden/2,
    previously_existed/2,
    moved_temporarily/2,
    moved_permanently/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, 
                    z_context:continue_session(
                        z_context:ensure_qs(Context))),
    Id = get_id(DispatchArgs, Context1),
    Medium = m_media:get(Id, Context1),
    {true, ReqData, {Id, Medium, Context1}}.

resource_exists(ReqData, {undefined, _Medium, _Context} = MC) ->
    {false, ReqData, MC};
resource_exists(ReqData, {_Id, undefined, _Context} = MC) ->
    {false, ReqData, MC};
resource_exists(ReqData, {Id, _Medium, Context} = MC) ->
    {not m_rsc:exists(Id, Context), ReqData, MC}.

previously_existed(ReqData, {undefined, _M, _Context} = MC) ->
    {false, ReqData, MC};
previously_existed(ReqData, {_Id, undefined, _Context} = MC) ->
    {false, ReqData, MC};
previously_existed(ReqData, {Id, _M, Context} = MC) ->
    Existed = m_rsc:exists(Id, Context) orelse m_rsc_gone:is_gone(Id, Context),
    {Existed, ReqData, MC}.

forbidden(ReqData, {undefined, _M, _Context} = MC) ->
    {false, ReqData, MC};
forbidden(ReqData, {_Id, undefined, _Context} = MC) ->
    {false, ReqData, MC};
forbidden(ReqData, {Id, _M, Context} = MC) ->
    {not z_acl:rsc_visible(Id, Context), ReqData, MC}.

moved_temporarily(ReqData, {Id, Medium, Context} = MC) ->
    case z_context:get(is_permanent, Context, false) of
        true -> {false, ReqData, MC};
        false -> do_redirect(Id, Medium, ReqData, Context)
    end.

moved_permanently(ReqData, {Id, Medium, Context} = MC) ->
    case z_context:get(is_permanent, Context, false) of
        true -> do_redirect(Id, Medium, ReqData, Context);
        false -> {false, ReqData, MC}
    end.


do_redirect(Id, undefined, ReqData, Context) ->
    {false, ReqData, {Id, undefined, Context}};
do_redirect(undefined, Medium, ReqData, Context) ->
    {false, ReqData, {undefined, Medium, Context}};
do_redirect(Id, Medium, ReqData, Context) ->
    case proplists:get_value(filename, Medium) of
        <<>> ->
            {false, ReqData, {Id, Medium, Context}};
        undefined ->
            {false, ReqData, {Id, Medium, Context}};
        Filename ->
            Dispatch = z_context:get(dispatch, Context, media_inline),
            Args = z_context:get_all(Context),
            Args1 = case z_context:get(qargs, Context) of
                        undefined ->
                            Args;
                        true ->
                            z_context:get_q_all(Context) ++ Args;
                        ArgList when is_list(ArgList) ->
                            [{K, z_context:get_q(K, Context)} || K <- ArgList ] ++ Args
                    end,
            Args2 = lists:foldl(fun(K, Acc) -> 
                                    proplists:delete(K, Acc) 
                                end,
                                Args1,
                                [id, is_permanent, dispatch, q, qargs, zotonic_dispatch, star, ssl]),
            Location = z_dispatcher:url_for(Dispatch, [{star, Filename}|Args2], Context),
            {{true, z_context:abs_url(Location, Context)}, ReqData, {Id, Medium, Context}}
    end.


get_id(Args, Context) ->
    case proplists:get_value(id, Args) of
        undefined -> m_rsc:rid(z_context:get_q("id", Context), Context);
        ArgId -> m_rsc:rid(ArgId, Context)
    end.


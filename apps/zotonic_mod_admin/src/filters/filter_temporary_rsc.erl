%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015-2025 Marc Worrell
%% @doc Creates a temporary resource. If not modified then it will be deleted.
%% 2end

%% Copyright 2015-2025 Marc Worrell
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

-module(filter_temporary_rsc).
-moduledoc("
Creates a temporary resource if its input value is not defined.

The created resource receives the properties of the second parameter.

After the resource is created, every hour a check is made if the resource has been edited and still registered in the
Server Storage.

Note

`mod_server_storage` must be enabled for this filter to work.

If the resource is abandoned and not changed since its creation, then it is automatically deleted.

Only a single temporary resource can exist per category per session. Requesting a second resource of the same category
for a session will return the earlier created resource. If the earlier resource has been updated, then a new resource is created.

This filter is used for situations where an edit form is needed but an intermediate dialog requesting for the title of
the resource is unwanted.

Example:


```none
{% with id|temporary_rsc:%{ title:_\"New\", category_id: `article` } as id %}
    {% wire id=#form type=\"submit\" postback=`rscform` delegate=`controller_admin_edit` %}
    <form id=\"{{ #form }}\" action=\"postback\">
        <input type=\"hidden\" name=\"id\" value=\"{{ id }}\" />
        <input type=\"hidden\" name=\"is_published\" value=\"1\" />
        <input type=\"text\" name=\"title\" value=\"{{ id.title }}\" />
        ...
    </form>
{% endwith %}
```

See [mod\\_admin](/id/doc_module_mod_admin), [mod\\_server\\_storage](/id/doc_module_mod_server_storage)
").
-export([
    temporary_rsc/2,
    temporary_rsc/3,

    task_delete_inactive/4,
    is_creator/2
]).

%% Check every hour if the page can be deleted.
-define(INACTIVE_CHECK_DELAY, 3600).

-include_lib("kernel/include/logger.hrl").

temporary_rsc(RscId, Context) ->
    temporary_rsc(RscId, #{}, Context).

temporary_rsc(undefined, Props, Context) when is_map(Props) ->
    PropsMap = z_props:from_map(Props),
    make_temporary_rsc(PropsMap, Context);
temporary_rsc(undefined, {props, Props}, Context) when is_list(Props) ->
    PropsMap = z_props:from_list(Props),
    make_temporary_rsc(PropsMap, Context);
temporary_rsc(undefined, Props, Context) when is_list(Props) ->
    PropsMap = z_props:from_list(Props),
    make_temporary_rsc(PropsMap, Context);
temporary_rsc(<<>>, Props, Context) ->
    temporary_rsc(undefined, Props, Context);
temporary_rsc([], Props, Context) ->
    temporary_rsc(undefined, Props, Context);
temporary_rsc(RscId, _Props, _Context) ->
    RscId.

task_delete_inactive(RscId, Key, SessionId, Context) ->
    case is_unmodified_rsc(RscId, Context) of
        true ->
            case z_server_storage:secure_lookup(Key, SessionId, Context) of
                {ok, RscId} ->
                    {delay, ?INACTIVE_CHECK_DELAY};
                _Other ->
                    ?LOG_DEBUG(#{
                        text => <<"Deleting unmodified temporary resource">>,
                        in => zotonic_mod_admin,
                        rsc_id => RscId
                    }),
                    ok = m_rsc:delete(RscId, z_acl:sudo(Context)),
                    ok
            end;
        false ->
            % Remove temporary flag
            case m_rsc:p_no_acl(RscId, <<"is_temporary">>, Context) of
                true ->
                    m_rsc:update(RscId, #{ <<"is_temporary">> => undefined },
                                 [ no_touch, {is_acl_check, false} ], Context),
                    ok;
                _ ->
                    ok
            end
    end.

%% @doc Check if the current session user/session is the creator of the given temporary resource.
-spec is_creator(RscId, Context) -> boolean() when
    RscId :: m_rsc:resource() | undefined,
    Context :: z:context().
is_creator(RscId, Context) when is_integer(RscId) ->
    case m_rsc:p_no_acl(RscId, <<"is_temporary">>, Context) of
        true ->
            CatId = m_rsc:p_no_acl(RscId, <<"category_id">>, Context),
            case find_existing(CatId, Context) of
                {ok, TmpRscId} when TmpRscId =:= RscId ->
                    true;
                {ok, _TmpRscId} ->
                    false;
                {error, _} ->
                    false
            end;
        _ ->
            false
    end;
is_creator(undefined, _Context) ->
    false;
is_creator(RscId, Context) ->
    is_creator(m_rsc:rid(RscId, Context), Context).


%% --- internal functions ---

make_temporary_rsc(Props, Context) ->
    make_temporary_rsc( z_context:session_id(Context), Props, Context ).

make_temporary_rsc({error, _}, _Props, _Context) ->
    undefined;
make_temporary_rsc({ok, _SessionId}, Props, Context) ->
    {Cat, Props1} = ensure_category(Props),
    case m_rsc:rid(Cat, Context) of
        undefined ->
            ?LOG_WARNING(#{
                text => <<"filter_temporary_rsc: could not find category">>,
                in => zotonic_mod_admin,
                category => Cat
            }),
            undefined;
        CatId ->
            make_rsc(
                    find_existing(CatId, Context),
                    CatId, Props1,
                    Context)
    end.

make_rsc({ok, RscId}, _CatId, _Props, _Context) ->
    RscId;
make_rsc({error, not_found}, CatId, Props, Context) ->
    Props1 = Props#{
        <<"is_temporary">> => true
    },
    case m_rsc:insert(Props1, Context) of
        {ok, RscId} ->
            Key = {temporary_rsc, CatId},
            {ok, SessionId} = z_context:session_id(Context),
            m_server_storage:secure_store(Key, RscId, Context),
            Args = [
                RscId,
                Key,
                SessionId
            ],
            z_pivot_rsc:insert_task_after(
                        ?INACTIVE_CHECK_DELAY,
                        ?MODULE, task_delete_inactive, z_convert:to_binary(RscId), Args,
                        Context),
            RscId;
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Can not make temporary resource">>,
                in => zotonic_mod_admin,
                result => error,
                reason => Reason,
                category_id => CatId,
                props => Props1
            }),
            undefined
    end.

%% If no user then limit to 1 temporary rsc per client
find_existing(CatId, Context) ->
    case m_server_storage:secure_lookup({temporary_rsc, CatId}, Context) of
        {ok, RscId} when is_integer(RscId) ->
            case is_unmodified_rsc(RscId, Context) of
                true -> {ok, RscId};
                false -> {error, not_found}
            end;
        {error, not_found} ->
            {error, not_found};
        {error, no_session} ->
            {error, not_found}
    end.

is_unmodified_rsc(Id, Context) ->
    m_rsc:exists(Id, Context) andalso m_rsc:p_no_acl(Id, version, Context) =:= 1.

ensure_category(Props) ->
    case cat(Props) of
        undefined ->
            Props1 = maps:without([ <<"category">>, <<"category_id">> ], Props),
            {article, Props1#{ <<"category_id">> => article }};
        Cat ->
            {Cat, Props}
    end.

cat(#{ <<"category">> := Cat }) -> Cat;
cat(#{ <<"category_id">> := Cat }) -> Cat;
cat(_) -> undefined.


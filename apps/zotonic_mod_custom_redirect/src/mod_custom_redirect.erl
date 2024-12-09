%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2024 Marc Worrell
%% @doc Redirect custom domains/paths to other domains/paths.
%% This module is notified when the dispatcher found an unknown host name.
%% @end

%% Copyright 2013-2024 Marc Worrell
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

-module(mod_custom_redirect).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Custom Redirects").
-mod_description("Redirect custom domains and paths to any location.").
-mod_prio(10000).
-mod_schema(2).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-export([
    observe_dispatch_host/2,
    observe_dispatch/2,
    observe_admin_menu/3,
    event/2,
    manage_schema/2
]).


%% @doc Called when the host didn't match any site config
observe_dispatch_host(#dispatch_host{ host = Host, path = Path }, Context) ->
    case m_custom_redirect:list_dispatch_host(Host, Path, Context) of
        [{BestPath,_,_}=Best|Rest] -> select_best(Rest, size(BestPath), Best);
        [] -> undefined
    end.

%% @doc Called when the path didn't match any dispatch rule
observe_dispatch(#dispatch{ path = Path }, Context) ->
    case m_custom_redirect:get_dispatch(Path, Context) of
        {Redirect,IsPermanent} -> {ok, #dispatch_redirect{location=Redirect, is_permanent=IsPermanent}};
        undefined -> undefined
    end.


observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_custom_redirect,
                parent=admin_modules,
                label=?__("Domains and redirects", Context),
                url={admin_custom_redirect},
                visiblecheck={acl, use, mod_custom_redirect}}

     |Acc].

event(#submit{message=custom_redirects}, Context) ->
    case m_custom_redirect:is_allowed(Context) of
        true ->
            Qs = z_context:get_q_all_noz(Context),
            Rows = group_rows(Qs),
            ok = save_rows(Rows, Context),
            z_render:wire({reload, []}, Context);
        false ->
            z_render:growl_error(?__("You are not allowed to change this.", Context), Context)
    end.

manage_schema(Version, Context) ->
    m_custom_redirect:manage_schema(Version, Context).


%% ----------------------------------------------------------------------
%% Support functions
%% ----------------------------------------------------------------------

select_best([], _BestSize, {_Path, NewPath, IsPerm}) ->
    {ok, #dispatch_redirect{location=NewPath, is_permanent=IsPerm}};
select_best([{Path, _, _}=New|Rest], BestSize, Best) ->
    PathSize = size(Path),
    case PathSize > BestSize of
        true -> select_best(Rest, Path, New);
        false -> select_best(Rest, BestSize, Best)
    end.


group_rows(Qs) ->
    Ids = get_prefix(<<"id">>, Qs),
    Hosts = get_prefix(<<"host">>, Qs),
    Paths = get_prefix(<<"path">>, Qs),
    Redirects = get_prefix(<<"redirect">>, Qs),
    Perms = get_prefix(<<"is_permanent">>, Qs),
    zip([Ids, Hosts, Paths, Redirects, Perms]).

zip(Lists) ->
    zip(Lists, []).

zip([[]|_], Acc) ->
    lists:reverse(Acc);
zip(Lists, Acc) ->
    Heads = [ element(2,hd(L)) || L <- Lists ],
    Tails = [ tl(L) || L <- Lists ],
    zip(Tails, [ list_to_tuple(Heads) | Acc]).


save_rows(Rows, Context) ->
    z_db:transaction(fun(Ctx) -> save_rows_trans(Rows, Ctx) end, Context).

save_rows_trans(Rows, Context) ->
    CurrIds = m_custom_redirect:list_ids(Context),
    FoundIds = do_save_rows(Rows, [], Context),
    MissingIds = CurrIds -- FoundIds,
    [ m_custom_redirect:delete(Id, Context) || Id <- MissingIds ],
    ok.

do_save_rows([], Acc, _Context) ->
    Acc;
do_save_rows([{Id,Host,Path,Redirect,IsPermanent}|Rows], Acc, Context) ->
    Host1 = z_string:trim(Host),
    Path1 = z_string:trim(Path),
    Props = [
        {host, Host1},
        {path, Path1},
        {redirect, z_string:trim(Redirect)},
        {is_permanent, z_convert:to_bool(IsPermanent)}
    ],
    case do_save_redirect(Id, Host1, Path1, Props, Context) of
        skip -> do_save_rows(Rows, Acc, Context);
        {ok, NewId} -> do_save_rows(Rows, [NewId|Acc], Context)
    end.


do_save_redirect(_Id, <<>>, <<>>, _Props, _Context) ->
    skip;
do_save_redirect(<<>>, Host, Path, Props, Context) ->
    case m_custom_redirect:get(Host, Path, Context) of
        undefined ->
            m_custom_redirect:insert(Props, Context);
        Existing ->
            ExistingId = proplists:get_value(id, Existing),
            {ok,1} = m_custom_redirect:update(ExistingId, Props, Context),
            {ok, ExistingId}
    end;
do_save_redirect(Id, Host, Path, Props, Context) ->
    case m_custom_redirect:get(Host, Path, Context) of
        undefined ->
            IdInt = z_convert:to_integer(Id),
            {ok, 1} = m_custom_redirect:update(IdInt, Props, Context),
            {ok, IdInt};
        Existing ->
            ExistingId = proplists:get_value(id, Existing),
            {ok, 1} = m_custom_redirect:update(ExistingId, Props, Context),
            {ok, ExistingId}
    end.



get_prefix(Prefix, Qs) ->
    get_prefix(Prefix, [], Qs).

get_prefix(_Prefix, Acc, []) ->
    lists:reverse(Acc);
get_prefix(Prefix, Acc, [{Q,_}=QV|Qs]) ->
    case binary:longest_common_prefix([Prefix, Q]) == size(Prefix) of
        true -> get_prefix(Prefix, [QV|Acc], Qs);
        false -> get_prefix(Prefix, Acc, Qs)
    end.


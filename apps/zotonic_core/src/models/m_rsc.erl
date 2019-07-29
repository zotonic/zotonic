%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2014 Marc Worrell
%%
%% @doc Model for resource data. Interfaces between zotonic, templates and the database.

%% Copyright 2009-2014 Marc Worrell
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

-module(m_rsc).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(zotonic_model).

-export([
    m_get/3,
    % m_post/3,
    % m_delete/3,

    name_to_id/2,
    name_to_id_cat/3,

    page_path_to_id/2,

    get/2,
    get_raw/2,
    get_raw_lock/2,
    get_acl_props/2,
    insert/2,
    delete/2,
    merge_delete/3,
    merge_delete/4,
    update/3,
    update/4,
    duplicate/3,
    touch/2,

    exists/2,

    is_visible/2, is_editable/2, is_deletable/2, is_linkable/2,
    is_me/2,
    is_cat/3,
    is_a/2,
    is_a_id/2,
    is_a/3,

    p/3,
    p/4,
    p_no_acl/3,

    op/2, o/2, o/3, o/4,
    sp/2, s/2, s/3, s/4,
    media/2,
    page_url/2,
    page_url_abs/2,
    rid/2,

    name_lookup/2,
    uri_lookup/2,
    ensure_name/2,

    common_properties/1
]).

-export_type([resource/0, resource_id/0, resource_name/0, resource_uri/0, props/0]).

-include_lib("zotonic.hrl").

-type resource() :: resource_id() | list(digits()) | resource_name().
-type resource_id() :: integer().
-type resource_name() :: string() | binary() | atom().
-type resource_uri() :: binary().
-type props() :: proplists:proplist().
-type digits() :: 16#30..16#39.


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ Id, is_cat, Key | Rest ], _Msg, Context) ->
    {ok, {is_cat(Id, Key, Context), Rest}};
m_get([ Id, Key | Rest ], _Msg, Context) ->
    {ok, {p(Id, Key, Context), Rest}};
m_get([ Id ], _Msg, Context) ->
    {ok, {get(Id, Context), []}};
m_get(Vs, _Msg, _Context) ->
    lager:debug("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.


%% @doc Return the id of the resource with the name
-spec name_to_id(resource_name(), z:context()) -> {ok, resource_id()} | {error, string()}.
name_to_id(Name, _Context) when is_integer(Name) ->
    {ok, Name};
name_to_id(undefined, _Context) ->
    {error, {unknown_rsc, undefined}};
name_to_id(<<>>, _Context) ->
    {error, {unknown_rsc, <<>>}};
name_to_id([], _Context) ->
    {error, {unknown_rsc, []}};
name_to_id(Name, Context) ->
    case name_lookup(Name, Context) of
        Id when is_integer(Id) -> {ok, Id};
        _ -> {error, {unknown_rsc, Name}}
    end.

-spec name_to_id_cat(resource_name() | resource_id(), resource_name(), z:context()) -> any().
name_to_id_cat(Name, Cat, Context) when is_integer(Name) ->
    F = fun() ->
        {ok, CatId} = m_category:name_to_id(Cat, Context),
        case z_db:q1("select id from rsc where id = $1 and category_id = $2", [Name, CatId], Context) of
            undefined -> {error, {unknown_rsc_cat, Name, Cat}};
            Id -> {ok, Id}
        end
    end,
    z_depcache:memo(F, {rsc_name, Name, Cat}, ?DAY, [Cat], Context);
name_to_id_cat(Name, Cat, Context) ->
    F = fun() ->
        {ok, CatId} = m_category:name_to_id(Cat, Context),
        case z_db:q1("select id from rsc where Name = $1 and category_id = $2", [Name, CatId], Context) of
            undefined -> {error, {unknown_rsc_cat, Name, Cat}};
            Id -> {ok, Id}
        end
    end,
    z_depcache:memo(F, {rsc_name, Name, Cat}, ?DAY, [Cat], Context).

%% @doc Given a page path, return {ok, Id} with the id of the found
%% resource. When the resource does not have the page path, but did so
%% once, this function will return {redirect, Id} to indicate that the
%% page path was found but is no longer the current page path for the
%% resource.
-spec page_path_to_id( string(), z:context() ) ->
              {ok, resource_id()}
            | {redirect, resource_id()}
            | {error, {unknown_path, string()}}
            | {error, {illegal_page_path, string()}}.
page_path_to_id(Path, Context) ->
    Path1 = [ $/, string:strip(Path, both, $/)],
    case catch z_db:q1("select id from rsc where page_path = $1", [Path1], Context) of
        Id when is_integer(Id) -> {ok, Id};
        undefined ->
            case z_db:q1("select id from rsc_page_path_log where page_path = $1", [Path1], Context) of
                OtherId when is_integer(OtherId) ->
                    {redirect, OtherId};
                undefined ->
                    {error, {unknown_page_path, Path1}}
            end;
        Other ->
            {error, {illegal_page_path, Path1, Other}}
    end.

%% @doc Read a whole resource
-spec get(resource(), z:context()) -> props() | undefined.
get(Id, Context) ->
    case rid(Id, Context) of
        Rid when is_integer(Rid) ->
            case z_acl:rsc_visible(Id, Context) of
                true -> filter_props(Rid, get_cached(Rid, Context), Context);
                false -> undefined
            end;
        undefined ->
            undefined
    end.

-spec get_cached(resource_id(), z:context()) -> props() | undefined.
get_cached(Id, Context) when is_integer(Id) ->
    z_depcache:memo(
        fun() ->
            Props = get_raw(Id, false, Context),
            z_notifier:foldr(#rsc_get{id=Id}, Props, Context)
        end,
        Id,
        ?WEEK,
        Context).

-spec filter_props(resource(), props() | undefined, z:context()) -> props() | undefined.
filter_props(_Id, undefined, _Context) ->
    undefined;
filter_props(Id, Props, Context) ->
    case z_acl:is_admin(Context) of
        true -> Props;
        false ->
            case z_acl:rsc_visible(Id, Context) of
                false ->
                    [];
                true ->
                    lists:filter(
                        fun({Property, _Val}) ->
                            z_acl:rsc_prop_visible(Id, Property, Context)
                        end,
                        Props)
            end
    end.

%% @doc Get the resource from the database, do not fetch the pivot fields.
-spec get_raw(resource(), z:context()) -> props().
get_raw(Id, Context) ->
    Props = get_raw(Id, false, Context),
    filter_props(Id, Props, Context).

get_raw_lock(Id, Context) ->
    Props = get_raw(Id, true, Context),
    filter_props(Id, Props, Context).

get_raw(Id, IsLock, Context) ->
    SQL = case z_memo:get(rsc_raw_sql) of
              undefined ->
                  AllCols = [z_convert:to_list(C) || C <- z_db:column_names(rsc, Context)],
                  DataCols = lists:filter(
                      fun ("pivot_geocode") -> true;
                          ("pivot_geocode_qhash") -> true;
                          ("pivot_location_lat") -> true;
                          ("pivot_location_lng") -> true;
                          ("pivot_" ++ _) -> false;
                          (_) -> true
                      end,
                      AllCols),
                  Query = "select " ++ string:join(DataCols, ",") ++ " from rsc where id = $1",
                  z_memo:set(rsc_raw_sql, Query),
                  Query;
              Memo ->
                  Memo
          end,
    SQL1 = case IsLock of
               true -> SQL ++ " for update";
               false -> SQL
           end,
    case z_db:assoc_props_row(SQL1, [m_rsc:rid(Id, Context)], Context) of
        undefined -> [];
        Raw -> ensure_utc_dates(Raw, Context)
    end.


%% Fix old records which had serialized data in localtime and no date_is_all_day flag
ensure_utc_dates(Props, Context) ->
    case lists:keymember(tz, 1, Props) of
        true ->
            Props;
        false ->
            % Convert dates, assuming the system's default timezone
            DateStart = lists:keyfind(date_start, 1, Props),
            DateEnd = lists:keyfind(date_end, 1, Props),
            IsAllDay = case {DateStart, DateEnd} of
                {{date_start, {_, {0, 0, _StartSec}}}, {date_end, {_, {23, 59, _EndSec}}}} ->
                    true;
                _ ->
                    false
            end,
            [
                {tz, z_context:tz(Context)},
                {date_is_all_day, IsAllDay}
                | [ensure_utc_date(P, IsAllDay) || P <- Props]
            ]
    end.

% publication_start, publication_end, created, and modified are already in UTC
ensure_utc_date(Date, IsAllDay) ->
    try
        ensure_utc_date_1(Date, IsAllDay)
    catch
        error:badarg ->
            {element(1, Date), undefined}
    end.

ensure_utc_date_1({date_start, DT}, false) when is_tuple(DT) ->
    {date_start, hd(calendar:local_time_to_universal_time_dst(DT))};
ensure_utc_date_1({date_end, DT}, false) when is_tuple(DT) ->
    {date_end, hd(calendar:local_time_to_universal_time_dst(DT))};
ensure_utc_date_1({org_pubdate, DT}, _IsAllDay) when is_tuple(DT) ->
    {org_pubdate, hd(calendar:local_time_to_universal_time_dst(DT))};
ensure_utc_date_1(P, _IsAllDay) ->
    P.


%% @doc Get the ACL fields for the resource with the id.
%% Will always return a valid record, even if the resource does not exist.
-spec get_acl_props(Id :: resource(), z:context()) -> #acl_props{}.
get_acl_props(Id, Context) when is_integer(Id) ->
    F = fun() ->
        Result =
            z_db:q_row(
                "select is_published, is_authoritative, "
                "publication_start, publication_end, "
                "content_group_id, visible_for "
                "from rsc "
                "where id = $1",
                [Id], Context),
        case Result of
            {IsPub, IsAuth, PubS, PubE, CGId, VisFor} ->
                #acl_props{
                    is_published = IsPub,
                    is_authoritative = IsAuth,
                    publication_start = PubS,
                    publication_end = PubE,
                    content_group_id = CGId,
                    visible_for = VisFor
                };
            undefined ->
                #acl_props{
                    is_published = false
                }
        end
    end,
    z_depcache:memo(F, {rsc_acl_fields, Id}, ?DAY, [Id], Context);
get_acl_props(Name, Context) ->
    case rid(Name, Context) of
        undefined ->
            #acl_props{
                is_published = false
            };
        Id -> get_acl_props(Id, Context)
    end.


%% @doc Insert a new resource
-spec insert(Props :: props(), z:context()) -> {ok, resource_id()} | {error, term()}.
insert(Props, Context) ->
    m_rsc_update:insert(Props, Context).

%% @doc Delete a resource
-spec delete(resource(), z:context()) -> ok | {error, term()}.
delete(Id, Context) ->
    m_rsc_update:delete(Id, Context).

%% @doc Merge a resource with another, delete the loser.
-spec merge_delete(resource(), resource(), z:context()) -> ok | {error, term()}.
merge_delete(WinnerId, LoserId, Context) ->
    m_rsc_update:merge_delete(WinnerId, LoserId, [ {is_merge_trans, false} ], Context).

%% @doc Merge a resource with another, delete the loser.
-spec merge_delete(resource(), resource(), list(), z:context()) -> ok | {error, term()}.
merge_delete(WinnerId, LoserId, Options, Context) ->
    m_rsc_update:merge_delete(WinnerId, LoserId, Options, Context).

%% @doc Update a resource
-spec update(resource(), props(), z:context()) -> {ok, resource()} | {error, term()}.
update(Id, Props, Context) ->
    m_rsc_update:update(Id, Props, Context).

-spec update(resource(), props(), list(), z:context()) -> {ok, resource()} | {error, term()}.
update(Id, Props, Options, Context) ->
    m_rsc_update:update(Id, Props, Options, Context).


%% @doc Duplicate a resource.
-spec duplicate(resource(), props(), z:context()) ->
    {ok, NewId :: resource_id()} | {error, Reason :: string()}.
duplicate(Id, Props, Context) ->
    m_rsc_update:duplicate(Id, Props, Context).


%% @doc "Touch" the rsc, incrementing the version nr and the modification date/ modifier_id.
%% This should be called as part of another update or transaction and does not resync the caches,
%% and does not check the ACL.  After "touching" the resource will be re-pivoted.
-spec touch(resource(), z:context()) -> {ok, resource_id()} | {error, Reason :: string()}.
touch(Id, Context) ->
    case z_db:q(
        "update rsc set version = version + 1, modifier_id = $1, modified = now() where id = $2",
        [z_acl:user(Context), rid(Id, Context)],
        Context
    ) of
        1 -> {ok, Id};
        0 -> {error, {unknown_rsc, Id}}
    end.


-spec exists(resource(), z:context()) -> boolean().
exists(Id, Context) ->
    case rid(Id, Context) of
        Rid when is_integer(Rid) ->
            case m_rsc:p_no_acl(Rid, id, Context) of
                Rid -> true;
                undefined -> false
            end;
        undefined -> false
    end.

-spec is_visible(resource(), z:context()) -> boolean().
is_visible(Id, Context) ->
    z_acl:rsc_visible(Id, Context).

-spec is_editable(resource(), z:context()) -> boolean().
is_editable(Id, Context) ->
    z_acl:rsc_editable(Id, Context).

-spec is_deletable(resource(), z:context()) -> boolean().
is_deletable(Id, Context) ->
    z_acl:rsc_deletable(Id, Context).

-spec is_linkable(resource(), z:context()) -> boolean().
is_linkable(Id, Context) ->
    z_acl:rsc_linkable(Id, Context).

-spec is_me(resource(), z:context()) -> boolean().
is_me(Id, Context) ->
    case rid(Id, Context) of
        RscId when is_integer(RscId) ->
            z_acl:user(Context) =:= RscId;
        _ ->
            false
    end.

is_published_date(Id, Context) ->
    case rid(Id, Context) of
        RscId when is_integer(RscId) ->
            case m_rsc:p_no_acl(RscId, is_published, Context) of
                true ->
                    Date = erlang:universaltime(),
                    m_rsc:p_no_acl(RscId, publication_start, Context) =< Date
                        andalso m_rsc:p_no_acl(RscId, publication_end, Context) >= Date;
                false ->
                    false;
                undefined ->
                    false
            end;
        _ ->
            false
    end.


%% @doc Fetch a property from a resource. When the rsc does not exist, the property does not
%% exist or the user does not have access rights to the property then return 'undefined'.
-spec p(resource(), atom(), z:context()) -> term() | undefined.
p(Id, Property, Context) when is_list(Property); is_binary(Property) ->
    try
        P1 = binary_to_existing_atom(Property, utf8),
        p(Id, P1, Context)
    catch
        error:badarg ->
            p1(Id, Property, Context)
    end;
p(Id, Property, Context)
    when Property =:= category_id
    orelse Property =:= page_url
    orelse Property =:= page_url_abs
    orelse Property =:= category
    orelse Property =:= is_a
    orelse Property =:= uri
    orelse Property =:= is_authoritative
    orelse Property =:= is_published
    orelse Property =:= exists
    orelse Property =:= id
    orelse Property =:= privacy
    orelse Property =:= default_page_url ->
    p_no_acl(rid(Id, Context), Property, Context);
p(Id, Property, Context) ->
    p1(Id, Property, Context).

p1(Id, Property, Context) ->
    case rid(Id, Context) of
        undefined ->
            undefined;
        RId ->
            case z_acl:rsc_visible(RId, Context) of
                true ->
                    case z_acl:rsc_prop_visible(RId, Property, Context) of
                        true -> p_no_acl(RId, Property, Context);
                        false -> undefined
                    end;
                false ->
                    undefined
            end
    end.

%% Fetch property from a resource; but return a default value if not found.
p(Id, Property, DefaultValue, Context) ->
    case p(Id, Property, Context) of
        undefined -> DefaultValue;
        Value -> Value
    end.


%% @doc Fetch a property from a resource, no ACL check is done.
p_no_acl(undefined, _Predicate, _Context) -> undefined;
p_no_acl(Id, Prop, Context) when not is_integer(Id) ->
    case rid(Id, Context) of
        Rid when is_integer(Rid) -> p_no_acl(Rid, Prop, Context);
        undefined -> undefined
    end;
p_no_acl(Id, o, Context) -> o(Id, Context);
p_no_acl(Id, s, Context) -> s(Id, Context);
p_no_acl(Id, op, Context) -> op(Id, Context);
p_no_acl(Id, sp, Context) -> sp(Id, Context);
p_no_acl(Id, is_me, Context) -> is_me(Id, Context);
p_no_acl(Id, is_visible, Context) -> is_visible(Id, Context);
p_no_acl(Id, is_editable, Context) -> is_editable(Id, Context);
p_no_acl(Id, is_deletable, Context) -> is_deletable(Id, Context);
p_no_acl(Id, is_linkable, Context) -> is_linkable(Id, Context);
p_no_acl(Id, is_published_date, Context) -> is_published_date(Id, Context);
p_no_acl(Id, is_a, Context) -> [{C, true} || C <- is_a(Id, Context)];
p_no_acl(Id, exists, Context) -> exists(Id, Context);
p_no_acl(Id, page_url_abs, Context) ->
    case p_no_acl(Id, page_path, Context) of
        undefined -> page_url(Id, true, Context);
        PagePath ->
            opt_url_abs(z_notifier:foldl(#url_rewrite{args = [{id, Id}]}, PagePath, Context), true, Context)
    end;
p_no_acl(Id, page_url, Context) ->
    case p_no_acl(Id, page_path, Context) of
        undefined -> page_url(Id, false, Context);
        PagePath ->
            opt_url_abs(z_notifier:foldl(#url_rewrite{args = [{id, Id}]}, PagePath, Context), false, Context)
    end;
p_no_acl(Id, translation, Context) ->
    fun(Code) ->
        fun(Prop) ->
            case p_no_acl(Id, Prop, Context) of
                {trans, _} = Translated ->
                    z_trans:lookup(Translated, Code, Context);
                Value -> Value
            end
        end
    end;
p_no_acl(Id, default_page_url, Context) -> page_url(Id, Context);
p_no_acl(Id, uri, Context) ->
    case p_cached(Id, uri, Context) of
        Empty when Empty =:= <<>>; Empty =:= undefined ->
            non_informational_uri(Id, Context);
        Uri ->
            Uri
    end;
p_no_acl(Id, category, Context) ->
    m_category:get(p_no_acl(Id, category_id, Context), Context);
p_no_acl(Id, media, Context) -> media(Id, Context);
p_no_acl(Id, medium, Context) -> m_media:get(Id, Context);
p_no_acl(Id, depiction, Context) -> m_media:depiction(Id, Context);
p_no_acl(Id, predicates_edit, Context) -> predicates_edit(Id, Context);
p_no_acl(Id, day_start, Context) ->
    case p_cached(Id, date_start, Context) of
        {{_, _, _} = Date, _} -> Date;
        _Other -> undefined
    end;
p_no_acl(Id, day_end, Context) ->
    case p_cached(Id, date_end, Context) of
        {{_, _, _} = Date, _} -> Date;
        _Other -> undefined
    end;
p_no_acl(Id, email_raw, Context) ->
    z_html:unescape(p_no_acl(Id, email, Context));
% p_no_acl(Id, title, Context) ->
%     Title = p_cached(Id, title, Context),
%     Title1 = case z_utils:is_empty(Title) of true -> undefined; false -> Title end,
%     case z_notifier:first(#rsc_property{id=Id, property=title, value=Title1}, Context) of
%         undefined -> Title;
%         OtherTitle -> OtherTitle
%     end;
p_no_acl(Id, title_slug, Context) ->
    case p_cached(Id, title_slug, Context) of
        undefined -> p_cached(Id, slug, Context);
        Slug -> Slug
    end;

% Check if the requested predicate is a readily available property or an edge
p_no_acl(Id, Predicate, Context) when is_integer(Id) ->
    p_cached(Id, Predicate, Context).


p_cached(Id, Property, Context) ->
    Value = case z_depcache:get(Id, Property, Context) of
        {ok, V} ->
            V;
        undefined ->
            case get_cached(Id, Context) of
                undefined -> undefined;
                PropList when is_binary(Property) ->
                    try
                        P1 = binary_to_existing_atom(Property, utf8),
                        proplists:get_value(P1, PropList)
                    catch
                        error:badarg ->
                            undefined
                    end;
                PropList ->
                    proplists:get_value(Property, PropList)
            end
    end,
    case Value of
        undefined ->
            % Unknown properties will be checked against the predicates, returns o(Predicate).
            case m_predicate:is_predicate(Property, Context) of
                true -> o(Id, Property, Context);
                false ->
                    undefined % z_notifier:first(#rsc_property{id=Id, property=Predicate}, Context)
            end;
        _ ->
            Value
    end.


non_informational_uri(Id, Context) ->
    case z_dispatcher:url_for(id, [{id, Id}], z_context:set_language(undefined, Context)) of
        undefined ->
            iolist_to_binary(z_context:abs_url(<<"/id/", (z_convert:to_binary(Id))/binary>>, Context));
        Url ->
            iolist_to_binary(z_context:abs_url(Url, Context))
    end.


%% Return a list of all edge predicates of this resource
-spec op(resource(), z:context()) -> list().
op(Id, Context) when is_integer(Id) ->
    m_edge:object_predicates(Id, Context);
op(undefined, _Context) ->
    [];
op(Id, Context) ->
    op(rid(Id, Context), Context).

%% @doc Used for dereferencing object edges inside template expressions
-spec o(resource(), z:context()) -> fun().
o(Id, _Context) ->
    fun(P, Context) -> o(Id, P, Context) end.

%% @doc Return the list of objects with a certain predicate
-spec o(resource(), atom(), z:context()) -> #rsc_list{}.
o(Id, Predicate, Context) when is_integer(Id) ->
    #rsc_list{list = m_edge:objects(Id, Predicate, Context)};
o(undefined, _Predicate, _Context) ->
    #rsc_list{list = []};
o(Id, Predicate, Context) ->
    o(rid(Id, Context), Predicate, Context).


%% Return the nth object in the predicate list
-spec o(resource(), atom(), pos_integer(), z:context()) -> resource_id() | undefined.
o(Id, Predicate, N, Context) when is_integer(Id) ->
    case m_edge:object(Id, Predicate, N, Context) of
        undefined -> undefined;
        ObjectId -> ObjectId
    end;
o(undefined, _Predicate, _N, _Context) ->
    undefined;
o(Id, Predicate, N, Context) ->
    o(rid(Id, Context), Predicate, N, Context).


%% Return a list of all edge predicates to this resource
-spec sp(resource(), z:context()) -> list().
sp(Id, Context) when is_integer(Id) ->
    m_edge:subject_predicates(Id, Context);
sp(undefined, _Context) ->
    [];
sp(Id, Context) ->
    sp(rid(Id, Context), Context).

%% Used for dereferencing subject edges inside template expressions
-spec s(resource(), z:context()) -> fun().
s(Id, _Context) ->
    fun(P, Context) -> s(Id, P, Context) end.

%% Return the list of subjects with a certain predicate
-spec s(resource(), atom(), z:context()) -> #rsc_list{}.
s(Id, Predicate, Context) when is_integer(Id) ->
    #rsc_list{list = m_edge:subjects(Id, Predicate, Context)};
s(undefined, _Predicate, _Context) ->
    #rsc_list{list = []};
s(Id, Predicate, Context) ->
    s(rid(Id, Context), Predicate, Context).

%% Return the nth object in the predicate list
-spec s(resource(), atom(), pos_integer(), z:context()) -> resource_id() | undefined.
s(Id, Predicate, N, Context) when is_integer(Id) ->
    case m_edge:subject(Id, Predicate, N, Context) of
        undefined -> undefined;
        SubjectId -> SubjectId
    end;
s(undefined, _Predicate, _N, _Context) ->
    undefined;
s(Id, Predicate, N, Context) ->
    s(rid(Id, Context), Predicate, N, Context).


%% Return the list of all media attached to the resource
-spec media(resource(), z:context()) -> list().
media(Id, Context) when is_integer(Id) ->
    m_edge:objects(Id, depiction, Context);
media(undefined, _Context) ->
    [];
media(Id, Context) ->
    media(rid(Id, Context), Context).


%% @doc Fetch a resource id from any input
-spec rid(resource()|{trans, _}, z:context()) -> resource_id() | undefined.
rid(Id, _Context) when is_integer(Id) ->
    Id;
rid({Id}, _Context) when is_integer(Id) ->
    Id;
rid(#rsc_list{list = [R | _]}, _Context) ->
    R;
rid(#rsc_list{list = []}, _Context) ->
    undefined;
rid(undefined, _Context) ->
    undefined;
rid(<<>>, _Context) ->
    undefined;
rid([], _Context) ->
    undefined;
rid({trans, _} = Tr, Context) ->
    rid(z_trans:lookup_fallback(Tr, Context), Context);
rid(UniqueName, Context) ->
    case z_utils:only_digits(UniqueName) of
        true -> z_convert:to_integer(UniqueName);
        false -> name_lookup(UniqueName, Context)
    end.


%% @doc Return the id of the resource with a certain unique name.
-spec name_lookup(resource_name(), z:context()) -> resource_id() | undefined.
name_lookup(Name, Context) ->
    Lower = z_string:to_name(Name),
    case z_depcache:get({rsc_name, Lower}, Context) of
        {ok, undefined} ->
            undefined;
        {ok, Id} ->
            Id;
        undefined ->
            Id = case z_db:q1("select id from rsc where name = $1", [Lower], Context) of
                undefined -> undefined;
                Value -> Value
            end,
            z_depcache:set({rsc_name, Lower}, Id, ?DAY, [Id, {rsc_name, Lower}], Context),
            Id
    end.


%% @doc Return the id of the resource with a certain uri.
-spec uri_lookup( resource_uri() | string(), z:context()) -> resource_id() | undefined.
uri_lookup(<<>>, _Context) ->
    undefined;
uri_lookup(Uri, Context) when is_binary(Uri) ->
    case z_depcache:get({rsc_uri, Uri}, Context) of
        {ok, undefined} ->
            undefined;
        {ok, Id} ->
            Id;
        undefined ->
            Id = case z_db:q1("select id from rsc where uri = $1", [Uri], Context) of
                undefined -> undefined;
                Value -> Value
            end,
            z_depcache:set({rsc_uri, Uri}, Id, ?DAY, [Id, {rsc_uri, Uri}], Context),
            Id
    end;
uri_lookup(Uri, Context) ->
    uri_lookup(z_convert:to_binary(Uri), Context).

%% @doc Check if the resource is exactly the category
-spec is_cat(resource(), atom(), z:context()) -> boolean().
is_cat(Id, Cat, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
            RscCatId = p(Id, category_id, Context),
            case RscCatId of
                CatId ->
                    true;
                _ ->
                    Path = m_category:get_path(RscCatId, Context),
                    lists:any(fun(X) -> X == CatId end, Path)
            end;
        _ ->
            false
    end.

%% @doc Return the categories and the inherited categories of the resource. Returns a list with category atoms
-spec is_a(resource(), z:context()) -> list(atom()).
is_a(Id, Context) ->
    RscCatId = p(Id, category_id, Context),
    m_category:is_a(RscCatId, Context).

%% @doc Return the categories and the inherited categories of the resource. Returns a list with
%% category ids
-spec is_a_id(resource(), z:context()) -> list(pos_integer()).
is_a_id(Id, Context) ->
    RscCatId = p(Id, category_id, Context),
    [RscCatId | m_category:get_path(RscCatId, Context)].

%% @doc Check if the resource is in a category.
-spec is_a(resource(), m_category:category(), z:context()) -> boolean().
is_a(Id, Cat, Context) ->
    RscCatId = p(Id, category_id, Context),
    m_category:is_a(RscCatId, Cat, Context).

-spec page_url( resource(), z:context() ) -> iodata() | undefined.
page_url(Id, Context) ->
    page_url(Id, false, Context).

-spec page_url_abs( resource(), z:context() ) -> iodata() | undefined.
page_url_abs(Id, Context) ->
    page_url(Id, true, Context).

-spec page_url( resource(), boolean(), z:context() ) -> iodata() | undefined.
page_url(Id, IsAbs, Context) ->
    case rid(Id, Context) of
        RscId when is_integer(RscId) ->
            CatPath = lists:reverse(is_a(Id, Context)),
            case z_notifier:first(#page_url{id = RscId, is_a = CatPath}, Context) of
                {ok, Url} ->
                    opt_url_abs(Url, IsAbs, Context);
                undefined ->
                    Args = [
                        {id,RscId},
                        {slug, slug(RscId, Context)}
                        | z_context:get(extra_args, Context, [])
                    ],
                    Url = page_url_path(CatPath, Args, Context),
                    case IsAbs of
                        true -> z_dispatcher:abs_url(Url, Context);
                        false -> Url
                    end
            end;
        _ ->
            undefined
    end.

page_url_path([], Args, Context) ->
    case z_dispatcher:url_for(page, Args, Context) of
        undefined ->
            lager:warning("Failed to get page url path. Is the 'page' dispatch rule missing?"),
            undefined;
        Url -> Url
    end;
page_url_path([CatName | Rest], Args, Context) ->
    case z_dispatcher:url_for(CatName, Args, Context) of
        undefined -> page_url_path(Rest, Args, Context);
        Url -> Url
    end.

slug(Id, Context) ->
    case m_rsc:p(Id, title_slug, Context) of
        undefined -> undefined;
        <<>> -> undefined;
        {trans, _} = Tr -> z_trans:lookup_fallback(Tr, Context);
        Slug when is_binary(Slug) -> Slug
    end.

%% @doc Depending on the context or the requested property we make the URL absolute
opt_url_abs(undefined, _IsAbs, _Context) ->
    undefined;
opt_url_abs(Url, true, Context) ->
    z_dispatcher:abs_url(Url, Context);
opt_url_abs(Url, false, Context) ->
    case z_context:get(absolute_url, Context) of
        true -> z_dispatcher:abs_url(Url, Context);
        _ -> Url
    end.

%% @doc Return the predicates that are valid combined with the predicates that
%% are actually used by the subject.
%% This list is to show which predicates are editable for the subject rsc.
%% @spec predicates_edit(Id, Context) -> [Predicate]
predicates_edit(Id, Context) ->
    ByCategory = m_predicate:for_subject(Id, Context),
    Present = m_edge:object_predicate_ids(Id, Context),
    ByCategory ++ Present.


%% @doc Ensure that a resource has a name, caller must have update rights.
-spec ensure_name(integer(), z:context()) -> ok.
ensure_name(Id, Context) ->
    case m_rsc:p_no_acl(Id, name, Context) of
        undefined ->
            CatId = m_rsc:p_no_acl(Id, category_id, Context),
            CatName = m_rsc:p_no_acl(CatId, name, Context),
            BaseName = z_string:to_name(iolist_to_binary([CatName, $_, english_title(Id, Context)])),
            BaseName1 = ensure_name_maxlength(BaseName),
            Name = ensure_name_unique(BaseName1, 0, Context),
            {ok, _} = m_rsc_update:update(Id, [{name, Name}], z_acl:sudo(Context)),
            ok;
        _Name ->
            ok
    end.

ensure_name_maxlength(<<Name:70/binary, _/binary>>) -> Name;
ensure_name_maxlength(Name) -> Name.

english_title(Id, Context) ->
    case m_rsc:p_no_acl(Id, title, Context) of
        Title when is_binary(Title) -> Title;
        {trans, []} -> <<>>;
        undefined -> <<>>;
        {trans, Tr} ->
            case proplists:get_value(en, Tr) of
                undefined ->
                    {_, T} = hd(Tr),
                    T;
                T ->
                    T
            end
    end.

ensure_name_unique(BaseName, N, Context) ->
    Name = iolist_to_binary([BaseName, postfix(N)]),
    case z_db:q1("select id from rsc where name = $1", [Name], Context) of
        undefined -> Name;
        _Id -> ensure_name_unique(BaseName, N + 1, Context)
    end.

postfix(0) -> <<>>;
postfix(N) -> integer_to_list(N).


%% @doc Common properties, these are used by exporter and backup routines.
common_properties(_Context) ->
    [
        title,

        category_id,
        creator_id,
        modifier_id,

        created,
        modified,

        publication_start,
        publication_end,

        is_published,
        is_featured,
        is_protected,

        chapeau,
        subtitle,
        short_title,
        summary,

        name_prefix,
        name_first,
        name_surname_prefix,
        name_surname,

        phone,
        phone_mobile,
        phone_alt,
        phone_emergency,

        email,
        website,

        date_start,
        date_end,
        date_remarks,

        address_street_1,
        address_street_2,
        address_city,
        address_state,
        address_postcode,
        address_country,

        mail_street_1,
        mail_street_2,
        mail_city,
        mail_state,
        mail_postcode,
        mail_country,

        location_lng,
        location_lat,

        body,
        body_extra,
        blocks,

        page_path,
        name,

        seo_noindex,
        title_slug,
        custom_slug,
        seo_desc
    ].

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Model for resource data. Interfaces between zotonic, templates and the database.
%% @end

%% Copyright 2009-2024 Marc Worrell
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
    m_post/3,
    m_delete/3,

    name_to_id/2,
    name_to_id_cat/3,

    page_path_to_id/2,

    get/2,
    get_export/2,
    get_raw/2,
    get_raw_lock/2,
    get_acl_props/2,
    insert/2,
    insert/3,
    delete/2,
    delete/3,
    merge_delete/3,
    merge_delete/4,
    update/3,
    update/4,
    duplicate/3,
    duplicate/4,
    touch/2,

    make_authoritative/2,

    exists/2,

    is_visible/2, is_editable/2, is_deletable/2, is_linkable/2,
    is_published_date/2,
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
    uri/2,
    uri_lookup/2,
    ensure_name/2,

    common_properties/1
]).

-include_lib("zotonic.hrl").

-type resource() :: resource_id()
                  | list(digits())
                  | resource_name()
                  | resource_uri()
                  | resource_uri_map()
                  | undefined.
-type resource_id() :: integer().
-type resource_name() :: string() | binary() | atom().
-type resource_uri() :: binary().
-type resource_uri_map() :: #{
        % <<"uri">> := resource_uri(),
        % <<"name">> := binary(),
        % <<"is_a">> := [ binary() ],
        binary() => term()
    }.
-type props() :: map().
-type props_legacy() :: proplists:proplist().
-type props_all() :: props() | props_legacy().
-type digits() :: 16#30..16#39.

-type update_function() :: fun(
        ( resource() | insert_rsc, props(), z:context() )
        ->
        {ok, UpdateProps :: props() } | {error, term()}
    ).

-type duplicate_options() :: list(duplicate_option()).
-type duplicate_option() :: edges
                          | {edges, boolean()}
                          | medium
                          | {medium, boolean()}.

% Range of resource id values in PostgreSQL.
% Make larger if moving to bigint resource ids.
-define(MIN_RSC_ID, 1).
-define(MAX_RSC_ID, 2147483647).

-define(is_valid_rsc_id(N), (is_integer(N) andalso N >= ?MIN_RSC_ID andalso N =< ?MAX_RSC_ID)).
-define(is_invalid_rsc_id(N), (is_integer(N) andalso (N < ?MIN_RSC_ID orelse N > ?MAX_RSC_ID))).

-export_type([
    resource/0,
    resource_id/0,
    resource_name/0,
    resource_uri/0,
    props/0,
    props_all/0,
    props_legacy/0,
    update_function/0,
    duplicate_option/0,
    duplicate_options/0
]).


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"-">>, <<"lookup">>, <<"page_path">> | Path ], _Msg, Context) ->
    Path1 = iolist_to_binary(lists:join($/, Path)),
    case page_path_to_id(Path1, Context) of
        {ok, Id} ->
            {#{
                <<"id">> => Id,
                <<"is_redirect">> => false
            }, []};
        {redirect, Id} ->
            {#{
                <<"id">> => Id,
                <<"is_redirect">> => true,
                <<"page_url">> => m_rsc:p(Id, <<"page_url">>, Context)
            }, []};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"-">>, <<"lookup">>, <<"rid">>, Id | Rest ], _Msg, Context) ->
    case rid(Id, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            {ok, {RscId, Rest}}
    end;
m_get([ <<"-">>, <<"lookup">>, <<"name">>, Name | Rest ], _Msg, Context) ->
    Result = name_to_id(Name, Context),
    {ok, {Result, Rest}};
m_get([ Id, <<"is_cat">>, Key | Rest ], _Msg, Context) ->
    {ok, {is_cat(Id, Key, Context), Rest}};
m_get([ Id, <<"is_a">>, Cat | Rest ], _Msg, Context) ->
    IsA = m_rsc:is_a(Id, Cat, Context),
    {ok, {IsA, Rest}};
m_get([ Id, <<"is_a">> ], _Msg, Context) ->
    IsA = m_rsc:is_a(Id, Context),
    {ok, {IsA, []}};
m_get([ Id, Key | Rest ], _Msg, Context) ->
    {ok, {p(Id, Key, Context), Rest}};
m_get([ Id ], _Msg, Context) ->
    case get_export(Id, Context) of
        {ok, Rsc} ->
            {ok, {Rsc, []}};
        {error, _} = Error ->
            Error
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc API to update or insert resources.
-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | ok | {error, term()}.
m_post([ Id ], #{ payload := Payload }, Context) when is_map(Payload) ->
    case m_rsc:rid(Id, Context) of
        undefined -> {error, enoent};
        RId -> m_rsc:update(RId, Payload, Context)
    end;
m_post([], #{ payload := Payload }, Context) when is_map(Payload) ->
    m_rsc:insert(Payload, Context).

%% @doc API to delete a resource
-spec m_delete( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | ok | {error, term()}.
m_delete([ Id ], _Msg, Context) ->
    delete(Id, Context).


%% @doc Return the id of the resource with the name
-spec name_to_id(resource_name(), z:context()) -> {ok, resource_id()} | {error, {unknown_rsc, resource_name()}}.
name_to_id(Name, _Context) when is_integer(Name) ->
    {ok, Name};
name_to_id(undefined, _Context) ->
    {error, {unknown_rsc, undefined}};
name_to_id(<<>>, _Context) ->
    {error, {unknown_rsc, <<>>}};
name_to_id("", _Context) ->
    {error, {unknown_rsc, <<>>}};
name_to_id(Name, Context) ->
    case name_lookup(Name, Context) of
        Id when is_integer(Id) -> {ok, Id};
        _ -> {error, {unknown_rsc, Name}}
    end.

-spec name_to_id_cat(resource(), resource_name(), z:context()) ->
        {ok, resource_id()} | {error, {unknown_rsc_cat, resource(), resource_name()}}.
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
-spec page_path_to_id( binary() | string(), z:context() ) ->
              {ok, resource_id()}
            | {redirect, resource_id()}
            | {error, {unknown_page_path, binary()}}
            | {error, {illegal_page_path, binary(), length|unicode}}.
page_path_to_id(Path, Context) ->
    Path1 = iolist_to_binary([ $/, z_string:trim(Path, $/) ]),
    case is_utf8(Path1) of
        true when size(Path1) < 200 ->
            case z_db:q1("select id from rsc where page_path = $1", [Path1], Context) of
                undefined ->
                    case z_db:q1(
                        "select id from rsc_page_path_log where page_path = $1",
                        [Path1],
                        Context)
                    of
                        OtherId when is_integer(OtherId) ->
                            {redirect, OtherId};
                        undefined ->
                            {error, {unknown_page_path, Path1}}
                    end;
                Id ->
                    {ok, Id}
            end;
        true ->
            {error, {illegal_page_path, Path1, length}};
        false ->
            {error, {illegal_page_path, Path1, unicode}}
    end.

is_utf8(<<>>) -> true;
is_utf8(<<_/utf8, S/binary>>) -> is_utf8(S);
is_utf8(_) -> false.


%% @doc Get all properties of a resource for export. This adds the
%% page urls for all languages and the language neutral uri to the
%% resource properties. Note that normally the page_url is a single
%% property without translation, as it is generated using the current
%% language context. As this export is used for UIs, all language
%% variants are added.
-spec get_export(Id, Context) -> {ok, Rsc} | {error, Reason} when
    Id :: resource(),
    Context :: z:context(),
    Rsc :: props(),
    Reason :: eacces | enoent.
get_export(Id, Context) ->
    RscId = m_rsc:rid(Id, Context),
    case get(RscId, Context) of
        undefined ->
            case m_rsc:exists(RscId, Context) of
                true ->
                    {error, eacces};
                false ->
                    {error, enoent}
            end;
        Rsc ->
            Rsc1 = add_export_props(RscId, Rsc, Context),
            {ok, Rsc1}
    end.

-spec add_export_props( Id, Rsc, Context ) -> Rsc1 when
    Id :: m_rsc:resource_id(),
    Rsc :: m_rsc:props(),
    Rsc1 :: m_rsc:props(),
    Context :: z:context().
add_export_props(Id, Rsc, Context) ->
    Languages = [ 'x-default' | z_language:enabled_language_codes(Context) ],
    PageUrl = page_url(Id, Context),
    PageUrlAbs = z_context:abs_url(PageUrl, Context),
    PageUrls = lists:map(
        fun(Lang) -> {Lang, page_url(Id, z_context:set_language(Lang, Context))} end,
        Languages),
    PageUrlsAbs = lists:map(
        fun({Lang, Url}) -> {Lang, z_context:abs_url(Url, Context)} end,
        PageUrls),
    ShortUrl = z_dispatcher:url_for(id, [ {id, Id}, {absolute_url, true} ], Context),
    Rsc#{
        <<"uri">> => uri(Id, z_context:set_language('x-default', Context)),
        <<"short_url">> => ShortUrl,
        <<"page_url">> => PageUrl,
        <<"page_url_abs">> => PageUrlAbs,       % canonical url
        <<"alternate_page_url">> => #trans{ tr = PageUrls },
        <<"alternate_page_url_abs">> => #trans{ tr = PageUrlsAbs }
    }.

%% @doc Read a whole resource. Return 'undefined' if the resource was
%%      not found, crash on database errors. The properties are filtered
%%      by the ACL.
-spec get(resource(), z:context()) -> map() | undefined.
get(Id, Context) ->
    case rid(Id, Context) of
        Rid when is_integer(Rid) ->
            case z_acl:rsc_visible(Id, Context) of
                true -> filter_props_acl(Rid, get_cached_unsafe(Rid, Context), Context);
                false -> undefined
            end;
        undefined ->
            undefined
    end.

%% @doc Read a whole resource. Return 'undefined' if the resource was
%%      not found, crash on database errors. The properties are NOT
%%      filtered by the ACL. This is used internally to cache the
%%      resource for all possible users.
-spec get_cached_unsafe(resource_id(), z:context()) -> map() | undefined.
get_cached_unsafe(Id, Context) when is_integer(Id) ->
    z_depcache:memo(
        fun() ->
            % Crash on database errors - we don't want errors to
            % be cached in the depcache.
            case get_raw(Id, false, Context) of
                {ok, Map} ->
                    MapDT = ensure_utc_dates(Map, Context),
                    z_notifier:foldr(#rsc_get{ id = Id }, MapDT, Context);
                {error, nodb} ->
                    undefined;
                {error, enoent} ->
                    undefined
            end
        end,
        Id,
        ?WEEK,
        Context).

-spec filter_props_acl(resource(), map() | undefined, z:context()) -> map() | undefined.
filter_props_acl(_Id, undefined, _Context) ->
    undefined;
filter_props_acl(Id, Props, Context) when is_map(Props) ->
    case z_acl:is_admin(Context) of
        true ->
            Props;
        false ->
            case z_acl:rsc_visible(Id, Context) of
                false ->
                    % Not accessible, only show some primary properties
                    #{
                        <<"id">> => maps:get(<<"id">>, Props),
                        <<"category_id">> => maps:get(<<"category_id">>, Props)
                    };
                true ->
                    % Accessible, filter on property level
                    maps:filter(
                        fun(Property, _Val) ->
                            z_acl:rsc_prop_visible(Id, Property, Context)
                        end,
                        Props)
            end
    end.

%% @doc Get the resource from the database, do not fetch the pivot fields and
%%      do not use the cached result. The properties are NOT filtered by the ACL.
-spec get_raw(resource(), z:context()) -> {ok, map()} | {error, term()}.
get_raw(Id, Context) when is_integer(Id) ->
    get_raw(Id, false, Context).

%% @doc Same as get_raw/2 but also lock the resource for update.
%%      The properties are NOT filtered by the ACL.
-spec get_raw_lock(resource(), z:context()) -> {ok, map()} | {error, term()}.
get_raw_lock(Id, Context) ->
    get_raw(Id, true, Context).

-spec get_raw(resource(), boolean(), z:context()) -> {ok, map()} | {error, term()}.
get_raw(Id, IsLock, Context) when ?is_valid_rsc_id(Id) ->
    SQL = case z_memo:get(rsc_raw_sql) of
        undefined ->
            AllCols = [ z_convert:to_binary(C) || C <- z_db:column_names(rsc, Context) ],
            DataCols = lists:filter(
                fun (<<"pivot_geocode">>) -> true;
                    (<<"pivot_geocode_qhash">>) -> true;
                    (<<"pivot_location_lat">>) -> true;
                    (<<"pivot_location_lng">>) -> true;
                    (<<"pivot_", _/binary>>) -> false;
                    (_) -> true
                end,
                AllCols),
            Query = iolist_to_binary([
                "select ",lists:join($,, DataCols),
                " from rsc where id = $1"
            ]),
            z_memo:set(rsc_raw_sql, Query),
            Query;
        Memo ->
            Memo
    end,
    SQL1 = case IsLock of
        true -> z_convert:to_list(SQL) ++ " for update";
        false -> z_convert:to_list(SQL)
    end,
    case z_db:qmap_props_row(SQL1, [ Id ], [ {keys, binary} ], Context) of
        {ok, Map} ->
            {ok, map_language_atoms(Map)};
        {error, _} = Error ->
            Error
    end;
get_raw(Id, _IsLock, _Context) when ?is_invalid_rsc_id(Id) ->
    {error, enoent};
get_raw(undefined, _IsLock, _Context) ->
    {error, enoent};
get_raw(Id, IsLock, Context) ->
    get_raw(rid(Id, Context), IsLock, Context).

%% The languages are stored as a psql array, map to the internal atom
%% representation. On update this is mapped to binaries in z_db:update/3.
map_language_atoms(#{ <<"language">> := Lang } = Map) when is_list(Lang) ->
    Lang1 = lists:filtermap(
        fun(Iso) ->
            case z_language:to_language_atom(Iso) of
                {ok, IsoAtom} -> {true, IsoAtom};
                {error, _} -> false
            end
        end,
        Lang),
    Map#{ <<"language">> => Lang1 };
map_language_atoms(Map) ->
    Map#{ <<"language">> => [] }.

%% Fix old records which had serialized data in localtime and no date_is_all_day flag
ensure_utc_dates(#{ <<"tz">> := _ } = Map, _Context) ->
    Map;
ensure_utc_dates(Map, Context) ->
    % Convert dates, assuming the system's default timezone
    DateStart = maps:find(<<"date_start">>, Map),
    DateEnd = maps:find(<<"date_end">>, Map),
    IsAllDay = case {DateStart, DateEnd} of
        {{ok, {_, {0, 0, _StartSec}}}, {ok, {_, {23, 59, _EndSec}}}} ->
            true;
        _ ->
            false
    end,
    Map1 = maps:map(
        fun(K, V) ->
            ensure_utc_date(K, V, IsAllDay)
        end,
        Map),
    Map1#{
        <<"tz">> => z_context:tz(Context),
        <<"date_is_all_day">> => IsAllDay
    }.

% Only date_start, date_end and org_pubdate should be converted
ensure_utc_date(K, Date, IsAllDay) ->
    try
        ensure_utc_date_1(K, Date, IsAllDay)
    catch
        error:badarg ->
            undefined
    end.

ensure_utc_date_1(<<"date_start">>, DT, false) when is_tuple(DT) ->
    hd(calendar:local_time_to_universal_time_dst(DT));
ensure_utc_date_1(<<"date_end">>, DT, false) when is_tuple(DT) ->
    hd(calendar:local_time_to_universal_time_dst(DT));
ensure_utc_date_1(<<"org_pubdate">>, DT, _IsAllDay) when is_tuple(DT) ->
    hd(calendar:local_time_to_universal_time_dst(DT));
ensure_utc_date_1(_K, P, _IsAllDay) ->
    P.


%% @doc Get the ACL fields for the resource with the id.
%% Will always return a valid record, even if the resource does not exist.
-spec get_acl_props(Id , Context) -> AclProps when
    Id :: resource(),
    Context :: z:context(),
    AclProps :: #acl_props{}.
get_acl_props(Id, Context) when ?is_valid_rsc_id(Id) ->
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
get_acl_props(Id, _Context) when ?is_invalid_rsc_id(Id) ->
    #acl_props{
        is_published = false
    };
get_acl_props(Name, Context) ->
    case rid(Name, Context) of
        undefined ->
            #acl_props{
                is_published = false
            };
        Id -> get_acl_props(Id, Context)
    end.


%% @doc Insert a new resource
-spec insert(props_all(), z:context()) -> {ok, resource_id()} | {error, term()}.
insert(Props, Context) ->
    m_rsc_update:insert(Props, [], Context).

-spec insert(props_all(), list(), z:context()) -> {ok, resource_id()} | {error, term()}.
insert(Props, Options, Context) ->
    m_rsc_update:insert(Props, Options, Context).

%% @doc Delete a resource
-spec delete(resource(), z:context()) -> ok | {error, term()}.
delete(Id, Context) ->
    m_rsc_update:delete(Id, undefined, Context).

-spec delete(resource(), resource(), z:context()) -> ok | {error, term()}.
delete(Id, FollowUp, Context) ->
    m_rsc_update:delete(Id, FollowUp, Context).

%% @doc Merge a resource with another, delete the loser.
-spec merge_delete(resource(), resource(), z:context()) -> ok | {error, term()}.
merge_delete(WinnerId, LoserId, Context) ->
    m_rsc_update:merge_delete(WinnerId, LoserId, [ {is_merge_trans, false} ], Context).

%% @doc Merge a resource with another, delete the loser.
-spec merge_delete(resource(), resource(), list(), z:context()) -> ok | {error, term()}.
merge_delete(WinnerId, LoserId, Options, Context) ->
    m_rsc_update:merge_delete(WinnerId, LoserId, Options, Context).

%% @doc Update a resource
-spec update(
        resource(),
        props_all() | update_function(),
        z:context()
    ) -> {ok, resource()} | {error, term()}.
update(Id, Props, Context) ->
    m_rsc_update:update(Id, Props, Context).

-spec update(
        resource(),
        props_all() | update_function(),
        list(),
        z:context()
    ) -> {ok, resource()} | {error, term()}.
update(Id, Props, Options, Context) ->
    m_rsc_update:update(Id, Props, Options, Context).


%% @doc Duplicate a resource.
-spec duplicate(resource(), props_all(), z:context()) ->
    {ok, NewId :: resource_id()} | {error, Reason :: term()}.
duplicate(Id, Props, Context) ->
    m_rsc_update:duplicate(Id, Props, Context).

-spec duplicate(resource(), props_all(), duplicate_options(), z:context()) ->
    {ok, NewId :: resource_id()} | {error, Reason :: term()}.
duplicate(Id, Props, Options, Context) ->
    m_rsc_update:duplicate(Id, Props, Options, Context).


%% @doc "Touch" the rsc, incrementing the version nr and the modification date/ modifier_id.
%% This should be called as part of another update or transaction and does not resync the caches,
%% and does not check the ACL.  After "touching" the resource will be re-pivoted.
-spec touch(resource(), z:context()) -> {ok, resource_id()} | {error, enoent}.
touch(Id, Context) ->
    case z_db:q(
        "update rsc set version = version + 1, modifier_id = $1, modified = now() where id = $2",
        [z_acl:user(Context), rid(Id, Context)],
        Context
    ) of
        1 ->
            z_depcache:flush(Id, Context),
            IsA = m_rsc:is_a(Id, Context),
            Topic = [ <<"model">>, <<"rsc">>, <<"event">>, Id, <<"update">> ],
            z_mqtt:publish(
                Topic,
                #{
                    id => Id,
                    pre_is_a => IsA,
                    post_is_a => IsA
                },
                Context),
            {ok, Id};
        0 ->
            {error, enoent}
    end.


%% @doc Make a resource authoritative. This removes the uri from the resource and sets the
%% resource as 'authoritative'. The uri is added to the m_rsc_gone delete log.
-spec make_authoritative( m_rsc:resource(), z:context() ) -> {ok, resource_id()} | {error, term()}.
make_authoritative(RscId, Context)  ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        Id ->
            case m_rsc:p_no_acl(Id, is_authoritative, Context) of
                true ->
                    {error, authoritative};
                false ->
                    case z_acl:rsc_editable(Id, Context) of
                        true ->
                            {ok, _} = m_rsc_gone:gone(Id, Id, Context),
                            NewProps = #{
                                <<"is_authoritative">> => true,
                                <<"uri">> => undefined
                            },
                            m_rsc_update:update(Id, NewProps, Context);
                        false ->
                            {error, eacces}
                    end
            end
    end.


-spec exists(resource(), z:context()) -> boolean().
exists(Id, Context) ->
    case rid(Id, Context) of
        Rid when ?is_valid_rsc_id(Rid) ->
            case p_no_acl(Rid, <<"id">>, Context) of
                Rid -> true;
                undefined -> false
            end;
        _ ->
            false
    end.

%% @doc Check if a resource is visible for the current user. Non existing
%% resources are visible.
-spec is_visible(resource(), z:context()) -> boolean().
is_visible(Id, Context) ->
    z_acl:rsc_visible(Id, Context).

%% @doc Check if a resource can be edited by the current user. Non existing
%% resources are not editable.
-spec is_editable(resource(), z:context()) -> boolean().
is_editable(Id, Context) ->
    z_acl:rsc_editable(Id, Context).

%% @doc Check if a resource can be deleted by the current user. Non existing
%% resources are not deletable.
-spec is_deletable(resource(), z:context()) -> boolean().
is_deletable(Id, Context) ->
    z_acl:rsc_deletable(Id, Context).

%% @doc Check if an connection can be added to the resource. Returns true if
%% the ACL allows adding a 'relation' edge from the resource to itself.
-spec is_linkable(resource(), z:context()) -> boolean().
is_linkable(Id, Context) ->
    z_acl:rsc_linkable(Id, Context).

%% @doc Check if the resource is the current user.
-spec is_me(resource(), z:context()) -> boolean().
is_me(Id, Context) ->
    case rid(Id, Context) of
        RscId when is_integer(RscId) ->
            z_acl:user(Context) =:= RscId;
        _ ->
            false
    end.

%% @doc Check if a resource is published and within its publication start/end
%% range. If a resource does not exist then false is returned. The checks are
%% done without checking access permissions.
-spec is_published_date(Id, Context) -> IsPublished when
    Id :: resource(),
    Context :: z:context(),
    IsPublished :: boolean().
is_published_date(Id, Context) ->
    case rid(Id, Context) of
        RscId when is_integer(RscId) ->
            case p_no_acl(RscId, <<"is_published">>, Context) of
                true ->
                    Date = erlang:universaltime(),
                    p_no_acl(RscId, <<"publication_start">>, Context) =< Date
                        andalso p_no_acl(RscId, <<"publication_end">>, Context) >= Date;
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
-spec p(Resource, Property, Context) -> Value when
    Resource :: resource(),
    Property :: atom() | binary() | string(),
    Context :: z:context(),
    Value :: term() | undefined.
p(_Id, undefined, _Context) ->
    undefined;
p(undefined, _Property, _Context) ->
    undefined;
p(Id, Property, Context) when is_atom(Property) ->
    p(Id, atom_to_binary(Property, utf8), Context);
p(Id, Property, Context) when is_list(Property) ->
    p(Id, unicode:characters_to_binary(Property, utf8), Context);
p(Id, Property, Context)
    when   Property =:= <<"category_id">>
    orelse Property =:= <<"category">>
    orelse Property =:= <<"page_url">>
    orelse Property =:= <<"page_url_abs">>
    orelse Property =:= <<"is_a">>
    orelse Property =:= <<"uri">>
    orelse Property =:= <<"uri_raw">>
    orelse Property =:= <<"is_authoritative">>
    orelse Property =:= <<"is_published">>
    orelse Property =:= <<"is_published_date">>
    orelse Property =:= <<"is_visible">>
    orelse Property =:= <<"is_editable">>
    orelse Property =:= <<"is_deletable">>
    orelse Property =:= <<"is_linkable">>
    orelse Property =:= <<"is_me">>
    orelse Property =:= <<"exists">>
    orelse Property =:= <<"id">>
    orelse Property =:= <<"privacy">>
    orelse Property =:= <<"default_page_url">> ->
    p_no_acl(rid(Id, Context), Property, Context);
p(Id, Property, Context) when is_binary(Property) ->
    p1(Id, Property, Context).

p1(Id, Property, Context) ->
    case rid(Id, Context) of
        undefined ->
            undefined;
        RId ->
            case z_acl:rsc_visible(RId, Context) of
                true ->
                    case z_acl:rsc_prop_visible(RId, prop_for_acl(Property), Context) of
                        true -> p_no_acl(RId, Property, Context);
                        false -> undefined
                    end;
                false ->
                    undefined
            end
    end.

prop_for_acl(<<"email_raw">>) -> <<"email">>;
prop_for_acl(<<"day_start">>) -> <<"date_start">>;
prop_for_acl(<<"day_end">>) -> <<"date_end">>;
prop_for_acl(P) -> P.

%% Fetch property from a resource; but return a default value if not found.
p(Id, Property, DefaultValue, Context) ->
    case p(Id, Property, Context) of
        undefined -> DefaultValue;
        Value -> Value
    end.


%% @doc Fetch a property from a resource, no ACL check is done.
p_no_acl(undefined, _Predicate, _Context) ->
    undefined;
p_no_acl(_Id, undefined, _Context) ->
    undefined;
p_no_acl(Id, Prop, Context) when is_atom(Prop) ->
    p_no_acl(Id, atom_to_binary(Prop, utf8), Context);
p_no_acl(Id, Prop, Context) when not is_integer(Id) ->
    p_no_acl(rid(Id, Context), Prop, Context);
p_no_acl(Id, <<"o">>, Context) -> o(Id, Context);
p_no_acl(Id, <<"s">>, Context) -> s(Id, Context);
p_no_acl(Id, <<"op">>, Context) -> op(Id, Context);
p_no_acl(Id, <<"sp">>, Context) -> sp(Id, Context);
p_no_acl(Id, <<"is_me">>, Context) -> is_me(Id, Context);
p_no_acl(Id, <<"is_visible">>, Context) -> is_visible(Id, Context);
p_no_acl(Id, <<"is_editable">>, Context) -> is_editable(Id, Context);
p_no_acl(Id, <<"is_deletable">>, Context) -> is_deletable(Id, Context);
p_no_acl(Id, <<"is_linkable">>, Context) -> is_linkable(Id, Context);
p_no_acl(Id, <<"is_published_date">>, Context) -> is_published_date(Id, Context);
p_no_acl(Id, <<"is_a">>, Context) -> is_a(Id, Context);
p_no_acl(Id, <<"exists">>, Context) -> exists(Id, Context);
p_no_acl(Id, <<"page_url_abs">>, Context) ->
    case p_no_acl(Id, <<"page_path">>, Context) of
        undefined -> page_url(Id, true, Context);
        PagePath ->
            opt_url_abs(z_notifier:foldl(#url_rewrite{args = [{id, Id}]}, PagePath, Context), true, Context)
    end;
p_no_acl(Id, <<"page_url">>, Context) ->
    case p_no_acl(Id, <<"page_path">>, Context) of
        undefined -> page_url(Id, false, Context);
        PagePath ->
            opt_url_abs(z_notifier:foldl(#url_rewrite{args = [{id, Id}]}, PagePath, Context), false, Context)
    end;
p_no_acl(Id, <<"translation">>, Context) ->
    fun(Code) ->
        fun(Prop) ->
            case p_no_acl(Id, Prop, Context) of
                #trans{} = Translated ->
                    z_trans:lookup(Translated, Code, Context);
                Value -> Value
            end
        end
    end;
p_no_acl(Id, <<"default_page_url">>, Context) -> page_url(Id, Context);
p_no_acl(Id, <<"image_url_abs">>, Context) -> z_context:abs_url(p_no_acl(Id, <<"image_url">>, Context), Context);
p_no_acl(Id, <<"thumbnail_url_abs">>, Context) -> z_context:abs_url(p_no_acl(Id, <<"thumbnail_url">>, Context), Context);
p_no_acl(Id, <<"image_url">>, Context) -> image_url(Id, <<"image">>, Context);
p_no_acl(Id, <<"thumbnail_url">>, Context) -> image_url(Id, <<"thumbnail">>, Context);
p_no_acl(Id, <<"uri">>, Context) -> uri(Id, Context);
p_no_acl(Id, <<"uri_raw">>, Context) -> p_cached(Id, <<"uri">>, Context);
p_no_acl(Id, <<"category">>, Context) -> m_category:get(p_no_acl(Id, <<"category_id">>, Context), Context);
p_no_acl(Id, <<"media">>, Context) -> media(Id, Context);
p_no_acl(Id, <<"medium">>, Context) -> m_media:get(Id, Context);
p_no_acl(Id, <<"depiction">>, Context) -> m_media:depiction(Id, Context);
p_no_acl(Id, <<"predicates_edit">>, Context) -> predicates_edit(Id, Context);
p_no_acl(Id, <<"day_start">>, Context) ->
    case p_cached(Id, <<"date_start">>, Context) of
        {{_, _, _} = Date, _} -> Date;
        _Other -> undefined
    end;
p_no_acl(Id, <<"day_end">>, Context) ->
    case p_cached(Id, <<"date_end">>, Context) of
        {{_, _, _} = Date, _} -> Date;
        _Other -> undefined
    end;
p_no_acl(Id, <<"email_raw">>, Context) ->
    z_html:unescape(p_no_acl(Id, <<"email">>, Context));
% p_no_acl(Id, title, Context) ->
%     Title = p_cached(Id, title, Context),
%     Title1 = case z_utils:is_empty(Title) of true -> undefined; false -> Title end,
%     case z_notifier:first(#rsc_property{id=Id, property=title, value=Title1}, Context) of
%         undefined -> Title;
%         OtherTitle -> OtherTitle
%     end;
p_no_acl(Id, <<"title_slug">>, Context) ->
    case p_cached(Id, <<"title_slug">>, Context) of
        undefined -> p_cached(Id, <<"slug">>, Context);
        Slug -> Slug
    end;

% Check if the requested predicate is a readily available property or an edge
p_no_acl(Id, Predicate, Context) when is_integer(Id) ->
    p_cached(Id, Predicate, Context).


p_cached(Id, Property, Context) ->
    case p_cached_1(Id, Property, Context) of
        undefined ->
            case z_rdf_props:mapping(Property) of
                undefined ->
                    undefined;
                MappedProp ->
                    p_cached_1(Id, MappedProp, Context)
            end;
        V ->
            V
    end.

p_cached_1(Id, Property, Context) ->
    case z_depcache:get(Id, Property, Context) of
        {ok, V} ->
            V;
        undefined ->
            case get_cached_unsafe(Id, Context) of
                undefined -> undefined;
                Map ->
                    maps:get(Property, Map, undefined)
            end
    end.


%% @doc Determine the non informational uri of a resource.
-spec uri( resource() | undefined, z:context() ) -> binary() | undefined.
uri(Id, Context) when is_integer(Id) ->
    case p_cached(Id, <<"uri">>, Context) of
        Empty when Empty =:= <<>>; Empty =:= undefined ->
            uri_dispatch(Id, Context);
        Uri ->
            Uri
    end;
uri(undefined, _Context) ->
    undefined;
uri(Id, Context) ->
    uri(rid(Id, Context), Context).

uri_dispatch(Id, Context) ->
    DispatchId = case is_named_meta(Id, Context) of
        {true, Name} -> Name;
        false -> Id
    end,
    case z_dispatcher:url_for(id, [{id, DispatchId}], z_context:set_language('x-default', Context)) of
        undefined ->
            iolist_to_binary(z_context:abs_url(<<"/id/", (z_convert:to_binary(DispatchId))/binary>>, Context));
        Url ->
            iolist_to_binary(z_context:abs_url(Url, Context))
    end.

is_named_meta(Id, Context) ->
    case p_cached(Id, <<"name">>, Context) of
        Empty when Empty =:= <<>>; Empty =:= undefined ->
            false;
        Name ->
            case is_a(Id, meta, Context) of
                true ->
                    {true, Name};
                false ->
                    false
            end
    end.

image_url(Id, Mediaclass, Context) ->
    case z_media_tag:url(Id, [ {mediaclass, Mediaclass} ], Context) of
        {ok, <<>>} ->
            undefined;
        {ok, Url} ->
            Url;
        {error, _} ->
            undefined
    end.


%% @doc Return a list of all edge predicates of this resource
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
-spec o(resource(), atom(), z:context()) -> list().
o(undefined, _Predicate, _Context) ->
    [];
o(_Id, undefined, _Context) ->
    [];
o(Id, Predicate, Context) when is_integer(Id) ->
    m_edge:objects(Id, Predicate, Context);
o(Id, Predicate, Context) ->
    o(rid(Id, Context), Predicate, Context).


%% @doc Return the nth object in the predicate list
-spec o(resource(), atom(), pos_integer(), z:context()) -> resource_id() | undefined.
o(_Id, undefined, _N, _Context) ->
    undefined;
o(undefined, _Predicate, _N, _Context) ->
    undefined;
o(Id, Predicate, N, Context) when is_integer(Id) ->
    case m_edge:object(Id, Predicate, N, Context) of
        undefined -> undefined;
        ObjectId -> ObjectId
    end;
o(Id, Predicate, N, Context) ->
    o(rid(Id, Context), Predicate, N, Context).


%% @doc Return a list of all edge predicates to this resource
-spec sp(resource(), z:context()) -> list().
sp(undefined, _Context) ->
    [];
sp(Id, Context) when is_integer(Id) ->
    m_edge:subject_predicates(Id, Context);
sp(Id, Context) ->
    sp(rid(Id, Context), Context).

%% @doc Used for dereferencing subject edges inside template expressions
-spec s(resource(), z:context()) -> fun().
s(Id, _Context) ->
    fun(P, Context) -> s(Id, P, Context) end.

%% @doc Return the list of subjects with a certain predicate
-spec s(resource(), atom(), z:context()) -> list().
s(undefined, _Predicate, _Context) ->
    [];
s(_Id, undefined, _Context) ->
    [];
s(Id, Predicate, Context) when is_integer(Id) ->
    m_edge:subjects(Id, Predicate, Context);
s(Id, Predicate, Context) ->
    s(rid(Id, Context), Predicate, Context).

%% @doc Return the nth object in the predicate list.
-spec s(resource(), atom(), pos_integer(), z:context()) -> resource_id() | undefined.
s(undefined, _Predicate, _N, _Context) ->
    undefined;
s(_Id, undefined, _N, _Context) ->
    undefined;
s(Id, Predicate, N, Context) when is_integer(Id) ->
    case m_edge:subject(Id, Predicate, N, Context) of
        undefined -> undefined;
        SubjectId -> SubjectId
    end;
s(Id, Predicate, N, Context) ->
    s(rid(Id, Context), Predicate, N, Context).


%% @doc Return the list of all media attached to the resource with the depiction
%% predicate.
-spec media(Id, Context) -> MediaIds when
    Id :: resource(),
    Context :: z:context(),
    MediaIds :: list( resource_id() ).
media(Id, Context) when is_integer(Id) ->
    m_edge:objects(Id, depiction, Context);
media(undefined, _Context) ->
    [];
media(Id, Context) ->
    media(rid(Id, Context), Context).


%% @doc Fetch a resource id from any input
-spec rid(ResourceReference, Context) -> Id | undefined when
    ResourceReference :: resource() | #trans{},
    Context :: z:context(),
    Id :: resource_id().
rid(Id, _Context) when ?is_valid_rsc_id(Id) ->
    Id;
rid(Id, _Context) when ?is_invalid_rsc_id(Id) ->
    undefined;
rid({Id}, _Context) when ?is_valid_rsc_id(Id) ->
    Id;
rid({Id}, _Context) when ?is_invalid_rsc_id(Id) ->
    undefined;
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
rid([X|_], _Context) when not is_integer(X) ->
    undefined;
rid(#trans{} = Tr, Context) ->
    rid(z_trans:lookup_fallback(Tr, Context), Context);
rid(<<"urn:", _/binary>> = Uri, Context) ->
    uri_lookup(Uri, Context);
rid(<<"http:", _/binary>> = Uri, Context) ->
    uri_lookup(Uri, Context);
rid(<<"https:", _/binary>> = Uri, Context) ->
    uri_lookup(Uri, Context);
rid(<<"/", _/binary>> = Uri, Context) ->
    uri_lookup(Uri, Context);
rid("urn:" ++ _ = Uri, Context) ->
    uri_lookup(Uri, Context);
rid("http:" ++ _ = Uri, Context) ->
    uri_lookup(Uri, Context);
rid("https:" ++ _ = Uri, Context) ->
    uri_lookup(Uri, Context);
rid("/" ++ _ = Uri, Context) ->
    uri_lookup(Uri, Context);
rid(#{ <<"uri">> := Uri } = Map, Context) ->
    Name = maps:get(<<"name">>, Map, undefined),
    case rid(Uri, Context) of
        undefined ->
            case rid(Name, Context) of
                undefined ->
                    undefined;
                Id ->
                    case is_matching_category(maps:get(<<"is_a">>, Map, undefined), is_a(Id, Context)) of
                        true -> Id;
                        false -> undefined
                    end
            end;
        Id ->
            Id
    end;
rid(#{ <<"@id">> := Uri }, Context) ->
    uri_lookup(Uri, Context);
rid(MaybeName, Context) when is_binary(MaybeName) ->
    case z_utils:only_digits(MaybeName) of
        true ->
            Id = z_convert:to_integer(MaybeName),
            if
                ?is_valid_rsc_id(Id) -> Id;
                true -> false
            end;
        false ->
            case binary:match(MaybeName, <<":">>) of
                nomatch -> name_lookup(MaybeName, Context);
                _ -> uri_lookup(MaybeName, Context)
            end
    end;
rid(MaybeName, Context) when is_list(MaybeName) ->
    case z_utils:only_digits(MaybeName) of
        true ->
            Id = z_convert:to_integer(MaybeName),
            if
                ?is_valid_rsc_id(Id) -> Id;
                true -> false
            end;
        false ->
            case lists:any(fun(C) -> C =:= $: end, MaybeName) of
                false -> name_lookup(MaybeName, Context);
                true -> uri_lookup(MaybeName, Context)
            end
    end;
rid(MaybeName, Context) ->
    name_lookup(MaybeName, Context).

is_matching_category(undefined, _) -> true;
is_matching_category([], _) -> true;
is_matching_category(ExtIsA, LocalIsA) ->
    ExtIsA1 = [ z_convert:to_binary(A) || A <- ExtIsA ],
    LocalIsA1 = [ z_convert:to_binary(A) || A <- LocalIsA ],
    lists:any( fun(A) -> lists:member(A, ExtIsA1) end, LocalIsA1 ).


%% @doc Return the id of the resource with a certain unique name.
-spec name_lookup(resource_name(), z:context()) -> resource_id() | undefined.
name_lookup(Name, Context) ->
    try
        z_string:to_name(Name)
    of
        Lower ->
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
            end
    catch
        error:badarg ->
            undefined
    end.


%% @doc Return the id of the resource with a certain uri.
-spec uri_lookup( resource_uri() | string(), z:context()) -> resource_id() | undefined.
uri_lookup(<<>>, _Context) ->
    undefined;
uri_lookup(Uri, Context) when is_binary(Uri) ->
    case is_rsc_uri(Uri) of
        true ->
            case is_local_uri(Uri, Context) of
                true ->
                    % Check for id in URL
                    local_uri_to_id(Uri, Context);
                false ->
                    case z_depcache:get({rsc_uri, Uri}, Context) of
                        {ok, undefined} ->
                            undefined;
                        {ok, Id} ->
                            Id;
                        undefined ->
                            Id = uri_lookup_1(Uri, Context),
                            z_depcache:set({rsc_uri, Uri}, Id, ?DAY, [Id, {rsc_uri, Uri}], Context),
                            Id
                    end
            end;
        false ->
            undefined
    end;
uri_lookup(Uri, Context) ->
    uri_lookup(z_convert:to_binary(Uri), Context).

% Check if URI is imported or replaced by another resource
uri_lookup_1(Uri, Context) ->
    case z_db:q1("select id from rsc where uri = $1", [Uri], Context) of
        undefined ->
            case m_rsc_gone:get_uri(Uri, Context) of
                undefined -> undefined;
                Gone -> proplists:get_value(new_id, Gone)
            end;
        Id ->
            Id
    end.


%% @doc Check if the hostname in an URL matches the current site
is_local_uri(<<"/", C, _/binary>>, _Context) when C =/= $/ ->
    true;
is_local_uri(Uri, Context) ->
    Site = z_context:site(Context),
    case z_sites_dispatcher:get_site_for_url(Uri) of
        {ok, Site} ->
            true;
        _ ->
            % Unknown or some other site
            false
    end.


%% @doc Use the dispatcher to extract the id from the local URI
local_uri_to_id(<<$/, C, _/binary>> = Path, Context) when C =/= $/ ->
    case z_sites_dispatcher:dispatch_path(Path, Context) of
        {ok, #{
            controller_options := Options,
            bindings := Bindings
        }} ->
            Id = maps:get(id, Bindings, proplists:get_value(id, Options)),
            rid(Id, Context);
        _ ->
            % Non matching sites and illegal urls are rejected
            undefined
    end;
local_uri_to_id(Uri, Context) ->
    Site = z_context:site(Context),
    case z_sites_dispatcher:dispatch_url(Uri) of
        {ok, #{
            site := Site,
            controller_options := Options,
            bindings := Bindings
        }} ->
            Id = maps:get(id, Bindings, proplists:get_value(id, Options)),
            rid(Id, Context);
        _ ->
            % Non matching sites and illegal urls are rejected
            undefined
    end.

is_rsc_uri(<<"urn:", _/binary>>) -> true;
is_rsc_uri(<<"http:", _/binary>>) -> true;
is_rsc_uri(<<"https:", _/binary>>) -> true;
is_rsc_uri(<<"/", _/binary>>) -> true;
is_rsc_uri(B) ->
    binary:match(B, <<":">>) =/= nomatch.


%% @doc Check if the resource is exactly the category
-spec is_cat(resource(), atom(), z:context()) -> boolean().
is_cat(Id, Cat, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
            RscCatId = p(Id, <<"category_id">>, Context),
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

%% @doc Return the categories and the inherited categories of the resource. Returns a list with category atoms.
%% The first atom is the most generic category, the last is the most specific. Example:
%% [text, article, news, local_news]
-spec is_a(resource(), z:context()) -> list(atom()).
is_a(Id, Context) ->
    RscCatId = p(Id, category_id, Context),
    m_category:is_a(RscCatId, Context).

%% @doc Return the categories and the inherited categories of the resource. Returns a list with
%% category ids. The first id is the most generic category, the last is the most specific.
-spec is_a_id(resource(), z:context()) -> list(m_rsc:resource_id()).
is_a_id(Id, Context) ->
    RscCatId = p(Id, <<"category_id">>, Context),
    m_category:get_path(RscCatId, Context) ++ [RscCatId].

%% @doc Check if the resource is in a category.
-spec is_a(resource(), m_category:category(), z:context()) -> boolean().
is_a(Id, Cat, Context) ->
    RscCatId = p(Id, <<"category_id">>, Context),
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
            ?LOG_WARNING("Failed to get page url path. Is the 'page' dispatch rule missing?"),
            undefined;
        Url -> Url
    end;
page_url_path([CatName | Rest], Args, Context) ->
    case z_dispatcher:url_for(CatName, Args, Context) of
        undefined -> page_url_path(Rest, Args, Context);
        Url -> Url
    end.

slug(Id, Context) ->
    case p(Id, <<"title_slug">>, Context) of
        undefined -> undefined;
        <<>> -> undefined;
        #trans{} = Tr -> z_trans:lookup_fallback(Tr, Context);
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
    case p_no_acl(Id, <<"name">>, Context) of
        undefined ->
            CatId = p_no_acl(Id, <<"category_id">>, Context),
            CatName = p_no_acl(CatId, <<"name">>, Context),
            BaseName = z_string:to_name(iolist_to_binary([CatName, $_, english_title(Id, Context)])),
            BaseName1 = ensure_name_maxlength(BaseName),
            Name = ensure_name_unique(BaseName1, 0, Context),
            {ok, _} = m_rsc_update:update(Id, [{<<"name">>, Name}], z_acl:sudo(Context)),
            ok;
        _Name ->
            ok
    end.

ensure_name_maxlength(<<Name:70/binary, _/binary>>) -> Name;
ensure_name_maxlength(Name) -> Name.

english_title(Id, Context) ->
    Text = p_no_acl(Id, <<"title">>, Context),
    z_convert:to_binary(z_trans:lookup_fallback(Text, [en, 'en-us', 'en-gb'], Context)).

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
        <<"title">>,

        <<"category_id">>,
        <<"creator_id">>,
        <<"modifier_id">>,

        <<"created">>,
        <<"modified">>,

        <<"publication_start">>,
        <<"publication_end">>,

        <<"is_published">>,
        <<"is_featured">>,
        <<"is_protected">>,

        <<"chapeau">>,
        <<"subtitle">>,
        <<"short_title">>,
        <<"summary">>,

        <<"name_prefix">>,
        <<"name_first">>,
        <<"name_surname_prefix">>,
        <<"name_surname">>,

        <<"phone">>,
        <<"phone_mobile">>,
        <<"phone_alt">>,
        <<"phone_emergency">>,

        <<"email">>,
        <<"website">>,

        <<"date_start">>,
        <<"date_end">>,
        <<"date_remarks">>,

        <<"address_street_1">>,
        <<"address_street_2">>,
        <<"address_city">>,
        <<"address_state">>,
        <<"address_postcode">>,
        <<"address_country">>,

        <<"mail_email">>,
        <<"mail_street_1">>,
        <<"mail_street_2">>,
        <<"mail_city">>,
        <<"mail_state">>,
        <<"mail_postcode">>,
        <<"mail_country">>,

        <<"billing_email">>,
        <<"billing_street_1">>,
        <<"billing_street_2">>,
        <<"billing_city">>,
        <<"billing_state">>,
        <<"billing_postcode">>,
        <<"billing_country">>,

        <<"location_lng">>,
        <<"location_lat">>,

        <<"body">>,
        <<"body_extra">>,
        <<"blocks">>,

        <<"page_path">>,
        <<"name">>,

        <<"seo_noindex">>,
        <<"title_slug">>,
        <<"custom_slug">>,
        <<"seo_desc">>
    ].

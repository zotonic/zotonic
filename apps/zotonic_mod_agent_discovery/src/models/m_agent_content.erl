%% Copyright 2026 Marc Worrell
%% SPDX-License-Identifier: Apache-2.0
-module(m_agent_content).
-behaviour(zotonic_model).
-moduledoc(#{zotonic_keywords => ["reference", "backend_developer", "model", "http"]}).
-moduledoc("Bounded anonymous reads: search, resource/+id, related/+id and schema.
Requests accept binary-keyed maps. Public listings exclude noindex resources.
Direct reads retain public noindex content. No totals or private properties are returned.

Search accepts text, cat, language, page, pagelen, sort (id/newest/oldest/modified),
and inclusive UTC publication_after/publication_before dates (YYYY-MM-DD).
Related reads accept direction (in/out) and an optional public predicate ID/name.
Schema reads accept kind (category/predicate) and describe public category parents
and bounded predicate category constraints. They are vocabulary, not ACL rules.
All reads default to the site language, use a five-second budget, and cap pages
at 100 candidates. Follow next_url even after an empty filtered page.").
-export([m_get/3, read/4, validate/3]).
-include_lib("zotonic_core/include/zotonic.hrl").

-spec m_get(list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([<<"resource">>, Id | Rest], Msg, C) -> model(resource, Id, Rest, Msg, C);
m_get([<<"related">>, Id | Rest], Msg, C) -> model(related, Id, Rest, Msg, C);
m_get([<<"search">> | Rest], Msg, C) -> model(search, undefined, Rest, Msg, C);
m_get([<<"schema">> | Rest], Msg, C) -> model(schema, undefined, Rest, Msg, C);
m_get(_, _, _) -> {error, unknown_path}.

model(Op, Id, Rest, Msg, C) ->
    Args = case Msg of #{payload := P} when is_map(P) -> P; _ -> #{} end,
    case read(Op, Id, Args, C) of
        {ok, Value} -> {ok, {Value, Rest}};
        Error -> Error
    end.

%% A fresh process avoids process-local ACL memoization from a signed-in caller.
%% Killing this worker also releases/cancels its database connection on timeout.
-spec read(atom(), term(), map(), z:context()) -> {ok, map()} | {error, atom()}.
read(Op, Id, Args, Context) ->
    C = z_context:prune_for_async(z_acl:anondo(Context)),
    case validate(Op, Args, C) of
        {error, _} = Error -> Error;
        {ok, Request} ->
            {Pid, Ref} = spawn_monitor(fun() ->
                Result = try execute(Op, Id, Request, z_context:set_language(maps:get(language, Request), C))
                    catch
                        throw:too_large -> {error, too_large};
                        throw:unavailable -> {error, unavailable}
                    end,
                exit({agent_result, Result})
            end),
            receive
                {'DOWN', Ref, process, Pid, {agent_result, Result}} -> Result;
                {'DOWN', Ref, process, Pid, _} -> {error, unavailable}
            after 5000 ->
                exit(Pid, kill),
                receive {'DOWN', Ref, process, Pid, _} -> ok end,
                {error, timeout}
            end
    end.

-spec validate(atom(), map(), z:context()) -> {ok, map()} | {error, bad_request}.
validate(Op, Args, C) when is_map(Args) ->
    try
        Extra = case Op of
            search -> [<<"text">>, <<"cat">>, <<"sort">>, <<"publication_after">>, <<"publication_before">>];
            schema -> [<<"kind">>];
            related -> [<<"direction">>, <<"predicate">>];
            resource -> []
        end,
        Allowed = case Op of resource -> [<<"language">>]; _ -> [<<"language">>, <<"page">>, <<"pagelen">> | Extra] end,
        true = lists:all(fun(K) -> lists:member(K, Allowed) end, maps:keys(Args)),
        Page = integer(maps:get(<<"page">>, Args, 1)),
        Size = integer(maps:get(<<"pagelen">>, Args, 20)),
        true = Page > 0 andalso Size > 0 andalso Size =< 100 andalso Page * Size =< 10000,
        Default = z_language:default_language(C),
        {ok, Language} = z_language:to_language_atom(maps:get(<<"language">>, Args, Default)),
        true = lists:member(Language, z_language:enabled_languages(C)),
        Text = maps:get(<<"text">>, Args, <<>>),
        true = is_binary(Text) andalso byte_size(Text) =< 512
            andalso is_list(unicode:characters_to_list(Text)),
        Cat = maps:get(<<"cat">>, Args, undefined),
        true = Cat =:= undefined orelse (is_binary(Cat) andalso byte_size(Cat) =< 128),
        Kind = maps:get(<<"kind">>, Args, <<"category">>),
        true = lists:member(Kind, [<<"category">>, <<"predicate">>]),
        Direction = maps:get(<<"direction">>, Args, <<"out">>),
        true = lists:member(Direction, [<<"in">>, <<"out">>]),
        Sort = maps:get(<<"sort">>, Args, <<"id">>),
        true = lists:member(Sort, [<<"id">>, <<"newest">>, <<"oldest">>, <<"modified">>]),
        After = date(maps:get(<<"publication_after">>, Args, undefined)),
        Before = date(maps:get(<<"publication_before">>, Args, undefined)),
        true = After =:= undefined orelse Before =:= undefined orelse After =< Before,
        Predicate = maps:get(<<"predicate">>, Args, undefined),
        true = Predicate =:= undefined orelse (is_binary(Predicate) andalso byte_size(Predicate) =< 128),
        {ok, #{sort => Sort, publication_after => After, publication_before => Before,
            predicate => Predicate, page => Page, pagelen => Size, language => Language,
            text => Text, cat => Cat, kind => Kind, direction => Direction}}
    catch _:_ -> {error, bad_request} end;
validate(_, _, _) -> {error, bad_request}.

integer(I) when is_integer(I) -> I;
integer(B) when is_binary(B), byte_size(B) =< 5 -> binary_to_integer(B).

date(undefined) -> undefined;
date(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>> = Date) ->
    match = re:run(Date, <<"^[0-9]{4}-[0-9]{2}-[0-9]{2}$">>, [{capture, none}]),
    true = binary_to_integer(Y) > 0,
    true = calendar:valid_date({binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}),
    Date.

datetime(<<Y:4/binary, "-", M:2/binary, "-", D:2/binary>>, Time) ->
    {{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}, Time}.

search_args(R) ->
    Order = case maps:get(sort, R) of
        <<"id">> -> [<<"id">>];
        <<"newest">> -> [<<"-publication_start">>, <<"id">>];
        <<"oldest">> -> [<<"publication_start">>, <<"id">>];
        <<"modified">> -> [<<"-modified">>, <<"id">>]
    end,
    Dates = [{<<"publication_after">>, maps:get(publication_after, R), {0,0,0}},
        {<<"publication_before">>, maps:get(publication_before, R), {23,59,59}}],
    (maps:from_list([{Key, datetime(Value, Time)} || {Key, Value, Time} <- Dates, Value =/= undefined]))#{
        <<"page">> => maps:get(page, R), <<"pagelen">> => maps:get(pagelen, R),
        %% Explicit API ordering (including its unique ID tie-breaker) must precede
        %% full-text relevance, which the query builder adds to ordinary sort.
        <<"text">> => maps:get(text, R), <<"asort">> => Order}.

execute(resource, Ref, R, C) ->
    case visible(Ref, C) of
        undefined -> {error, not_found};
        Id -> {ok, resource(Id, R, C)}
    end;
execute(Op, _, R, C) when Op =:= search; Op =:= schema ->
    Cat = case Op of schema -> maps:get(kind, R); search -> maps:get(cat, R) end,
    Args = search_args(R),
    SearchArgs = case Cat of undefined -> Args; _ -> Args#{<<"cat">> => Cat} end,
    case m_search:search(<<"query">>, SearchArgs#{<<"options">> => #{is_single_page => true}}, C) of
        {ok, #search_result{result = Ids, next = Next}} ->
            Items = [listing(Op, Id, R, C) || Id <- Ids, listed(Id, C)],
            {ok, continuation(Op, undefined, R, page(Items, Next, R), C)};
        {error, _} -> {error, unavailable}
    end;
execute(related, Ref, R, C) ->
    case visible(Ref, C) of
        undefined -> {error, not_found};
        Id ->
            case maps:get(predicate, R) of
                undefined -> related(Id, undefined, R, C);
                Predicate ->
                    case visible(Predicate, C) of
                        undefined -> {error, not_found};
                        P ->
                            case listed(P, C) andalso m_rsc:is_a(P, predicate, C) of
                                true -> related(Id, P, R, C);
                                false -> {error, not_found}
                            end
                    end
            end
    end.

visible(Ref, C) when is_integer(Ref); is_binary(Ref) ->
    case m_rsc:rid(Ref, C) of
        undefined -> undefined;
        Id ->
            case z_acl:rsc_visible(Id, C) andalso m_rsc:is_published_date(Id, C) of
                true -> Id;
                false -> undefined
            end
    end;
visible(_, _) -> undefined.

listed(Id, C) ->
    visible(Id, C) =/= undefined
        andalso not m_config:get_boolean(seo, noindex, C)
        andalso not z_convert:to_bool(m_rsc:p(Id, <<"seo_noindex">>, C))
        andalso not z_convert:to_bool(m_rsc:p(m_rsc:p(Id, <<"category_id">>, C), <<"is_seo_noindex_cat">>, C))
        andalso m_rsc:p(Id, <<"name">>, C) =/= <<"hasusergroup">>.

summary(Id, R, C) ->
    Props = m_rsc:get(Id, C),
    Requested = maps:get(language, R),
    Available = case maps:get(<<"language">>, Props, []) of
        [] -> [z_language:default_language(C)];
        Ls -> Ls
    end,
    Actual = case lists:member(Requested, Available) of
        true -> Requested;
        false ->
            Default = z_language:default_language(C),
            case lists:member(Default, Available) of true -> Default; false -> hd(Available) end
    end,
    LC = z_context:set_language(Actual, C),
    #{id => Id, uri => m_rsc:uri(Id, C), page_url => m_rsc:p(Id, <<"page_url_abs">>, LC),
      title => plain(maps:get(<<"title">>, Props, <<>>), Actual, C),
      summary => plain(maps:get(<<"summary">>, Props, <<>>), Actual, C),
      name => maps:get(<<"name">>, Props, undefined),
      modified => modified(maps:get(<<"modified">>, Props, undefined), C),
      requested_language => Requested, language => Actual, available_languages => Available,
      is_fallback => Requested =/= Actual,
      resource_url => url(agent_resource, Id, LC), markdown_url => url(agent_markdown, Id, LC),
      related_url => url(agent_related, Id, LC)}.

resource(Id, R, C) ->
    S = summary(Id, R, C),
    Lang = maps:get(language, S),
    Props = m_rsc:get(Id, C),
    Content = agent_content_markdown:compose(Id, Props, Lang, C),
    (maps:merge(S, Content))#{
        license => plain(maps:get(<<"license">>, Props, <<>>), Lang, C),
        attribution => plain(maps:get(<<"attribution">>, Props, <<>>), Lang, C),
        media => medium(Id, C), full_export_url => url(api_rsc_export, Id, C)}.

plain(Value, Lang, C) ->
    Text = translated(Value, Lang, C),
    case byte_size(Text) =< 16384 of
        true -> z_html:unescape(z_html:strip(Text));
        false -> throw(too_large)
    end.
%% Plain resource text belongs to the resource language selected above. z_trans
%% treats binaries as belonging to the context language, which differs on fallback.
translated(Value, _Lang, _C) when is_binary(Value) -> Value;
translated(Value, _Lang, _C) when is_list(Value) -> unicode:characters_to_binary(Value);
translated(Value, Lang, C) -> z_convert:to_binary(z_trans:lookup(Value, Lang, C)).

related(Id, Predicate, R, C) ->
    Direction = case maps:get(direction, R) of
        <<"out">> -> out;
        <<"in">> -> in
    end,
    Size = maps:get(pagelen, R),
    Page = maps:get(page, R),
    case m_edge:get_page(Direction, Id, Predicate, (Page - 1) * Size, Size, C) of
        {ok, #{items := Rows, has_more := HasMore}} ->
            Items = [#{predicate => summary(P, R, C), resource => summary(E, R, C)}
                || #{endpoint_id := E, predicate_id := P} <- Rows,
                    listed(P, C), listed(E, C)],
            Next = case HasMore of true -> Page + 1; false -> false end,
            {ok, continuation(related, Id, R,
                (page(Items, Next, R))#{id => Id, direction => maps:get(direction, R)}, C)};
        {error, _} -> {error, unavailable}
    end.

page(Items, Next, R) ->
    #{items => Items, page => maps:get(page, R), pagelen => maps:get(pagelen, R),
      next => Next, pagination => <<"candidate_pages">>}.

url(Dispatch, Id, C) ->
    z_context:abs_url(z_dispatcher:url_for(Dispatch, [{id, Id}, {language, z_context:language(C)}], none, C), C).

%% Continuation preserves the normalized request and honors site dispatch overrides.
continuation(_, _, _, #{next := false} = Result, _) -> Result#{next_url => undefined};
continuation(Op, Id, R, #{next := Next} = Result, C) ->
    Size = maps:get(pagelen, R),
    case Next * Size > 10000 of
        true -> Result#{next => false, next_url => undefined, limit_reached => true};
        false ->
            {Dispatch, Extra} = case Op of
                search -> {agent_search, [{K, maps:get(K, R)} || K <-
                    [text, cat, sort, publication_after, publication_before]]};
                schema -> {agent_schema, [{kind, maps:get(kind, R)}]};
                related -> {agent_related, [{id, Id}, {direction, maps:get(direction, R)}, {predicate, maps:get(predicate, R)}]}
            end,
            Args = [{page, Next}, {pagelen, Size}, {language, maps:get(language, R)} | Extra],
            Url = z_context:abs_url(z_dispatcher:url_for(Dispatch,
                [{K, V} || {K, V} <- Args, V =/= undefined], none, C), C),
            Result#{next_url => Url}
    end.

%% Read the medium only after the owning resource passed anonymous ACL checks.
%% Never export storage paths, upload metadata or arbitrary medium properties.
medium(Id, C) ->
    case m_media:get(Id, C) of
        #{<<"size">> := Size} = M when Size > 0 ->
            (maps:with([<<"mime">>, <<"width">>, <<"height">>, <<"size">>], M))#{
                download_url => url(media_attachment, Id, C)};
        _ -> undefined
    end.

listing(search, Id, R, C) -> summary(Id, R, C);
listing(schema, Id, #{kind := <<"category">>} = R, C) ->
    Parent = proplists:get_value(parent_id, m_category:get(Id, C)),
    (summary(Id, R, C))#{kind => category, parent => public_summary(Parent, R, C)};
listing(schema, Id, R, C) ->
    %% Constraints are a bounded public description, never an authorization rule.
    {ok, Rows} = z_db:qmap("select category_id, is_subject from predicate_category
        where predicate_id = $1 order by is_subject, category_id limit 101",
        [Id], [{keys, atom}, {timeout, 3000}], C),
    Visible = [Row || #{category_id := Cat} = Row <- lists:sublist(Rows, 100), listed(Cat, C)],
    (summary(Id, R, C))#{kind => predicate,
        subject_categories => [summary(Cat, R, C) || #{category_id := Cat, is_subject := true} <- Visible],
        object_categories => [summary(Cat, R, C) || #{category_id := Cat, is_subject := false} <- Visible],
        constraints_scope => <<"public categories only; not validation rules">>,
        constraints_truncated => length(Rows) > 100}.

public_summary(undefined, _, _) -> undefined;
public_summary(Id, R, C) ->
    case listed(Id, C) of true -> summary(Id, R, C); false -> undefined end.

modified(undefined, _) -> undefined;
modified(Date, C) -> z_convert:to_binary(z_datetime:format_utc(Date, "c", C)).

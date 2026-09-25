%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell <marc@worrell.nl>
%% @doc Durable mailing runs, recipient status and delivery statistics.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(m_mailinglist_run).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "model", "mailing_lists",
        "schedule", "send_and_receive", "monitor"
    ]
}).
-moduledoc("
Durable mailing runs and delivery accounting, available as `m.mailinglist_run` in templates.
Recipient addresses and message diagnostics are never published on MQTT.
Internal worker functions require an already authorized sender context.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/` | List accessible runs, optionally filtered by a payload map. |
| `get` | `/run/+run_id/...` | Return a run with aggregate statistics, language results and saved-content metadata. |
| `get` | `/recipients/+run_id/...` | Return up to 100 recipient results, optionally filtered and paged using the payload. |
| `get` | `/page/+page_id/...` | List accessible runs for a page. Resource names and ids are accepted. |
| `get` | `/list/+list_id/...` | List accessible runs for a mailinglist. Resource names and ids are accepted. |
| `get` | `/history_expired/+page_id/+list_id/...` | Return whether any run for this page/list pair has expired recipient details. Both ids must be numeric. |
| `get` | `/recent/...` | Return up to five accessible recent runs, newest first, from the latest 200 runs. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments
are returned for further lookups. Run ids accept integers or numeric binaries.
Unrecognized paths return `{error, unknown_path}`.

Run lists and filters
---------------------

List results contain run metadata, including ids, language settings, status, timestamps,
test/resend information, errors and `details_expired`, plus a `stats` map.
Except for `/recent`, runs in `preparing`, `sending`, `retrying` or `interrupted` status
come first; within each group the newest creation time and id come first.
Each query selects at most 200 rows before checking access, so fewer rows may be returned.

The root path accepts a payload map with binary keys:

| Key | Default | Meaning |
| --- | --- | --- |
| `status` | Empty binary | Exact run status; empty means all statuses. |
| `language` | Empty binary | Exact run language setting; empty means all languages. |
| `page_id` | `0` | Numeric page id; zero means all pages. |
| `list_id` | `0` | Numeric mailinglist id; zero means all lists. |
| `offset` | `0` | Rows to skip before access checks, capped at 1,000,000. |

For example, `m.mailinglist_run::%{status: \"scheduled\", list_id: id}` lists scheduled
runs for a mailinglist. `m.mailinglist_run.page[id]` lists runs for a page.
Filters apply only to the root path; `/page`, `/list` and `/recent` ignore the payload.

Run details and statistics
--------------------------

`m.mailinglist_run.run[run_id]` returns the run without its internal `pickled_context`,
`options`, `props` or `request_key`. It adds:

* `stats`: recipient counts per status, plus `total`, `selected`, `waiting`,
  `unsuccessful`, `processed` and integer `percent`.
* `languages`: nonzero counts as maps containing `language`, `status` and `total`,
  ordered by language and status.
* `copies`: saved-content metadata containing `language` and `created`, ordered by language.
* `first_submitted`: the first email submission timestamp, if available.
* `test_address`: the single test recipient address, if this run has one.

`selected` excludes skipped recipients; `waiting` sums pending, submitting, queued and
retrying recipients. `unsuccessful` sums failed and bounced recipients. `processed` is
selected minus waiting; `percent` is the integer percentage processed, or zero if none
were selected. Individual status keys with no recorded count may be absent.
Sent counts mean acceptance by the receiving mail server, not confirmed inbox delivery.
Saved HTML is read through `content/3` and the saved-content controller, not an `m_get` path.

Recipient results
-----------------

The `/recipients/+run_id` payload accepts `status` and `after` with binary keys.
`status` can be `pending`, `submitting`, `queued`, `retrying`, `sent`, `failed`,
`bounced`, `skipped` or `cancelled`. An omitted, empty or unrecognized status means all.
`after` is a recipient-row id, defaulting to zero. Results are ordered by id ascending;
use the last returned id as `after` to fetch the next batch of up to 100 rows.

Each result contains `id`, `email`, `language`, `status`, `reason` and `modified`.
For example: `m.mailinglist_run.recipients[run_id]::%{status: \"failed\", after: last_id}`.
Expired recipient details return an empty list; aggregate statistics remain available.

Access and expired history
--------------------------

Run reads require permission to use `mod_mailinglist`, visibility of the mailed page,
and permission to edit the mailinglist. An authenticated sender may also read their own
test-list runs without edit permission on the test list. List paths omit inaccessible
runs; `/run` and `/recipients` return `{error, eacces}` for missing or inaccessible runs.

`/history_expired/+page_id/+list_id` instead checks
`mod_mailinglist:is_allowed_to_send(ListId, PageId, Context)` and returns
`{error, eacces}` if sending is not allowed. A true result means that recipient history
for this pair is incomplete, so a new mailing cannot reliably select only new or failed
recipients. It does not mean that every run for the pair has expired.

Successful `m_get/3` calls return `{ok, {Value, Rest}}`; templates receive `Value`
and use any remaining path segments for further lookups.
").

-behaviour(zotonic_model).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    m_get/3,
    content/3,
    create/6,
    import_scheduled/2,
    get/2,
    list/2,
    allowed/2,
    next_due/1,
    claim/1,
    release/2,
    fail/3,
    recover/1,
    cancel/2,
    resume/2,
    add_recipient/7,
    transition/4,
    message/6,
    prepared/2,
    refresh/2,
    periodic_cleanup/1,
    check_history/4,
    publish/2,
    stats/2,
    previous/5,
    language/4,
    next_state/2,
    status/3,
    resend/3,
    fallback/2,
    rebuild_stats/2
]).

-spec m_get(list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([<<"run">>, Id | Rest], _Msg, Context) ->
    case get(to_id(Id), Context) of
        {ok, Run} ->
            case allowed(Run, Context) of
                true ->
                    {ok, ByLanguage} = z_db:qmap(
                        "select language,status,total
                         from mailinglist_run_stats
                         where run_id = $1
                           and total > 0
                         order by language,status",
                        [to_id(Id)],
                        Context
                    ),
                    {ok, Copies} = z_db:qmap(
                        "select language,created
                         from mailinglist_run_content
                         where run_id=$1
                         order by language",
                        [to_id(Id)],
                        Context
                    ),
                    Public = maps:without(
                        [<<"pickled_context">>, <<"options">>, <<"props">>, <<"request_key">>], Run
                    ),
                    FirstSubmitted = z_db:q1(
                        "select coalesce((select first_submitted from mailinglist_run where id=$1), min(msg.created))
                         from mailinglist_run_message msg
                            join mailinglist_run_recipient rr on rr.id=msg.recipient_id
                         where rr.run_id=$1",
                        [to_id(Id)],
                        Context
                    ),
                    {ok, {
                        Public#{
                            <<"copies">> => Copies,
                            <<"first_submitted">> => FirstSubmitted,
                            <<"test_address">> => proplists:get_value(
                                single_test_address, maps:get(<<"options">>, Run, [])
                            ),
                            <<"stats">> => stats(to_id(Id), Context),
                            <<"languages">> => ByLanguage
                        },
                        Rest
                    }};
                false ->
                    {error, eacces}
            end;
        _ ->
            {error, eacces}
    end;
m_get([<<"recipients">>, Id | Rest], Msg, Context) ->
    case get(to_id(Id), Context) of
        {ok, Run} ->
            case allowed(Run, Context) of
                true ->
                    Payload =
                        case Msg of
                            #{payload := P} when is_map(P) -> P;
                            _ -> #{}
                        end,
                    State = maps:get(<<"status">>, Payload, <<>>),
                    After = max(0, to_id(maps:get(<<"after">>, Payload, 0))),
                    {ok, Rows} = z_db:qmap(
                        "select id, email, language, status, reason, modified
                         from mailinglist_run_recipient
                         where run_id=$1
                           and id > $2
                           and exists(select 1 from mailinglist_run where id=$1 and details_expired is null)
                           and ($3 = '' or status=$3)
                         order by id limit 100",
                        [to_id(Id), After, valid_status(State)],
                        Context
                    ),
                    {ok, {Rows, Rest}};
                false ->
                    {error, eacces}
            end;
        _ ->
            {error, eacces}
    end;
m_get([<<"page">>, Id | Rest], _Msg, Context) ->
    {ok, {list({page, m_rsc:rid(Id, Context)}, Context), Rest}};
m_get([<<"list">>, Id | Rest], _Msg, Context) ->
    {ok, {list({list, m_rsc:rid(Id, Context)}, Context), Rest}};
m_get([<<"history_expired">>, Page, List | Rest], _Msg, Context) ->
    case mod_mailinglist:is_allowed_to_send(to_id(List), to_id(Page), Context) of
        true -> {ok, {history_expired(to_id(List), to_id(Page), Context), Rest}};
        false -> {error, eacces}
    end;
m_get([<<"recent">> | Rest], _Msg, Context) ->
    {ok, {list(recent, Context), Rest}};
m_get([], #{payload := Filter}, Context) when is_map(Filter) ->
    {ok, {list({filter, Filter}, Context), []}};
m_get([], _Msg, Context) ->
    {ok, {list(all, Context), []}};
m_get(_, _, _) ->
    {error, unknown_path}.

to_id(I) when is_integer(I), I >= 0 -> I;
to_id(B) when is_binary(B), byte_size(B) < 20 ->
    try
        max(0, binary_to_integer(B))
    catch
        _:_ -> 0
    end;
to_id(_) ->
    0.

valid_status(S) ->
    case
        lists:member(S, [
            <<"pending">>,
            <<"submitting">>,
            <<"queued">>,
            <<"retrying">>,
            <<"sent">>,
            <<"failed">>,
            <<"bounced">>,
            <<"skipped">>,
            <<"cancelled">>
        ])
    of
        true -> S;
        false -> <<>>
    end.

-spec get(Id, Context) -> {ok, map()} | {error, term()} when
    Id :: integer(), Context :: z:context().
get(Id, Context) ->
    case z_db:qmap_props_row("select * from mailinglist_run where id=$1", [Id], Context) of
        {ok, Run} ->
            Options =
                case maps:get(<<"options">>, Run, []) of
                    L when is_list(L) -> L;
                    _ -> []
                end,
            {ok, Run#{<<"options">> => Options}};
        Error ->
            Error
    end.

%% Saved HTML has the same access boundary as the run and its recipient results.
-spec content(Id, Language, Context) -> {ok, map()} | {error, term()} when
    Id :: integer() | binary(), Language :: binary(), Context :: z:context().
content(Id, Language, Context) ->
    case get(to_id(Id), Context) of
        {ok, Run} ->
            case allowed(Run, Context) of
                true ->
                    z_db:qmap_row(
                        "select html,language,created
                         from mailinglist_run_content
                         where run_id=$1
                           and language=$2",
                        [to_id(Id), Language],
                        Context
                    );
                false ->
                    {error, eacces}
            end;
        _ ->
            {error, eacces}
    end.

-spec allowed(Run, Context) -> boolean() when Run :: map(), Context :: z:context().
allowed(#{<<"page_id">> := Page, <<"mailinglist_id">> := List} = Run, Context) ->
    z_acl:is_allowed(use, mod_mailinglist, Context) andalso
        z_acl:rsc_visible(Page, Context) andalso
        (z_acl:rsc_editable(List, Context) orelse
            (List =:= m_rsc:rid(mailinglist_test, Context) andalso
                is_integer(z_acl:user(Context)) andalso
                maps:get(<<"sender_id">>, Run, undefined) =:= z_acl:user(Context)));
allowed(_, _) ->
    false.

-spec list(Filter, Context) -> [map()] when
    Filter :: all | recent | tuple(), Context :: z:context().
list(Filter, Context) ->
    case z_acl:is_allowed(use, mod_mailinglist, Context) of
        false ->
            [];
        true ->
            {Where, Args} =
                case Filter of
                    {page, Id} ->
                        {"where page_id=$1", [Id]};
                    {list, Id} ->
                        {"where mailinglist_id=$1", [Id]};
                    {filter, F} ->
                        State = maps:get(<<"status">>, F, <<>>),
                        Lang = maps:get(<<"language">>, F, <<>>),
                        {"where ($1='' or status=$1) and ($2='' or language=$2) and ($3=0 or page_id=$3) and ($4=0 or mailinglist_id=$4)",
                            [
                                filter_text(State),
                                filter_text(Lang),
                                to_id(maps:get(<<"page_id">>, F, 0)),
                                to_id(maps:get(<<"list_id">>, F, 0))
                            ]};
                    recent ->
                        {"", []};
                    all ->
                        {"", []}
                end,
            Offset =
                case Filter of
                    {filter, Fs} -> min(1000000, to_id(maps:get(<<"offset">>, Fs, 0)));
                    _ -> 0
                end,
            Order =
                case Filter of
                    recent ->
                        "created desc, id desc";
                    _ ->
                        "(status in ('preparing','sending','retrying','interrupted')) desc, created desc, id desc"
                end,
            {ok, Rows} = z_db:qmap(
                "select id, page_id, mailinglist_id, sender_id, language,
                        fallback_language, status, due, type, created, started, finished, modified,
                        error, is_test, parent_id, details_expired
                 from mailinglist_run " ++
                    Where ++
                    " order by " ++ Order ++ " limit 200 offset " ++ integer_to_list(Offset),
                Args,
                Context
            ),
            Allowed = [R || R <- Rows, allowed(R, Context)],
            %% Apply the dashboard limit after access checks, before loading stats.
            Visible =
                case Filter of
                    recent -> lists:sublist(Allowed, 5);
                    _ -> Allowed
                end,
            Ids = [maps:get(<<"id">>, R) || R <- Visible],
            Summaries = list_stats(Ids, Context),
            [
                R#{<<"stats">> => totals(maps:get(maps:get(<<"id">>, R), Summaries, #{}))}
             || R <- Visible
            ]
    end.

list_stats([], _) ->
    #{};
list_stats(Ids, Context) ->
    {ok, Rows} = z_db:qmap(
        "select run_id, status, sum(total)::int as total
         from mailinglist_run_stats
         where run_id=any($1::bigint[])
         group by run_id, status",
        [Ids],
        Context
    ),
    lists:foldl(
        fun(#{<<"run_id">> := Id, <<"status">> := S, <<"total">> := N}, Acc) ->
            Counts = maps:get(Id, Acc, #{}),
            Acc#{Id => Counts#{S => N}}
        end,
        #{},
        Rows
    ).

filter_text(B) when is_binary(B), byte_size(B) < 40 -> B;
filter_text(_) -> <<>>.

-spec create(List, Page, Type, Due, Options, Context) -> {ok, integer()} | {error, term()} when
    List :: integer(),
    Page :: integer(),
    Type :: binary(),
    Due :: calendar:datetime(),
    Options :: list(),
    Context :: z:context().
create(List, Page, Type, Due, Options, Context) ->
    case mod_mailinglist:is_allowed_to_send(List, Page, Context) of
        false ->
            {error, eacces};
        true ->
            case check_history(List, Page, Options, Context) of
                ok -> create_run(List, Page, Type, Due, Options, Context);
                Error -> Error
            end
    end.

create_run(List, Page, Type, Due, Options, Context) ->
    case mod_mailinglist:is_allowed_to_send(List, Page, Context) of
        false ->
            {error, eacces};
        true ->
            Lang = proplists:get_value(language, Options, <<>>),
            Fallback = proplists:get_value(fallback_language, Options, fallback(Page, Context)),
            Mode = proplists:get_value(
                send_mode,
                Options,
                case proplists:get_bool(is_send_all, Options) of
                    true -> <<"all">>;
                    false -> <<"new">>
                end
            ),
            Audience = proplists:get_value(audience, Options, <<"matching">>),
            case
                valid_options(Lang, Fallback, Mode, Audience, Page, Context) andalso
                    lists:member(Type, [<<"date">>, <<"publication">>])
            of
                false ->
                    {error, invalid_options};
                true ->
                    Id = z_db:q1(
                        "insert into mailinglist_run
                            (page_id, mailinglist_id, sender_id, parent_id, language, fallback_language,
                             audience, send_mode, is_test, type, due, props, request_key)
                         values
                            ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13)
                         on conflict (request_key)
                            do update set request_key=excluded.request_key
                         returning id",
                        [
                            Page,
                            List,
                            z_acl:user(Context),
                            proplists:get_value(parent_id, Options),
                            z_convert:to_binary(Lang),
                            z_convert:to_binary(Fallback),
                            Audience,
                            Mode,
                            List =:= m_rsc:rid(mailinglist_test, Context),
                            Type,
                            Due,
                            ?DB_PROPS([
                                {options, Options}, {pickled_context, z_context:pickle(Context)}
                            ]),
                            proplists:get_value(request_key, Options, z_ids:id(32))
                        ],
                        Context
                    ),
                    publish(Id, Context),
                    {ok, Id}
            end
    end.

-spec fallback(Page, Context) -> atom() when Page :: integer(), Context :: z:context().
fallback(Page, Context) ->
    Languages = available_languages(Page, Context),
    Current = z_context:language(Context),
    case lists:member(Current, Languages) of
        true -> Current;
        false -> hd(Languages)
    end.

valid_options(Lang, Fallback, Mode, Audience, Page, Context) ->
    lists:member(Mode, [<<"new">>, <<"all">>, <<"failed">>]) andalso
        lists:member(Audience, [<<"matching">>, <<"all">>]) andalso
        is_language(Fallback) andalso
        lists:member(z_convert:to_binary(Fallback), [
            z_convert:to_binary(L)
         || L <- available_languages(Page, Context)
        ]) andalso
        (Lang =:= <<>> orelse
            (is_language(Lang) andalso
                lists:member(z_convert:to_binary(Lang), [
                    z_convert:to_binary(L)
                 || L <- available_languages(Page, Context)
                ]))).

available_languages(Page, Context) ->
    case m_rsc:p(Page, language, Context) of
        L when is_list(L), L =/= [] -> L;
        _ -> [z_context:language(Context)]
    end.

is_language(L) when is_atom(L); is_binary(L) ->
    case z_language:to_language_atom(L) of
        {ok, _} -> true;
        _ -> false
    end;
is_language(_) ->
    false.

%% Old schedules have no reliable run/language history. Preserve their options
%% and sender context; authorization is checked again when they are claimed.
-spec import_scheduled(Row, Context) -> ok when Row :: list(), Context :: z:context().
import_scheduled(Row, Context) ->
    Pickled = proplists:get_value(pickled_context, Row),
    Options = proplists:get_value(options, Row, []),
    {Sender, Lang} =
        try z_context:depickle(Pickled) of
            C -> {z_acl:user(C), z_context:language(C)}
        catch
            _:_ -> {undefined, z_context:language(Context)}
        end,
    Mode =
        case proplists:get_bool(is_send_all, Options) of
            true -> <<"all">>;
            false -> <<"new">>
        end,
    z_db:q(
        "insert into mailinglist_run
            (page_id, mailinglist_id, type, due, props,
             sender_id,fallback_language,send_mode,is_test)
         values ($1,$2,$3,$4,$5,$6,$7,$8,$9)",
        [
            proplists:get_value(page_id, Row),
            proplists:get_value(mailinglist_id, Row),
            proplists:get_value(type, Row),
            proplists:get_value(due, Row),
            ?DB_PROPS([{options, Options}, {pickled_context, Pickled}]),
            Sender,
            z_convert:to_binary(Lang),
            Mode,
            proplists:get_value(mailinglist_id, Row) =:= m_rsc:rid(mailinglist_test, Context)
        ],
        Context
    ),
    ok.

-spec next_due(Context) -> calendar:datetime() | undefined when Context :: z:context().
next_due(Context) ->
    z_db:q1(
        "select min(case when m.type='publication' then greatest(m.due,r.publication_start) else m.due end)
         from mailinglist_run m
            join rsc r on r.id=m.page_id
         where m.status='scheduled'
           and (m.type='date'
                or (r.is_published and r.publication_end >= greatest(now(),m.due,r.publication_start)))",
        Context
    ).

%% Claim atomically before starting a worker, preventing concurrent queue polls
%% from launching the same run twice.
-spec claim(Context) -> {ok, map()} | {error, term()} when Context :: z:context().
claim(Context) ->
    z_db:qmap_props_row(
        "update mailinglist_run
         set status = 'preparing',
             started = coalesce(started,now()),
             modified = now()
         where id = (
            select m.id
            from mailinglist_run m
                join rsc r on r.id = m.page_id
            where m.status = 'scheduled'
                and m.due <= now()
                and (m.type = 'date'
                     or (r.is_published
                         and r.publication_start<=now()
                         and r.publication_end>=now()))
            order by m.due, m.id
            for update of m
            skip locked
            limit 1)
         returning *",
        Context
    ).

-spec release(Id, Context) -> ok when Id :: integer(), Context :: z:context().
release(Id, Context) ->
    z_db:q(
        "update mailinglist_run set status='scheduled' where id=$1 and status='preparing'",
        [Id],
        Context
    ),
    ok.

-spec fail(Id, Reason, Context) -> ok when
    Id :: integer(), Reason :: term(), Context :: z:context().
fail(Id, Reason, Context) ->
    z_db:q(
        "update mailinglist_run set
            status = 'failed',
            error = $2,
            modified = now(),
            finished = now()
        where id = $1
          and details_expired is null
          and status <> 'cancelled'",
        [Id, detail(Reason)],
        Context
    ),
    publish(Id, Context).

-spec recover(Context) -> ok when Context :: z:context().
recover(Context) ->
    Rows = z_db:q(
        "update mailinglist_run set
            status = 'interrupted',
            error = 'Worker interrupted; review pending recipients before resuming.'
         where status in ('preparing','sending')
           and not prepared
           and modified < now() - interval '10 minutes'
         returning id",
        Context
    ),
    lists:foreach(fun({Id}) -> publish(Id, Context) end, Rows),
    %% An ambiguous handoff is never automatically submitted again.
    Stale = z_db:q(
        "update mailinglist_run set
            status = 'interrupted',
            error = 'Email handoff has not been confirmed. Review before retrying.'
        where status in ('sending','retrying')
          and modified < now() - interval '10 minutes'
          and exists(
                select 1
                from mailinglist_run_recipient rr
                where rr.run_id = mailinglist_run.id
                  and rr.status = 'submitting')
         returning id",
        Context
    ),
    lists:foreach(fun({Id}) -> publish(Id, Context) end, Stale),
    ok.

-spec cancel(Id, Context) -> ok | {error, eacces} when Id :: integer(), Context :: z:context().
cancel(Id, Context) ->
    case get(Id, Context) of
        {ok, R} ->
            case allowed(R, Context) andalso not z_acl:is_read_only(Context) of
                true ->
                    z_db:q(
                        "update mailinglist_run set
                            status = 'cancelled',
                            finished = now(),
                            modified = now()
                        where id=$1
                          and status not in ('completed','completed_errors','empty','failed')
                          and (
                            (not prepared and not exists(select 1 from mailinglist_run_recipient where run_id=$1))
                            or exists(select 1 from mailinglist_run_recipient where run_id=$1 and status='pending'))",
                        [Id],
                        Context
                    ),
                    Pending = z_db:q(
                        "select id from mailinglist_run_recipient where run_id=$1 and status='pending'",
                        [Id],
                        Context
                    ),
                    lists:foreach(
                        fun({Rid}) ->
                            transition(
                                Rid, <<"cancelled">>, <<"Cancelled before submission">>, Context
                            )
                        end,
                        Pending
                    ),
                    publish(Id, Context),
                    ok;
                false ->
                    {error, eacces}
            end;
        _ ->
            {error, eacces}
    end.

-spec resume(Id, Context) -> ok | {error, eacces} when Id :: integer(), Context :: z:context().
resume(Id, Context) ->
    case get(Id, Context) of
        {ok, R} ->
            case
                allowed(R, Context) andalso maps:get(<<"status">>, R) =:= <<"interrupted">> andalso
                    ((not maps:get(<<"prepared">>, R) andalso
                        maps:get(<<"total">>, stats(Id, Context), 0) =:= 0) orelse
                        maps:get(<<"pending">>, stats(Id, Context), 0) > 0) andalso
                    mod_mailinglist:is_allowed_to_send(
                        maps:get(<<"mailinglist_id">>, R), maps:get(<<"page_id">>, R), Context
                    )
            of
                true ->
                    z_db:q(
                        "update mailinglist_run set
                            status = 'scheduled',
                            due = now(),
                            type = 'date',
                            error = null,
                            modified = now(),
                            props = $2
                          where id = $1
                            and status = 'interrupted'",
                        [
                            Id,
                            ?DB_PROPS([
                                {pickled_context, z_context:pickle(Context)},
                                {options, maps:get(<<"options">>, R, [])}
                            ])
                        ],
                        Context
                    ),
                    mod_mailinglist:ensure_scheduled_task(Context),
                    publish(Id, Context),
                    ok;
                false ->
                    {error, eacces}
            end;
        _ ->
            {error, eacces}
    end.

-spec add_recipient(Run, Email, Rsc, Lang, State, Reason, Context) -> ok when
    Run :: integer(),
    Email :: binary(),
    Rsc :: integer() | undefined,
    Lang :: binary(),
    State :: binary(),
    Reason :: binary() | undefined,
    Context :: z:context().
add_recipient(Run, Email, Rsc, Lang, State, Reason, Context) ->
    z_db:transaction(
        fun(Ctx) ->
            case
                z_db:q1(
                    "insert into mailinglist_run_recipient
                        (run_id,email,recipient_id,language,status,reason)
                     values
                        ($1,$2,$3,$4,$5,$6)
                      on conflict (run_id,email)
                        do nothing
                      returning id",
                    [Run, Email, Rsc, Lang, State, Reason],
                    Ctx
                )
            of
                undefined -> ok;
                _ -> counter(Run, Lang, State, 1, Ctx)
            end
        end,
        Context
    ).

counter(Run, Lang, State, Delta, Context) ->
    z_db:q(
        "insert into mailinglist_run_stats
            (run_id,language,status,total)
         values
            ($1,$2,$3,$4)
         on conflict (run_id,language,status)
            do update set total = mailinglist_run_stats.total + $4",
        [Run, Lang, State, Delta],
        Context
    ),
    ok.

-spec transition(Id, State, Reason, Context) -> integer() when
    Id :: integer(), State :: binary(), Reason :: binary() | undefined, Context :: z:context().
transition(Id, State, Reason, Context) ->
    z_db:transaction(
        fun(Ctx) ->
            {ok, R} = z_db:qmap_row(
                "select * from mailinglist_run_recipient where id=$1 for update", [Id], Ctx
            ),
            Old = maps:get(<<"status">>, R),
            New = next_state(Old, State),
            Run = maps:get(<<"run_id">>, R),
            case Old =:= New of
                true ->
                    ok;
                false ->
                    Lang = maps:get(<<"language">>, R),
                    %% Increment first: no negative values are ever inserted into the summary table.
                    counter(Run, Lang, New, 1, Ctx),
                    z_db:q(
                        "update mailinglist_run_stats set total=total-1 where run_id=$1 and language=$2 and status=$3",
                        [Run, Lang, Old],
                        Ctx
                    ),
                    z_db:q(
                        "update mailinglist_run_recipient set status=$2, reason=$3, modified=now() where id=$1",
                        [Id, New, Reason],
                        Ctx
                    )
            end,
            Run
        end,
        Context
    ).

%% Duplicate and delayed notifications must not undo a terminal result.
-spec next_state(binary(), binary()) -> binary().
next_state(<<"bounced">>, _) -> <<"bounced">>;
next_state(<<"cancelled">>, _) -> <<"cancelled">>;
next_state(<<"skipped">>, _) -> <<"skipped">>;
next_state(_, <<"bounced">>) -> <<"bounced">>;
next_state(<<"sent">>, _) -> <<"sent">>;
next_state(<<"failed">>, _) -> <<"failed">>;
next_state(<<"retrying">>, <<"queued">>) -> <<"retrying">>;
next_state(Old, <<"submitting">>) when Old =/= <<"pending">> -> Old;
next_state(_, New) -> New.

-spec message(Id, State, Final, Retry, Detail, Context) -> ok when
    Id :: binary() | undefined,
    State :: binary(),
    Final :: boolean(),
    Retry :: integer() | undefined,
    Detail :: term(),
    Context :: z:context().
message(undefined, _, _, _, _, _) ->
    ok;
message(MsgId, State, Final, Retry, Detail, Context) ->
    %% Lock the run before its recipient, like submission and retention cleanup.
    %% A notification must not restore diagnostics after details have expired.
    Run = z_db:transaction(
        fun(Ctx) ->
            case
                z_db:q(
                    "select r.id,msg.recipient_id
                     from mailinglist_run_message msg
                        join mailinglist_run_recipient rr on rr.id = msg.recipient_id
                        join mailinglist_run r on r.id = rr.run_id
                     where msg.message_nr = $1
                       and r.details_expired is null
                     for update of r",
                    [MsgId],
                    Ctx
                )
            of
                [] ->
                    undefined;
                [{RunId, Rid}] ->
                    RunId = transition(Rid, State, detail(Detail), Ctx),
                    z_db:q(
                        "update mailinglist_run_message set
                            status=(select status from mailinglist_run_recipient where id=$2),
                            is_final=is_final or $3,
                            retry_count=greatest(retry_count,$4),
                            detail=$5,
                            modified=now()
                          where message_nr=$1",
                        [
                            MsgId,
                            Rid,
                            Final,
                            case Retry of
                                undefined -> 0;
                                _ -> Retry
                            end,
                            detail(Detail)
                        ],
                        Ctx
                    ),
                    RunId
            end
        end,
        Context
    ),
    case Run of
        undefined -> ok;
        _ -> refresh(Run, Context)
    end.

detail(history_expired) ->
    <<"Recipient history has expired. Create a new mailing and explicitly select all recipients; some people may receive this page again.">>;
detail(eacces) ->
    <<"The sender no longer has permission to send this mailing.">>;
detail(missing_context) ->
    <<"The scheduled sender context is missing. Create a new mailing.">>;
detail(invalid_context) ->
    <<"The scheduled sender context is invalid. Create a new mailing.">>;
detail(undefined) ->
    undefined;
detail(B) when is_binary(B) -> B;
detail(T) ->
    iolist_to_binary(io_lib:format("~p", [T])).

-spec stats(Id, Context) -> map() when Id :: integer(), Context :: z:context().
stats(Id, Context) ->
    {ok, Rows} = z_db:qmap(
        "select status,sum(total)::int as total from mailinglist_run_stats where run_id=$1 group by status",
        [Id],
        Context
    ),
    Counts = maps:from_list([{maps:get(<<"status">>, R), maps:get(<<"total">>, R)} || R <- Rows]),
    totals(Counts).

totals(Counts) ->
    Total = lists:sum(maps:values(Counts)),
    Selected = Total - maps:get(<<"skipped">>, Counts, 0),
    Pending = lists:sum([
        maps:get(S, Counts, 0)
     || S <- [<<"pending">>, <<"submitting">>, <<"queued">>, <<"retrying">>]
    ]),
    Counts#{
        <<"total">> => Total,
        <<"waiting">> => Pending,
        <<"unsuccessful">> => maps:get(<<"failed">>, Counts, 0) +
            maps:get(<<"bounced">>, Counts, 0),
        <<"selected">> => Selected,
        <<"processed">> => Selected - Pending,
        <<"percent">> =>
            case Selected of
                0 -> 0;
                _ -> (Selected - Pending) * 100 div Selected
            end
    }.

-spec prepared(Id, Context) -> ok when Id :: integer(), Context :: z:context().
prepared(Id, Context) ->
    z_db:q("update mailinglist_run set prepared=true, modified=now() where id=$1", [Id], Context),
    refresh(Id, Context).

-spec status(binary(), boolean(), map()) -> binary().
status(Old, _, _) when Old =:= <<"cancelled">>; Old =:= <<"failed">> -> Old;
status(<<"interrupted">>, Prepared, S) ->
    case not Prepared orelse maps:get(<<"pending">>, S, 0) + maps:get(<<"submitting">>, S, 0) > 0 of
        true -> <<"interrupted">>;
        false -> status(<<"sending">>, true, S)
    end;
status(_, false, _) ->
    <<"sending">>;
status(_, true, S) ->
    Pending = lists:sum([maps:get(K, S, 0) || K <- [<<"pending">>, <<"submitting">>, <<"queued">>]]),
    Retry = maps:get(<<"retrying">>, S, 0),
    Errors = maps:get(<<"failed">>, S, 0) + maps:get(<<"bounced">>, S, 0),
    if
        Pending > 0 ->
            <<"sending">>;
        Retry > 0 ->
            <<"retrying">>;
        Errors > 0 ->
            <<"completed_errors">>;
        true ->
            case maps:get(<<"sent">>, S, 0) of
                0 -> <<"empty">>;
                _ -> <<"completed">>
            end
    end.

-spec refresh(Id, Context) -> ok when Id :: integer(), Context :: z:context().
refresh(Id, Context) ->
    Result = z_db:transaction(
        fun(Ctx) ->
            {ok, R} = z_db:qmap_row(
                "select status,prepared from mailinglist_run where id=$1 for update", [Id], Ctx
            ),
            Old = maps:get(<<"status">>, R),
            New = status(Old, maps:get(<<"prepared">>, R), stats(Id, Ctx)),
            z_db:q(
                "update mailinglist_run set
                    status=$2::varchar,
                    modified=now(),
                    finished=case when $2::varchar in ('completed','completed_errors','empty') then coalesce(finished,now()) else finished end
                 where id=$1",
                [Id, New],
                Ctx
            ),
            Old =/= New
        end,
        Context
    ),
    case Result of
        true ->
            publish(Id, Context);
        _ ->
            case
                z_db:q1(
                    "update mailinglist_run set
                        notified=now()
                     where id=$1
                       and (notified is null or notified < now()-interval '2 seconds')
                     returning id",
                    [Id],
                    Context
                )
            of
                undefined -> ok;
                _ -> publish(Id, Context)
            end
    end.

-spec publish(Id, Context) -> ok when Id :: integer(), Context :: z:context().
publish(Id, Context) ->
    case z_db:q("select page_id,mailinglist_id from mailinglist_run where id=$1", [Id], Context) of
        [{Page, List}] ->
            lists:foreach(
                fun(Resource) ->
                    z_mqtt:publish(
                        [<<"model">>, <<"mailinglist">>, <<"event">>, Resource, <<"runs">>],
                        #{id => Resource},
                        Context
                    )
                end,
                lists:usort([Page, List])
            ),
            ok;
        [] ->
            ok
    end.

-spec previous(Run, Email, Lang, States, Context) -> boolean() when
    Run :: map(), Email :: binary(), Lang :: binary(), States :: [binary()], Context :: z:context().
previous(Run, Email, Lang, States, Context) ->
    z_db:q1(
        "select exists(
            select 1
            from mailinglist_run_recipient rr
            join mailinglist_run r on r.id=rr.run_id
            where r.page_id=$1
              and r.mailinglist_id=$2
              and r.details_expired is null
              and r.id<>$3
              and rr.email=$4
              and rr.language=$5
              and rr.status=any($6::varchar[]))",
        [
            maps:get(<<"page_id">>, Run),
            maps:get(<<"mailinglist_id">>, Run),
            maps:get(<<"id">>, Run),
            Email,
            Lang,
            States
        ],
        Context
    ).

%% Resolve once, store the actual rendering language, and use it consistently
%% for resource and email-only subscribers. Never silently choose a translation.
-spec language(binary(), term(), binary(), [atom()]) -> {ok, binary()} | {skip, binary()}.
language(Selected, Preferred, Fallback, Available) ->
    Requested =
        case Selected of
            <<>> ->
                case Preferred of
                    undefined -> Fallback;
                    <<>> -> Fallback;
                    _ -> Preferred
                end;
            _ ->
                Selected
        end,
    case z_language:to_language_atom(Requested) of
        {ok, Lang} ->
            case lists:member(Lang, Available) of
                true ->
                    {ok, z_convert:to_binary(Lang)};
                false ->
                    Base = z_language:fallback_language(Lang),
                    case lists:member(Base, Available) of
                        true -> {ok, z_convert:to_binary(Base)};
                        false -> {skip, <<"Missing translation">>}
                    end
            end;
        _ ->
            {skip, <<"Unknown recipient language">>}
    end.

%% Retry/resend retains a link to the original run and its language settings.
-spec resend(Id, Mode, Context) -> {ok, integer()} | {error, term()} when
    Id :: integer(), Mode :: binary(), Context :: z:context().
resend(Id, Mode, Context) when Mode =:= <<"failed">>; Mode =:= <<"all">>; Mode =:= <<"new">> ->
    case get(Id, Context) of
        {ok, R} ->
            case allowed(R, Context) of
                true ->
                    Options = [
                        {single_test_address,
                            proplists:get_value(
                                single_test_address, maps:get(<<"options">>, R, [])
                            )},
                        {parent_id, Id},
                        {send_mode, Mode},
                        {language, maps:get(<<"language">>, R)},
                        {fallback_language, maps:get(<<"fallback_language">>, R)},
                        {audience, maps:get(<<"audience">>, R)}
                    ],
                    create(
                        maps:get(<<"mailinglist_id">>, R),
                        maps:get(<<"page_id">>, R),
                        <<"date">>,
                        calendar:universal_time(),
                        Options,
                        Context
                    );
                false ->
                    {error, eacces}
            end;
        _ ->
            {error, eacces}
    end.

%% Maintenance repair: rebuild summaries from authoritative recipient states.
-spec rebuild_stats(Id, Context) -> ok when Id :: integer(), Context :: z:context().
rebuild_stats(Id, Context) ->
    Rebuilt = z_db:transaction(
        fun(Ctx) ->
            case
                z_db:q1(
                    "select details_expired is not null from mailinglist_run where id=$1 for update",
                    [Id],
                    Ctx
                )
            of
                true ->
                    false;
                _ ->
                    z_db:q(
                        "select id from mailinglist_run_recipient where run_id=$1 order by id for update",
                        [Id],
                        Ctx
                    ),
                    z_db:q("delete from mailinglist_run_stats where run_id=$1", [Id], Ctx),
                    z_db:q(
                        "insert into mailinglist_run_stats
                            (run_id,language,status,total)
                         select run_id,language,status,count(*)
                         from mailinglist_run_recipient
                         where run_id=$1
                         group by run_id,language,status",
                        [Id],
                        Ctx
                    ),
                    true
            end
        end,
        Context
    ),
    case Rebuilt of
        true -> refresh(Id, Context);
        false -> ok
    end.

%% Expired history cannot identify new or failed recipients reliably. An editor
%% must explicitly choose all recipients and review the duplicate-send warning.
-spec check_history(List, Page, Options, Context) -> ok | {error, history_expired} when
    List :: integer(), Page :: integer(), Options :: list(), Context :: z:context().
check_history(List, Page, Options, Context) ->
    Parent = proplists:get_value(parent_id, Options),
    ParentExpired =
        case Parent of
            undefined ->
                false;
            _ ->
                z_db:q1(
                    "select details_expired is not null from mailinglist_run where id=$1",
                    [Parent],
                    Context
                ) =:= true
        end,
    Mode = proplists:get_value(
        send_mode,
        Options,
        case proplists:get_bool(is_send_all, Options) of
            true -> <<"all">>;
            false -> <<"new">>
        end
    ),
    case ParentExpired orelse (Mode =/= <<"all">> andalso history_expired(List, Page, Context)) of
        true -> {error, history_expired};
        false -> ok
    end.

history_expired(List, Page, Context) ->
    z_db:q1(
        "select exists(
            select 1
            from mailinglist_run
            where mailinglist_id=$1
              and page_id=$2
              and details_expired is not null
         )",
        [List, Page],
        Context
    ).

%% Match email-log retention: hourly cleanup, at most 10,000 recipient records
%% per pass. Expiration hides details immediately while deletion drains in batches.
%% Keep run metadata, aggregate counts and saved content; never rebuild pruned summaries.
-spec periodic_cleanup(Context) -> ok when Context :: z:context().
periodic_cleanup(Context) ->
    Expired = z_db:transaction(
        fun(Ctx) ->
            Rows = z_db:q(
                "select id
                 from mailinglist_run
                 where details_expired is null
                   and (status <> 'scheduled' or (type='date' and due < now()-interval '3 months'))
                   and (finished < now()-interval '3 months'
                        or (finished is null and modified < now()-interval '3 months'))
                 order by id
                 limit 100
                 for update skip locked",
                Ctx
            ),
            lists:foreach(
                fun({Id}) ->
                    z_db:q(
                        "update mailinglist_run set
                            details_expired=now(),
                            props=null,
                            error=null,
                            request_key=null,
                            first_submitted=(
                                select min(msg.created)
                                from mailinglist_run_message msg
                                join mailinglist_run_recipient rr on rr.id=msg.recipient_id
                                where rr.run_id=$1),
                            status=case when status in ('completed','completed_errors','empty','cancelled','failed') then status else 'failed' end,
                            finished=coalesce(finished,now())
                         where id=$1",
                        [Id],
                        Ctx
                    )
                end,
                Rows
            ),
            Rows
        end,
        Context
    ),
    z_db:q(
        "delete from mailinglist_run_recipient
         where id in (
            select rr.id
            from mailinglist_run_recipient rr
                join mailinglist_run r on r.id=rr.run_id
            where r.details_expired is not null
            order by rr.id limit 10000
         )",
        Context,
        300000
    ),
    lists:foreach(fun({Id}) -> publish(Id, Context) end, Expired),
    ok.

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022 Marc Worrell
%% @doc Run a resource pivot job.

%% Copyright 2022 Marc Worrell
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

-module(z_pivot_rsc_job).

-export([
    start_pivot/2,

    pivot_job/2,

    pivot_resource_update/4,
    get_pivot_title/1,
    get_pivot_title/2,

    cleanup_tsv_text/1,

    stemmer_language/1,
    stemmer_language_config/1,

    pg_lang/1,
    pg_lang_extra/1
    ]).

-include_lib("zotonic.hrl").

%% Minimum day, inserted for date start search ranges
-define(EPOCH_START, {{-4700,1,1},{0,0,0}}).

%% Max number of characters for a tsv vector.
-define(MAX_TSV_LEN, 30000).


%% @doc Start a task queue sidejob.
-spec start_pivot( list(), z:context() ) -> {ok, pid()} | {error, overload}.
start_pivot(PivotRscList, Context) ->
    sidejob_supervisor:spawn(
            zotonic_sidejobs,
            {?MODULE, pivot_job, [ PivotRscList, Context ]}).


%% @doc Return a modified property list with fields that need immediate pivoting on an update.
pivot_resource_update(Id, UpdateProps, RawProps, Context) ->
    Props = lists:foldl(
        fun(Key, Acc) ->
            case maps:is_key(Key, UpdateProps) of
                false ->
                    Acc#{ Key => maps:get(Key, RawProps, undefined) };
                true ->
                    Acc
            end
        end,
        UpdateProps,
        [ <<"date_start">>, <<"date_end">>, <<"title">> ]),
    {DateStart, DateEnd} = pivot_date(Props),
    PivotTitle = truncate(get_pivot_title(Props), 100),
    Props1 = Props#{
        <<"pivot_date_start">> => DateStart,
        <<"pivot_date_end">> => DateEnd,
        <<"pivot_date_start_month_day">> => month_day(DateStart),
        <<"pivot_date_end_month_day">> => month_day(DateEnd),
        <<"pivot_title">> => PivotTitle
    },
    z_notifier:foldr(#pivot_update{id=Id, raw_props=RawProps}, Props1, Context).


%% @doc Run the sidejob task queue task.
-spec pivot_job( list(), z:context() ) -> ok.
pivot_job(PivotRscList, Context) ->
    z_context:logger_md(Context),
    ?LOG_DEBUG(#{
        text => <<"Pivot start">>,
        in => zotonic_core,
        rsc_list => PivotRscList
    }),
    F = fun(Ctx) ->
            [ {Id, pivot_resource(Id, Ctx)} || {Id,_Serial} <- PivotRscList ]
    end,
    case z_db:transaction(F, Context) of
        {rollback, PivotError} ->
            ?LOG_ERROR(#{
                text => <<"Pivot error">>,
                in => zotonic_core,
                rsc_list => PivotRscList,
                error => rollback,
                reason => PivotError
            });
        L when is_list(L) ->
            lists:map(
                fun({Id, _Serial}) ->
                    IsA = m_rsc:is_a(Id, Context),
                    z_notifier:notify(#rsc_pivot_done{id=Id, is_a=IsA}, Context),
                    % Flush the resource, as some synthesized attributes might depend on the pivoted fields.
                    % @todo Only do this if some fields are changed
                    m_rsc_update:flush(Id, Context)
                end,
                PivotRscList),
            lists:map(
                fun
                    ({Id, ok}) ->
                        ?LOG_DEBUG(#{
                            text => <<"Pivot done">>,
                            in => zotonic_core,
                            rsc_id => Id
                        }),
                        ok;
                    ({Id, {error, Reason}}) ->
                        ?LOG_ERROR(#{
                            text => <<"Pivot error">>,
                            in => zotonic_core,
                            rsc_id => Id,
                            reason => Reason
                        })
                end, L),
            delete_queue(PivotRscList, Context)
    end,
    z_pivot_rsc:pivot_job_done(Context).


%% @doc Delete the previously queued ids iff the queue entry has not been updated in the meanwhile
delete_queue(Qs, Context) ->
    F = fun(Ctx) ->
        lists:foreach(
            fun({Id, Serial}) ->
                delete_queue(Id, Serial, Ctx)
            end,
            Qs)
    end,
    z_db:transaction(F, Context).

%% @doc Delete a specific id/serial combination
delete_queue(_Id, undefined, _Context) ->
    ok;
delete_queue(Id, Serial, Context) ->
    z_db:q("
        delete from rsc_pivot_queue
        where rsc_id = $1
          and serial = $2",
        [ Id, Serial ],
        Context).

-spec pivot_resource(m_rsc:resource_id(), z:context()) -> ok | {error, enoent | term()}.
pivot_resource(Id, Context0) ->
    z_pivot_rsc:pivot_job_ping(Id, Context0),
    Lang = stemmer_language_config(Context0),
    Context = z_context:set_language(Lang,
                 z_context:set_tz(<<"UTC">>,
                    z_acl:sudo(Context0))),
    try
        pivot_resource_1(Id, Lang, Context)
    catch
        Type:Err:Stack ->
            ?LOG_ERROR(#{
                text => <<"Pivot error">>,
                in => zotonic_core,
                rsc_id => Id,
                result => Type,
                reason => Err,
                stack => Stack
            }),
            {error, Err}
    end.

pivot_resource_1(Id, Lang, Context) ->
    case m_rsc:exists(Id, Context) of
        true ->
            RscProps = get_pivot_rsc(Id, Context),
            Vars = #{
                id => Id,
                props => RscProps,
                z_language => Lang
            },
            case z_template_compiler_runtime:map_template({cat, <<"pivot/pivot.tpl">>}, Vars, Context) of
                {ok, Template} ->
                    TextA = render_block(a, Template, Vars, Context),
                    TextB = render_block(b, Template, Vars, Context),
                    TextC = render_block(c, Template, Vars, Context),
                    TextD = render_block(d, Template, Vars, Context),
                    TsvIds = render_block(related_ids, Template, Vars, Context),
                    Title = render_block(title, Template, Vars, Context),
                    Street = render_block(address_street, Template, Vars, Context),
                    City = render_block(address_city, Template, Vars, Context),
                    Postcode = render_block(address_postcode, Template, Vars, Context),
                    State = render_block(address_state, Template, Vars, Context),
                    Country = render_block(address_country, Template, Vars, Context),
                    NameFirst = render_block(name_first, Template, Vars, Context),
                    NameSurname = render_block(name_surname, Template, Vars, Context),
                    Gender = render_block(gender, Template, Vars, Context),
                    DateStart = to_datetime(render_block(date_start, Template, Vars, Context)),
                    DateEnd = to_datetime(render_block(date_end, Template, Vars, Context)),
                    DateStartMonthDay = to_integer(render_block(date_start_month_day, Template, Vars, Context)),
                    DateEndMonthDay = to_integer(render_block(date_end_month_day, Template, Vars, Context)),
                    LocationLat = to_float(render_block(location_lat, Template, Vars, Context)),
                    LocationLng = to_float(render_block(location_lng, Template, Vars, Context)),

                    % Make psql tsv texts from the A..D blocks
                    StemmerLanguage = stemmer_language(Context),
                    {SqlA, ArgsA} = to_tsv(TextA, $A, [], StemmerLanguage),
                    {SqlB, ArgsB} = to_tsv(TextB, $B, ArgsA, StemmerLanguage),
                    {SqlC, ArgsC} = to_tsv(TextC, $C, ArgsB, StemmerLanguage),
                    {SqlD, ArgsD} = to_tsv(TextD, $D, ArgsC, StemmerLanguage),

                    % Make the text and object-ids vectors for the pivot
                    TsvSql = [SqlA, " || ", SqlB, " || ", SqlC, " || ", SqlD],
                    Tsv  = z_db:q1(iolist_to_binary(["select ", TsvSql]), ArgsD, Context),
                    Rtsv = z_db:q1("select to_tsvector($1)", [TsvIds], Context),

                    KVs = #{
                        <<"pivot_tsv">> => Tsv,
                        <<"pivot_rtsv">> =>  Rtsv,
                        <<"pivot_street">> => truncate(Street, 120),
                        <<"pivot_city">> => truncate(City, 100),
                        <<"pivot_postcode">> => truncate(Postcode, 30),
                        <<"pivot_state">> => truncate(State, 50),
                        <<"pivot_country">> => truncate(Country, 80),
                        <<"pivot_first_name">> => truncate(NameFirst, 100),
                        <<"pivot_surname">> => truncate(NameSurname, 100),
                        <<"pivot_gender">> => truncate(Gender, 1),
                        <<"pivot_date_start">> => DateStart,
                        <<"pivot_date_end">> => DateEnd,
                        <<"pivot_date_start_month_day">> => DateStartMonthDay,
                        <<"pivot_date_end_month_day">> => DateEndMonthDay,
                        <<"pivot_title">> => truncate(Title, 100),
                        <<"pivot_location_lat">> => LocationLat,
                        <<"pivot_location_lng">> => LocationLng
                    },
                    KVs1 = z_notifier:foldr(#pivot_fields{id=Id, raw_props=RscProps}, KVs, Context),
                    update_changed(Id, KVs1, RscProps, Context),
                    pivot_resource_custom(Id, Context),

                    case to_datetime(render_block(date_repivot, Template, Vars, Context)) of
                        undefined ->
                            ok;
                        DateRepivot ->
                            z_pivot_rsc:insert_queue(Id, DateRepivot, Context)
                    end,
                    ok;
                {error, enoent} ->
                    ?LOG_ERROR(#{
                        text => <<"Missing 'pivot/pivot.tpl' template">>,
                        in => zotonic_core,
                        result => error,
                        reason => enoent,
                        template => <<"pivot/pivot.tpl">>,
                        id => Id
                    }),
                    {error, no_pivot_template}
            end;
        false ->
            {error, enoent}
    end.

render_block(Block, Template, Vars, Context) ->
    {Output, _RenderState} = z_template:render_block_to_iolist(Block, Template, Vars, Context),
    iolist_to_binary(Output).

%% @doc Check which pivot fields are changed, update only those
update_changed(Id, KVs, RscProps, Context) ->
    KVsChanged = maps:filter(
        fun
            (K, V) when is_list(V) ->
                maps:get(K, RscProps, undefined) =/= iolist_to_binary(V);
            (K, undefined) ->
                maps:get(K, RscProps, undefined) =/= undefined;
            (K, V) when is_atom(V) ->
                maps:get(K, RscProps, undefined) =/= z_convert:to_binary(V);
            (K, V) ->
                maps:get(K, RscProps, undefined) =/= V
        end,
        KVs),
    case maps:size(KVsChanged) of
        0 ->
            ok;
        _ ->
            % Make Sql update statement for the changed fields
            {Sql, Args} = maps:fold(
                    fun(K, V, {Sq,As}) ->
                        {[  Sq,
                            case As of [] -> []; _ -> $, end,
                            z_convert:to_list(K), " = $", integer_to_list(length(As)+1)
                        ],
                        [V|As]}
                    end,
                    {"update rsc set ",[]},
                    KVsChanged),

            z_db:q1(iolist_to_binary([Sql, " where id = $", integer_to_list(length(Args)+1)]),
                    lists:reverse([Id|Args]),
                    Context)
    end.


pivot_resource_custom(Id, Context) ->
    CustomPivots = z_notifier:map(#custom_pivot{ id = Id }, Context),
    lists:foreach(
            fun
                (undefined) -> ok;
                (ok) -> ok;
                (none) -> ok;
                ({error, Reason}) ->
                    ?LOG_ERROR(#{
                        text => <<"Error return from custom pivot">>,
                        in => zotonic_core,
                        rsc_id => Id,
                        result => error,
                        reason => Reason
                    });
                ({_Module, _Columns} = Res) ->
                    update_custom_pivot(Id, Res, Context)
            end,
            CustomPivots),
    ok.

update_custom_pivot(Id, {Module, Columns}, Context) ->
    TableName = "pivot_" ++ z_convert:to_list(Module),
    Result = case z_db:select(TableName, Id, Context) of
        {ok, _Row}  ->
            z_db:update(TableName, Id, Columns, Context);
        {error, enoent} ->
            z_db:insert(TableName, [ {id, Id} | Columns ], Context)
    end,
    case Result of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Error updating custom pivot">>,
                in => zotonic_core,
                result => error,
                reason => Reason,
                pivot_module => Module,
                id => Id
            })
    end.

to_datetime(Text) ->
    case z_string:trim(Text) of
        <<>> -> undefined;
        Text1 -> check_datetime(z_datetime:to_datetime(Text1))
    end.

check_datetime({{Y,M,D},{H,I,S}} = Date)
    when is_integer(Y), is_integer(M), is_integer(D),
         is_integer(H), is_integer(I), is_integer(S) ->
    Date;
% check_datetime({Y,M,D} = Date)
%     when is_integer(Y), is_integer(M), is_integer(D) ->
%     {Date, {0,0,0}};
check_datetime(_) ->
    undefined.


%% Make the setweight(to_tsvector()) parts of the update statement
to_tsv(Text, Level, Args, StemmingLanguage) when is_binary(Text) ->
    case cleanup_tsv_text(z_html:unescape(z_html:strip(Text))) of
        <<>> ->
            {"tsvector('')", Args};
        TsvText ->
            N = length(Args) + 1,
            Truncated = z_string:truncatechars(TsvText, ?MAX_TSV_LEN, <<>>),
            Args1 = Args ++ [Truncated],
            {["setweight(to_tsvector('pg_catalog.",StemmingLanguage,"', $",integer_to_list(N),"), '",Level,"')"], Args1}
    end.

-spec to_float(binary()) -> float() | undefined.
to_float(Text) ->
    case z_string:trim(Text) of
        <<>> -> undefined;
        Text1 -> z_convert:to_float(Text1)
    end.

-spec to_integer(binary()) -> integer() | undefined.
to_integer(Text) ->
    case z_string:trim(Text) of
        <<>> -> undefined;
        Text1 -> z_convert:to_integer(Text1)
    end.

-spec cleanup_tsv_text(binary()) -> binary().
cleanup_tsv_text(Text) when is_binary(Text) ->
    Text1 = z_string:sanitize_utf8(Text),
    Text2 = iolist_to_binary(re:replace(Text1, <<"[ ",13,10,9,"/-]+">>, <<" ">>, [global])),
    z_string:trim(Text2).

-spec truncate(binary(), integer()) -> binary().
truncate(S, Len) ->
    S1 = z_string:trim(S),
    S2 = truncate_1(S1, Len, <<>>),
    z_string:trim( z_string:to_lower(S2) ).

truncate_1(_S, 0, Acc) ->
    Acc;
truncate_1(<<>>, _Len, Acc) ->
    Acc;
truncate_1(<<C/utf8, Rest/binary>>, Len, Acc) when C =< 32 ->
    truncate_1(Rest, Len - 1, <<Acc/binary, " ">>);
truncate_1(<<C/utf8, Rest/binary>>, Len, Acc) ->
    N = size(<<C/utf8>>),
    case Len >= N of
        true -> truncate_1(Rest, Len - N, <<Acc/binary, C/utf8>>);
        false -> Acc
    end;
truncate_1(<<_, Rest/binary>>, Len, Acc) ->
    % Drop not-utf8 codepoints
    truncate_1(Rest, Len, Acc).


%% @doc Fetch the date range from the record
pivot_date(R) ->
    DateStart = z_datetime:undefined_if_invalid_date(maps:get(<<"date_start">>, R, undefined)),
    DateEnd   = z_datetime:undefined_if_invalid_date(maps:get(<<"date_end">>, R, undefined)),
    pivot_date1(DateStart, DateEnd).

pivot_date1(S, E) when not is_tuple(S) andalso not is_tuple(E) ->
    {undefined, undefined};
pivot_date1(S, E) when not is_tuple(S) andalso is_tuple(E) ->
    { ?EPOCH_START, E};
pivot_date1(S, E) when is_tuple(S) andalso not is_tuple(E) ->
    {S, ?ST_JUTTEMIS};
pivot_date1(S, E) when is_tuple(S) andalso is_tuple(E) ->
    {S, E}.

month_day(undefined) -> undefined;
month_day(?EPOCH_START) -> undefined;
month_day(?ST_JUTTEMIS) -> undefined;
month_day({{_Y,M,D}, _}) -> M*100+D.

%% @doc Fetch the title in the default language for sorting.
-spec get_pivot_title(m_rsc:resource_id(), z:context()) -> binary().
get_pivot_title(Id, Context) ->
    Title = m_rsc:p(Id, <<"title">>, z_language:default_language(Context), Context),
    Lang = z_language:default_language(Context),
    Title1 = z_trans:lookup_fallback(Title, Lang, Context),
    z_string:trim(z_string:to_lower(Title1)).

%% @doc Fetch the first title from the record for sorting.
-spec get_pivot_title(map()) -> binary().
get_pivot_title(Props) ->
    Title = case maps:get(<<"title">>, Props, <<>>) of
        #trans{ tr = [] } ->
            <<>>;
        #trans{ tr = [{_, Text}|_] } ->
            Text;
        T ->
            T
    end,
    z_string:trim(z_string:to_lower(Title)).


%% @doc Return the raw resource data for the pivoter
-spec get_pivot_rsc(m_rsc:resource_id(), z:context()) -> map() | undefined.
get_pivot_rsc(Id, Context) ->
    case z_db:qmap_props_row(
        "select * from rsc where id = $1",
        [ Id ],
        [ {keys, binary} ],
        Context)
    of
        {ok, FullRecord} ->
            z_notifier:foldl(#pivot_rsc_data{ id = Id }, FullRecord, Context);
        {error, _} ->
            undefined
    end.


%% @doc Translate a language to a language string as used by
%% postgresql. This language list is the intersection of the default
%% catalogs of postgres with the languages supported by
%% mod_translation.
pg_lang(dk) -> "danish";
pg_lang(nl) -> "dutch";
pg_lang(en) -> "english";
pg_lang(fi) -> "finnish";
pg_lang(fr) -> "french";
pg_lang(de) -> "german";
pg_lang(hu) -> "hungarian";
pg_lang(it) -> "italian";
pg_lang(no) -> "norwegian";
pg_lang(ro) -> "romanian";
pg_lang(ru) -> "russian";
pg_lang(es) -> "spanish";
pg_lang(se) -> "swedish";
pg_lang(tr) -> "turkish";
pg_lang(<<"dk">>) -> "danish";
pg_lang(<<"nl">>) -> "dutch";
pg_lang(<<"en">>) -> "english";
pg_lang(<<"fi">>) -> "finnish";
pg_lang(<<"fr">>) -> "french";
pg_lang(<<"de">>) -> "german";
pg_lang(<<"hu">>) -> "hungarian";
pg_lang(<<"it">>) -> "italian";
pg_lang(<<"no">>) -> "norwegian";
pg_lang(<<"ro">>) -> "romanian";
pg_lang(<<"ru">>) -> "russian";
pg_lang(<<"es">>) -> "spanish";
pg_lang(<<"se">>) -> "swedish";
pg_lang(<<"tr">>) -> "turkish";
pg_lang(_) -> "english".

%% Map extra languages, these are from the i18n.language_stemmer configuration and not
%% per default installed in PostgreSQL
pg_lang_extra(LangCode) ->
    case z_language:english_name(z_convert:to_atom(LangCode)) of
        undefined ->
            pg_lang(LangCode);
        Lang ->
            lists:takewhile(fun
                                (C) when C >= $a, C =< $z -> true;
                                (_) -> false
                            end,
                            z_convert:to_list(z_string:to_lower(Lang)))
    end.

% Default stemmers in a Ubuntu psql install:
%
%  pg_catalog | danish_stem     | snowball stemmer for danish language
%  pg_catalog | dutch_stem      | snowball stemmer for dutch language
%  pg_catalog | english_stem    | snowball stemmer for english language
%  pg_catalog | finnish_stem    | snowball stemmer for finnish language
%  pg_catalog | french_stem     | snowball stemmer for french language
%  pg_catalog | german_stem     | snowball stemmer for german language
%  pg_catalog | hungarian_stem  | snowball stemmer for hungarian language
%  pg_catalog | italian_stem    | snowball stemmer for italian language
%  pg_catalog | norwegian_stem  | snowball stemmer for norwegian language
%  pg_catalog | portuguese_stem | snowball stemmer for portuguese language
%  pg_catalog | romanian_stem   | snowball stemmer for romanian language
%  pg_catalog | russian_stem    | snowball stemmer for russian language
%  pg_catalog | simple          | simple dictionary: just lower case and check for stopword
%  pg_catalog | spanish_stem    | snowball stemmer for spanish language
%  pg_catalog | swedish_stem    | snowball stemmer for swedish language
%  pg_catalog | turkish_stem    | snowball stemmer for turkish language

%% @doc Return the language used for stemming the full text index.
%%      We use a single stemming to prevent having seperate indexes per language.
-spec stemmer_language(z:context()) -> string().
stemmer_language(Context) ->
    StemmingLanguage = m_config:get_value(i18n, language_stemmer, Context),
    case z_utils:is_empty(StemmingLanguage) of
        true -> pg_lang(z_language:default_language(Context));
        false -> pg_lang_extra(StemmingLanguage)
    end.

-spec stemmer_language_config(z:context()) -> atom().
stemmer_language_config(Context) ->
    StemmingLanguage = m_config:get_value(i18n, language_stemmer, Context),
    case z_utils:is_empty(StemmingLanguage) of
        true ->
            z_language:default_language(Context);
        false ->
            case z_language:to_language_atom(StemmingLanguage) of
                {ok, LangAtom} -> LangAtom;
                {error, not_a_language} -> z_language:default_language(Context)
            end
    end.


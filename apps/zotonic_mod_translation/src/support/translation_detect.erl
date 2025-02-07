%% @doc N-Gram language detection implementation.
%%
%% Modified from the Erlang version by Nathan Zorn at
%% https://github.com/thepug/erlngram
%%
%% Based on the ruby version found at
%% http://github.com/feedbackmine/language_detector Thanks to yong!
%%
%% See the wikipedia article for more information about this method.
%% http://en.wikipedia.org/wiki/Ngram
%% @end

%% Copyright 2009 Nathan Zorn
%% Copyright 2024 Marc Worrell
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

-module(translation_detect).

-author("Marc Worrell <marc@worrell.nl>").
-author("Nathan Zorn <nathan@collecta.com>").

-define(punctuation, ".,\n\t\r!?\"()+_[]{}#$%&'*/0123456789:;<=>@\\^_`|~").
-define(TRAIN_TOKEN_LIMIT, 20_000).
-define(NOMATCH_SCORE, 21_000).
-define(TEXT_DETECT_LIMIT, 500).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    detect/2,
    detect/1,

    ngrams/1,
    ngram_count/3,
    tokenize/1,
    compute_distance/3,
    generate_profile_data/0,
    load_profile_data/0,
    profiles/0,
    profile_data/1,
    train/0,
    train/1
    ]).

-type ngram() :: string().
-type ngram_score() :: {ngram(), non_neg_integer()}.
-type distance() :: non_neg_integer().
-type profiles() :: [ {z_language:language_code(), profile()} ].
-type profile() :: #{ ngram() => non_neg_integer() }.

%% @doc Return the most likely langauge of a given text.  Given the text
%% string this function looks at all trained profiles and returns the
%% best matching (closest profile) language code. Only language codes that
%% are editable can be matched.
-spec detect(Text, Context) -> {ok, Language} | {error, nomatch} when
    Text :: binary(),
    Context :: z:context(),
    Language :: z_language:language_code().
detect(Text, Context) ->
    Languages = z_language:editable_language_codes(Context),
    detect_1(Text, Languages).

%% @doc Return the most likely langauge of a given text.  Given the text
%% string this function looks at all trained profiles and returns the
%% best matching (closest profile) language code.
-spec detect(Text) -> {ok, Language} | {error, nomatch} when
    Text :: binary(),
    Language :: z_language:language_code().
detect(Text) ->
    detect_1(Text, []).


-spec detect_1(Text, Languages) -> {ok, Language} | {error, nomatch} when
    Text :: binary(),
    Languages :: [ z_language:language_code() ],
    Language :: z_language:language_code().
detect_1(Text, Languages) ->
    Ngrams = maps:to_list( ngrams(z_string:truncate(Text, ?TEXT_DETECT_LIMIT, <<>>)) ),
    {Iso, _} = lists:foldl(
        fun(Lang, {_, Best} = Acc) ->
            case Languages =:= [] orelse lists:member(Lang, Languages) of
                true ->
                    case profile_data(Lang) of
                        {ok, Profile} ->
                            D = compute_distance(Ngrams, Profile, Best),
                            case D < Best of
                                true ->
                                    {Lang, D};
                                false ->
                                    Acc
                            end;
                        {error, _} ->
                            Acc
                    end;
                false ->
                    Acc
            end
       end,
       {none, length(Ngrams) * ?NOMATCH_SCORE},
       profiles()),
    case Iso of
        none -> {error, nomatch};
        _ -> {ok, Iso}
    end.

%% @doc compute the distance between two ngram profiles Compare the list of
%% ngrams to the dictionary of language profiles.  Best is used to
%% keep track of the best distance. This ends the function when Best
%% is less than the computed distance.
-spec compute_distance(Ngrams, Profile, Best) -> Distance when
    Ngrams :: list(ngram_score()),
    Profile :: profile(),
    Best :: distance(),
    Distance :: distance().
compute_distance(P1, P2, Best) ->
    compute_distance(P1, P2, Best, 0).

compute_distance([], _Profile, _Best, Dist) ->
    Dist;
compute_distance(_Ngrams, _Profile, Best, Dist) when Best < Dist ->
    % terminate early - worse than best match till now
    Dist;
compute_distance([{K, Val}|R], Profile, Best, Dist) ->
    D = case maps:get(K, Profile, undefined) of
        undefined -> ?NOMATCH_SCORE;
        Value when is_integer(Value), is_integer(Val) -> abs(Val - Value)
    end,
    compute_distance(R, Profile, Best, D + Dist).

%% %% @doc Compile the language profiles into data files, reload the data files.
-spec generate_profile_data() -> {ok, Languages} when
    Languages :: [ z_language:language_code() ].
generate_profile_data() ->
    Profiles = train(),
    lists:foreach(
        fun({Iso, Profile}) ->
            save_profile_data(Iso, Profile)
        end,
        Profiles),
    load_profile_data().

save_profile_data(Iso, Data) ->
    Filename = filename:join([
        code:priv_dir(zotonic_mod_translation), "data", "profiles",
        z_convert:to_list(Iso) ++ ".dat" ]),
    file:write_file(Filename, term_to_binary(Data, [compressed, {minor_version, 1}])).

%% @doc (Re)load all profile data files in the priv/data/profiles dir.
-spec load_profile_data() -> {ok, Languages} when
    Languages :: [ z_language:language_code() ].
load_profile_data() ->
    Wildcard = filename:join([ code:priv_dir(zotonic_mod_translation), "data", "profiles", "*.dat" ]),
    Files = filelib:wildcard(Wildcard),
    Ps = lists:map(
        fun(F) ->
            Basename = filename:basename(F),
            Rootname = filename:rootname(Basename),
            Iso = list_to_atom(Rootname),
            profile_data(Iso),
            Iso
        end,
        Files),
    persistent_term:put({zotonic_mod_translation, profiles}, Ps),
    {ok, Ps}.

%% @doc Return the list of languages in the priv/data/profiles dir.
-spec profiles() -> Languages when
    Languages :: [ z_language:language_code() ].
profiles() ->
    case persistent_term:get({zotonic_mod_translation, profiles}, undefined) of
        undefined ->
            Wildcard = filename:join([ code:priv_dir(zotonic_mod_translation), "data", "profiles", "*.dat" ]),
            Files = filelib:wildcard(Wildcard),
            Ps = lists:map(
                fun(F) ->
                    Basename = filename:basename(F),
                    Rootname = filename:rootname(Basename),
                    list_to_atom(Rootname)
                end,
                Files),
            persistent_term:put({zotonic_mod_translation, profiles}, Ps),
            Ps;
        Ps when is_list(Ps) ->
            Ps
    end.

%% @doc Return the profile for a certain language. Cache the profile in the
%% persistent term storage.
-spec profile_data(Iso) -> {ok, Profile} | {error, Reason} when
    Iso :: z_language:language_code(),
    Profile :: profile(),
    Reason :: term().
profile_data(Iso) when is_atom(Iso) ->
    case persistent_term:get({zotonic_mod_translation, Iso}, undefined) of
        undefined ->
            Filename = filename:join([
                code:priv_dir(zotonic_mod_translation), "data", "profiles",
                z_convert:to_list(Iso) ++ ".dat" ]),
            case file:read_file(Filename) of
                {ok, Bin} ->
                    Data = binary_to_term(Bin),
                    persistent_term:put({zotonic_mod_translation, Iso}, {ok, Data}),
                    {ok, Data};
                {error, _} = Error ->
                    persistent_term:put({zotonic_mod_translation, Iso}, Error),
                    Error
            end;
        {ok, _} = Ok ->
            Ok;
        {error, _} = Error ->
            Error
    end.

%% @doc Read all language files to store as ngram profiles
%% sample wikipedia data is provied in this repository.
-spec train() -> Profiles when
    Profiles :: profiles().
train() ->
    train(filename:join([ code:priv_dir(zotonic_mod_translation), "data", "texts" ])).

%% @doc Read in directory of language files to store as ngram profiles
%% sample wikipedia data is provied in this repository.
-spec train(Directory) -> Profiles when
    Directory :: file:filename_all(),
    Profiles :: profiles().
train(Directory) ->
    Files = filelib:wildcard(filename:join(Directory, "*.txt")),
    TrainingData = lists:filtermap(
        fun(File) ->
            Filename = filename:basename(File),
            Language = filename:rootname(Filename),
            case z_language:to_language_atom(Language) of
                {ok, Iso} ->
                    {true, {Iso, File}};
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_translation,
                        text => <<"Unknown language for language detect trainings data">>,
                        result => error,
                        reason => Reason,
                        language => iolist_to_binary(Language),
                        file => File
                    }),
                    false
            end
        end,
        Files),
    lists:map(fun train_from_file/1, TrainingData).

%% read in each file and create profile
train_from_file({Iso, File}) ->
    %% read from file
    {ok, B} = file:read_file(File),
    B1 = z_string:sanitize_utf8(B),
    {Iso, ngrams(B1)}.

%% ngrams(Text) return a list of ngrams for given text
-spec ngrams( Text ) -> Ngrams when
    Text :: binary(),
    Ngrams :: profile().
ngrams(<<>>) ->
    #{};
ngrams(Text) ->
    Tokens = tokenize(Text),
    P1 = lists:reverse(
        lists:keysort(2,
            ngram_each([2, 3, 4, 5], Tokens, [])
        )
    ),
    ngrams_add_score(P1).

ngrams_add_score(L) ->
    ngrams_add_score(L, #{}, -1, 0, 0).

ngrams_add_score([], Pl, _PrevCount, _PrevNr, _Nr) ->
    Pl;
ngrams_add_score(_, Pl, _PrevCount, _PrevNr, Nr) when Nr >= ?TRAIN_TOKEN_LIMIT ->
    Pl;
ngrams_add_score([{Key,Count}|T], Pl, PrevCount, PrevNr, Nr) when Count =:= PrevCount ->
    Pl1 = Pl#{ Key => PrevNr },
    ngrams_add_score(T, Pl1, Count, PrevNr, Nr + 1);
ngrams_add_score([{Key,Count}|T], Pl, _PrevCount, _PrevNr, Nr) ->
    Nr1 = Nr + 1,
    Pl1 = Pl#{ Key => Nr1 },
    ngrams_add_score(T, Pl1, Count, Nr1, Nr1).


%% count the ngram in the given string
ngram(N, PL, L) when length(L) < N ->
    PL;
ngram(N, PL, [_|R]=L) ->
    {Key, _} = lists:split(N, L),
    PL2 = incr_pl(Key, PL),
    ngram(N, PL2, R).

%% iterate through list of integers and call ngram_count with given integer
ngram_each(_N, [], []) ->
    []; %% handle empty list and accumulator
ngram_each([], _Text, PL) ->
    dict:to_list(PL);
ngram_each(NL, Text, PL) when is_list(PL) ->
    ngram_each(NL, Text, dict:from_list(PL));
ngram_each([N|R], Text, PL) ->
    X = ngram_count(N, Text, PL),
    ngram_each(R, Text, X).

%% calls ngram() on each token
ngram_count(_N, [], PL) ->
    PL;
ngram_count(N, [T|R], PL) ->
    T0 = case length(T) >= N of
            true ->
                "_" ++ T ++ string:chars($_, N - 1);
            false ->
                T
         end,
    X = ngram(N, PL, T0),
    ngram_count(N, R, X).

incr_pl(Key, D) ->
    dict:update_counter(Key, 1, D).

%% remove punctuation characters
strip_punctuation(Input) ->
    strip_punctuation(Input, <<>>).

strip_punctuation(<<>>, Acc) ->
    Acc;
strip_punctuation(<<C/utf8, T/binary>>, Acc) ->
    case lists:member(C, ?punctuation) of
        false ->
            strip_punctuation(T, <<Acc/binary, C/utf8>>);
        true ->
            strip_punctuation(T, <<Acc/binary, " ">>)
    end.

%% tokenize , split up strings
-spec tokenize(Text) -> Tokens when
    Text :: binary(),
    Tokens :: list( string() ).
tokenize(Text) ->
    tokenize(Text, z_string:len(Text)).

-spec tokenize(Text, Length) -> Tokens when
    Text :: binary(),
    Length :: non_neg_integer(),
    Tokens :: list( string() ).
tokenize(Text, Length) ->
    Text1 = z_string:to_lower(Text),
    Text2 = z_string:truncatechars(Text1, Length, <<>>),
    Text3 = strip_punctuation(Text2),
    Tokens = binary:split(Text3, <<" ">>, [ global, trim_all ]),
    [ unicode:characters_to_list(Token) || Token <- Tokens ].


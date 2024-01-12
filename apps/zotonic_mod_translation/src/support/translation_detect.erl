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
    profile_to_erl/0,
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
    Profiles = translation_detect_profiles:profiles(),
    Ngrams = maps:to_list( ngrams(z_string:truncate(Text, ?TEXT_DETECT_LIMIT, <<>>)) ),
    {Iso, _} = lists:foldl(
        fun({Lang, ProfileFun}, {_, Best} = Acc) ->
            case Languages =:= [] orelse lists:member(Lang, Languages) of
                true ->
                    Profile = translation_detect_profiles:ProfileFun(),
                    D = compute_distance(Ngrams, Profile, Best),
                    case D < Best of
                        true ->
                            {Lang, D};
                        false ->
                            Acc
                    end;
                false ->
                    Acc
            end
       end,
       {none, length(Ngrams) * ?NOMATCH_SCORE},
       Profiles),
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
        Value -> abs(Val - Value)
    end,
    compute_distance(R, Profile, Best, D + Dist).

%% %% @doc Compile the language profiles into an erlang file for better performance.
-spec profile_to_erl() -> ok.
profile_to_erl() ->
    Profiles = train(),
    Languages = [ Iso || {Iso, _} <- Profiles ],
    FunNames = [ list_to_atom("profile_" ++ atom_to_list(Iso)) || Iso <- Languages ],
    Funs = lists:map(
        fun({Iso, Profile}) ->
            [
                "'profile_", z_convert:to_binary(Iso), "'() -> ",
                io_lib:format("~w.~n~n", [Profile])
            ]
        end,
        Profiles),
    LangFun = lists:zip(Languages, FunNames),
    FileData = iolist_to_binary([
        "%% Generated by translation_detect.erl\n\n",
        "-module(translation_detect_profiles).\n",
        "-export([",
            [ [$', atom_to_list(FunName), "'/0,\n"] || FunName <- FunNames ],
            "profiles/0]).\n\n",

        "-spec profiles() -> list( {z_language:language_code(), atom()} ).\n",
        io_lib:format("profiles() -> ~w.~n", [LangFun]),
        Funs
    ]),
    File = filename:join([
        code:lib_dir(zotonic_mod_translation),
        "src", "support", "translation_detect_profiles.erl"
    ]),
    file:write_file(File, FileData),
    ok.

%% @doc Read all language files to store as ngram profiles
%% sample wikipedia data is provied in this repository.
-spec train() -> Profiles when
    Profiles :: profiles().
train() ->
    train(filename:join([ code:priv_dir(zotonic_mod_translation), "data" ])).

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
    [];
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


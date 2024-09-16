%% @author Marc Worrell <marc@worrell.nl>
%% @copyright (c) 2009-2020 Marc Worrell
%% @doc Support for the {% lib filename ... %} tag in the templates.
%% Generates the &lt;link /&gt; or %lt;script /%gt; tag for css or js files.  Also
%% adds the greatest modification date so that updates are loaded by
%% the browser.

%% Copyright 2009-2020 Marc Worrell
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

-module(z_lib_include).

-author("Marc Worrell <marc@worrell.nl>").

-include_lib("zotonic.hrl").

-export([
    tag/2, tag/3,
    url/2, url/3,
    uncollapse/1
]).

-define(SEP, $~).
-define(TAG_CACHE_TIME, 60). % Cache generated link and script tags for 1 minute.

-type option() :: minify
                | {minify, boolean()}
                | async
                | {async, boolean()}
                | defer
                | {defer, boolean()}
                | absolute_url
                | {absolute_url, boolean()}
                | {media, binary()}
                | {rel, binary()}
                | {title, binary()}.

-type options() :: [ option() ].

-export_type([option/0, options/0]).


%% @doc Generate the link and/or script tags for the given files.
-spec tag( [ binary() ], z:context() ) -> [ [ binary() ] ].
tag(Files, Context) ->
    tag(Files, [], Context).

%% @doc Generate the link and/or script tags for the given files.
-spec tag( [ binary() ], options(), z:context() ) -> [ [ binary() ] ].
tag(Files, Args, Context) ->
    case m_config:get_boolean(mod_development, libsep, Context) of
        false ->
            tag1(Files, Args, Context);
        true ->
            lists:foldr(fun(F, [Css,Js]) ->
                            [C,J] = tag1([F], Args, Context),
                            [[C|Css], [J|Js]]
                         end,
                         [[],[]],
                         Files)
    end.

%% @doc Generate urls for the given files, js and css files are separated.
-spec url( [ binary() ], z:context() ) -> [ binary() ].
url(Files, Context) ->
    url(Files, [], Context).

%% @doc Generate urls for the given files, js and css files are separated.
-spec url( [ binary() ], options(), z:context() ) -> [ binary() ].
url(Files, Args, Context) ->
    case split_css_js(Files) of
        {[], []} ->
            [];
        {CssFiles, []} ->
            [ url_for(CssFiles, <<".css">>, Args, Context) ];
        {[], JsFiles} ->
            [ url_for(JsFiles, <<".js">>, Args, Context) ];
        {CssFiles, JsFiles} ->
            [ url_for(CssFiles, <<".css">>, Args, Context)
            , url_for(JsFiles, <<".js">>, Args, Context)
            ]
    end.

-spec url_for( list( binary() ), binary(), options(), z:context() ) -> binary().
url_for([], _Ext, _Args, _Context) ->
    <<>>;
url_for(P, Ext, Args, Context) ->
    z_dispatcher:url_for(lib(Args, Context), url_for_args(P, Ext, Args, Context), Context).

lib(Args, Context) ->
    MinifyRequested = z_convert:to_bool( proplists:get_value(minify, Args, false))
               orelse m_config:get_boolean(site, minification_enabled, Context),
    IsLiveReload = m_config:get_boolean(mod_development, livereload, Context),
    IsNoCache = z_convert:to_bool( proplists:get_value(nocache, Args, false) ),
    IsMinify = MinifyRequested and not IsLiveReload,
    case IsNoCache of
        true when IsMinify ->
            lib_min_nocache;
        true ->
            lib_nocache;
        false ->
            case IsMinify of
                true -> lib_min;
                false -> lib
            end
    end.

tag1(Files, Args, Context) ->
    F = fun() ->
            {CssFiles, JsFiles} = split_css_js(Files),
            NoLangContext = z_context:set_language(undefined, Context),
            [link_element(CssFiles, Args, NoLangContext),
             script_element(JsFiles, Args, NoLangContext)]
    end,
    case z_module_manager:active(mod_development, Context) of
        false ->
            z_depcache:memo(F, {tag, Files, Args}, ?TAG_CACHE_TIME, Context);
        true ->
            F()
    end.


link_element([], _Args, _Context) ->
    [];
link_element(CssFiles, Args, Context) ->
    TitleAttr = case proplists:get_value(title, Args, undefined) of
           undefined -> [];
           TitleValue -> [<<" title=\"">>, TitleValue, $"]
           end,
    Media = proplists:get_value(media, Args, <<"all">>),
    Rel = proplists:get_value(rel, Args, <<"stylesheet">>),
    CssUrl = z_dispatcher:url_for(
        lib(Args, Context),
        url_for_args(CssFiles, <<".css">>, Args, Context),
        Context),
    case z_convert:to_bool( proplists:get_value(async, Args, false)) of
        true ->
            iolist_to_binary([
                <<"<link href=\"">>, CssUrl, <<"\" type=\"text/css\"">>,
                   TitleAttr,
                   <<" media=\"none\"">>,
                   <<" media-onload=\"">>, Media ,<<"\"">>,
                   <<" rel=\"">>, Rel, $",
                <<">">>,
                <<"<noscript>">>,
                    <<"<link href=\"">>, CssUrl, <<"\" type=\"text/css\"">>,
                       TitleAttr,
                       <<" media=\"">>, Media, $",
                       <<" rel=\"">>, Rel, $",
                    <<">">>,
                <<"</noscript>">>

            ]);
        false ->
            iolist_to_binary([
                <<"<link href=\"">>, CssUrl, <<"\" type=\"text/css\"">>,
                   TitleAttr,
                   <<" media=\"">>, Media, $",
                   <<" rel=\"">>, Rel, $",
                <<">">>
            ])
    end.

script_element([], _Args, _Context) ->
    [];
script_element(JsFiles, Args, Context) ->
    JsUrl = z_dispatcher:url_for(
        lib(Args, Context),
        url_for_args(JsFiles, <<".js">>, Args, Context),
        Context),
    AsyncDeferAttr = case z_convert:to_bool( proplists:get_value(async, Args, false)) of
        true -> <<" async defer ">>;
        false ->
            case z_convert:to_bool( proplists:get_value(defer, Args, false)) of
                true -> <<" defer ">>;
                false -> <<>>
            end
    end,
    iolist_to_binary([
            <<"<script src=\"">>, JsUrl, <<"\"">>,
                AsyncDeferAttr,
                <<" type=\"text/javascript\"">>,
            <<">">>,
            <<"</script>">>
        ]).

url_for_args(Files, Extension, Args, Context) ->
    AbsUrlArg = case proplists:get_value(absolute_url, Args, false) of
        false -> [];
        true -> [ absolute_url ]
    end,
    {Checksum, FoundFiles} = checksum(Files, Context),
    Sort = collapse_dirs(FoundFiles),
    Joined = lists:join(?SEP, Sort),
    <<$/,Path/binary>> = iolist_to_binary(Joined),
    [{star, iolist_to_binary([Path, ?SEP, integer_to_binary(Checksum), Extension])} | AbsUrlArg].


%% @doc Given the filepath of the request, return all files collapsed in the path.
-spec uncollapse( string() | binary() ) -> [ binary() ].
uncollapse(<<>>) ->
    [];
uncollapse(Path) when is_binary(Path) ->
    add_extension( uncollapse_dirs(binary:split(Path, <<?SEP>>, [global])) );
uncollapse(Path) ->
    uncollapse( z_convert:to_binary(Path) ).

add_extension([]) ->
    [];
add_extension([ File ]) ->
    [ File ];
add_extension([ TimestampExt | Files ]) ->
    Extension = filename:extension(TimestampExt),
    lists:foldl(
        fun(F, Acc) ->
            [ add_extension_1(F,Extension) | Acc ]
        end,
        [],
        Files).

add_extension_1(F, Ext) when is_binary(F) ->
    Ext1 = z_convert:to_binary(Ext),
    <<F/binary, Ext1/binary>>.

uncollapse_dirs([]) ->
    [];
uncollapse_dirs([ File | Rest ]) ->
    case filename:dirname(File) of
        <<X>> when X =:= $. orelse X =:= $/ ->
            uncollapse_dirs(Rest, <<>>, [File]);
        N ->
            uncollapse_dirs(Rest, N, [File])
    end.

uncollapse_dirs([], _Dirname, Acc) ->
    Acc;
uncollapse_dirs([ <<>> | Rest ], Dirname, Acc) ->
    uncollapse_dirs(Rest, Dirname, Acc);
uncollapse_dirs([ Rest ], _Dirname, Acc) ->
    [ Rest | Acc ];
uncollapse_dirs([ <<$/,_/binary>> = File | Rest ], _Dirname, Acc) ->
    uncollapse_dirs(Rest, filename:dirname(File), [ File | Acc ]);
uncollapse_dirs([ File | Rest ], Dirname, Acc) ->
    File1 = <<Dirname/binary, $/, File/binary>>,
    uncollapse_dirs(Rest, filename:dirname(File1), [ File1 | Acc ]).


%% @doc Try to remove directory names that are the same as the directory of the previous file in the list.
collapse_dirs([]) ->
    [];
collapse_dirs([ File | Files ]) ->
    collapse_dirs(
        Files,
        binary:split( dirname( z_convert:to_binary(File)), <<"/">>, [global] ),
        [ ensure_abspath(filename:rootname(File)) ]).

collapse_dirs([], _PrevTk, Acc) ->
    lists:reverse(Acc);
collapse_dirs([ File | Files ], PrevTk, Acc) ->
    FileTk = binary:split( dirname(File), <<"/">>, [global] ),
    case drop_prefix(PrevTk, FileTk) of
        {[], []} ->
            % File is in the same directory
            collapse_dirs(Files, FileTk, [filename:rootname(filename:basename(File)) | Acc ]);
        {[], B} ->
            % File is in a subdirectory from A
            RelFile = [ lists:join($/, B), $/, filename:rootname(filename:basename(File))],
            collapse_dirs(Files, FileTk, [RelFile | Acc]);
        {_A, _B} ->
            % File is in a (sub-)directory higher from the previous one, reset to top level
            collapse_dirs(Files, FileTk, [ensure_abspath(filename:rootname(File)) | Acc ])
    end.

drop_prefix([A|RestA], [A|RestB]) ->
    drop_prefix(RestA, RestB);
drop_prefix(A, B) ->
    {A, B}.

dirname(F) ->
    case filename:dirname(F) of
        <<".">> -> <<>>;
        Dirname -> Dirname
    end.

ensure_abspath(<<$/, _/binary>> = File) ->
    File;
ensure_abspath(File) ->
    <<$/, File/binary>>.


%% @doc Calculate a checksum of the mod times of the list of files.
checksum(Files, Context) ->
    checksum(Files, [], {0, 0}, Context).

checksum([], FoundFiles, State, _Context) ->
    {fletcher_final(State), lists:reverse(FoundFiles)};
checksum([File|Files], FoundFiles, State, Context) ->
    case z_module_indexer:find(lib, File, Context) of
        {ok, #module_index{filepath=FilePath}} ->
            State1 = dt_checksum(last_modified(FilePath), State),
            checksum(Files, [ File | FoundFiles ], State1, Context);
        {error, enoent} ->
            case File of
                <<"lib/", File1/binary>> ->
                    case z_module_indexer:find(lib, File1, Context) of
                        {ok, #module_index{filepath=FilePath}} ->
                            State1 = dt_checksum(last_modified(FilePath), State),
                            checksum(Files, [ File1 | FoundFiles ], State1, Context);
                        {error, enoent} ->
                            %% Not found, skip the file
                            ?LOG_WARNING(#{
                                text => <<"Lib file not found">>,
                                file => File
                            }),
                            checksum(Files, [ File | FoundFiles ], State, Context)
                    end;
                _ ->
                    %% Not found, skip the file
                    ?LOG_WARNING(#{
                        text => <<"Lib file not found">>,
                        file => File
                    }),
                    checksum(Files, [ File | FoundFiles ], State, Context)
            end
    end.

last_modified(FilePath) ->
    case z_file_mtime:mtime(FilePath) of
        {ok, MTime} -> MTime;
        {error, notfound} -> {{0,0,0},{0,0,0}}
    end.

dt_checksum({{Year, Month, Day}, {Hour, Minute, Second}}, State) ->
    fletcher_sum(Year, fletcher_sum(Month, fletcher_sum(Day,
        fletcher_sum(Hour, fletcher_sum(Minute, fletcher_sum(Second, State)))))).

%% Calculation of Fletcher32 checksum
fletcher_final({Sum1, Sum2}) ->
    (Sum2 bsl 16) bor Sum1.

fletcher_sum(Val, {Sum1, Sum2}) ->
    Sum1_1 = (Sum1 + Val) rem 16#FFFF,
    {Sum1_1, (Sum2 + Sum1_1) rem 16#FFFF}.

%% @doc Split the list of files in js and css files, remove leading '/'
split_css_js(Files) ->
    split_css_js(Files, [], []).

split_css_js([], CssAcc, JsAcc) ->
    {lists:reverse(CssAcc), lists:reverse(JsAcc)};
split_css_js([<<$/,File/binary>>|Rest], CssAcc, JsAcc) ->
    split_css_js([File|Rest], CssAcc, JsAcc);
split_css_js([File|Rest], CssAcc, JsAcc) when is_binary(File) ->
    case filename:extension(File) of
        <<".css">> -> split_css_js(Rest, [File|CssAcc], JsAcc);
        <<".js">>  -> split_css_js(Rest, CssAcc, [File|JsAcc]);
        _ -> split_css_js(Rest, CssAcc, JsAcc)
    end;
split_css_js([File|Rest], CssAcc, JsAcc) ->
    FileB = z_convert:to_binary(File),
    split_css_js([FileB|Rest], CssAcc, JsAcc).

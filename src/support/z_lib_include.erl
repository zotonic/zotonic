%% @author Marc Worrell <marc@worrell.nl>
%% Date: 2009-07-22
%% @copyright (c) 2009 Marc Worrell
%% @doc Support for the {% lib filename ... %} tag in the templates.
%% Generates the <link /> or <script /> tag for css or js files.  Also
%% adds the greatest modification date so that updates are loaded by
%% the browser.

%% Copyright 2009 Marc Worrell
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
    tag/2,
    tag/3,
    uncollapse/1
]).

-define(SEP, $~).

%% @doc Generate the link and/or script tags for the given files.
tag(Files, Context) ->
    tag(Files, [], Context).

%% @doc Generate the link and/or script tags for the given files.
tag(Files, Args, Context) ->
    case m_config:get_value(mod_development, libsep, Context) of
        Empty when Empty == []; Empty == <<>>; Empty == undefined ->
            tag1(Files, Args, Context);
        _Other ->
            lists:foldr(fun(F, [Css,Js]) ->
                            [C,J] = tag1([F], Args, Context),
                            [[C|Css], [J|Js]]
                         end,
                         [[],[]],
                         Files)
    end.


tag1(Files, Args, Context) ->
    {Css, CssPath, Js, JsPath} = collapsed_paths(Files),
    UrlPrefix = case proplists:get_bool(use_absolute_url, Args) of
		    true -> [<<"http://">>, z_context:hostname_port(Context), <<"/lib">>];
		    false -> <<"/lib">>
    end,
    TitleAttr = case proplists:get_value(title, Args) of
		   undefined -> [];
		   TitleValue -> [<<" title=\"">>, TitleValue, $"]
	       end,
    MediaAttr = [<<" media=\"">>, proplists:get_value(media, Args, "all"), $"],
    RelAttr = [<<" rel=\"">>, proplists:get_value(rel, Args, "stylesheet"), $"],

    LinkElement = case CssPath of
        [] ->
            [];
        _ -> 
            ModCss = newest(Css, {{1970,1,1},{12,0,0}}, Context),
            iolist_to_binary([ <<"<link href=\"">>, UrlPrefix, CssPath, ?SEP, integer_to_list(ModCss), <<".css\" type=\"text/css\"">>, MediaAttr, TitleAttr, RelAttr, $/, $>])
    end,
    ScriptElement = case JsPath of
        [] ->
            [];
        _ -> 
            ModJs = newest(Js, {{1970,1,1},{12,0,0}}, Context),
            iolist_to_binary([ <<"<script src=\"">>, UrlPrefix, JsPath, ?SEP, integer_to_list(ModJs), <<".js\" type=\"text/javascript\"></script>">>])
    end,
    [LinkElement, ScriptElement].


%% @doc Make the collapsed paths for the js and the css files.
collapsed_paths(Files) ->
    {Css, Js} = split_css_js([ z_convert:to_list(F) || F <- Files]),
    CssSort = collapse_dirs(Css),
    JsSort= collapse_dirs(Js),
    CssPath = string:join(CssSort, [?SEP]),
    JsPath = string:join(JsSort, [?SEP]),
    {Css, CssPath, Js, JsPath}.
    

%% @doc Given the filepath of the request, return all files collapsed in the path.
%% @spec uncollapse(string()) -> list()
uncollapse(Path) ->
    Parts = string:tokens(Path, [?SEP]),
    case uncollapse_dirs(Parts) of
        [] -> 
            [];
        [File] ->
            [File];
        [TimestampExt | Files] ->
            Extension = filename:extension(TimestampExt),
            lists:foldl(fun(F, Acc) -> [F++Extension|Acc] end, [], Files)
    end.
    

    uncollapse_dirs([]) ->
        [];
    uncollapse_dirs([File|Rest]) ->
        case filename:dirname(File) of
            [X] when X =:= $. orelse X =:= $/ ->
                uncollapse_dirs(Rest, [], [File]);
            N ->
                uncollapse_dirs(Rest, N, [File])
        end.

        uncollapse_dirs([], _Dirname, Acc) ->
            Acc;
        uncollapse_dirs([[$/|_]=File|Rest], _Dirname, Acc) ->
            uncollapse_dirs(Rest, filename:dirname(File), [File|Acc]);
        uncollapse_dirs([File|Rest], Dirname, Acc) ->
            File1 = Dirname ++ [$/ | File],
            uncollapse_dirs(Rest, filename:dirname(File1), [File1|Acc]).


%% @doc Try to remove directory names that are the same as the directory of the previous file in the list.
collapse_dirs([]) ->
    [];
collapse_dirs([File|Files]) ->
    collapse_dirs(Files, string:tokens(dirname(File), "/"), [ensure_abspath(filename:rootname(File))]).

    collapse_dirs([], _PrevTk, Acc) ->
        lists:reverse(Acc);
    collapse_dirs([File|Files], PrevTk, Acc) ->
        FileTk = string:tokens(dirname(File), "/"),
        case drop_prefix(PrevTk, FileTk) of
            {[], []} ->
                % File is in the same directory
                collapse_dirs(Files, FileTk, [filename:rootname(filename:basename(File)) | Acc ]);
            {[], B} ->
                % File is in a subdirectory from A
                RelFile = string:join(B, "/") ++ [$/ | filename:rootname(filename:basename(File))],
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
            "." -> [];
            Dirname -> Dirname
        end.

    ensure_abspath([$/ | _] = File) ->
        File;
    ensure_abspath(File) ->
        [$/ | File].


%% @doc Get the newest mod time of the list of files.
newest([], Mod, _Context) ->
	[ModUTC|_] = calendar:local_time_to_universal_time_dst(Mod),
    calendar:datetime_to_gregorian_seconds(ModUTC);
newest([File|Files], Mod, Context) ->
    case z_module_indexer:find(lib, File, Context) of
        {ok, FilePath} ->
            FileMod = filelib:last_modified(FilePath),
            case FileMod > Mod of
                true -> newest(Files, FileMod, Context);
                false -> newest(Files, Mod, Context)
            end;
        {error, enoent} ->
            %% Not found, skip the file
            newest(Files, Mod, Context)
    end.


%% @doc Split the list of files in js and css files, remove leading '/'
split_css_js(Files) ->
    split_css_js(Files, [], []).
    
    split_css_js([], CssAcc, JsAcc) ->
        {lists:reverse(CssAcc), lists:reverse(JsAcc)};
    split_css_js([[$/|File]|Rest], CssAcc, JsAcc) ->
		split_css_js([File|Rest], CssAcc, JsAcc);
    split_css_js([File|Rest], CssAcc, JsAcc) ->
        case filename:extension(File) of
            ".css" -> split_css_js(Rest, [File|CssAcc], JsAcc);
            ".js"  -> split_css_js(Rest, CssAcc, [File|JsAcc]);
            _ -> split_css_js(Rest, CssAcc, JsAcc)
        end.

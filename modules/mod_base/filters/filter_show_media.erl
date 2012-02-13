%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc 'show_media' filter, show the media inserted with the html editor.

%% Copyright 2010 Arjan Scherpenisse
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

-module(filter_show_media).
-export([show_media/2]).

-include_lib("zotonic.hrl").


show_media(undefined, _Context) ->
    undefined;
show_media(Input, Context) when is_binary(Input) ->
    show_media1(Input, 0, Context);
show_media(Input, _Context) ->
    Input.


% scanning for media start marker
show_media1(Input, Index, Context) when is_binary(Input) ->
    case Input of
        <<Pre:Index/binary, "<!-- z-media ", Post/binary>> ->
            process_binary_match(Pre, <<>>, size(Post), show_media1_id(Post, 0, Context));
        <<_:Index/binary, _/binary>> ->
            show_media1(Input, Index + 1, Context);
        _ ->
            Input
    end.

% scanning for media id
show_media1_id(Input, Index, Context) ->
    case Input of
        <<Id:Index/binary, " -->", Post/binary>> ->
            Html = show_media_html(Id, Context),
            process_binary_match(<<>>, Html, size(Post), show_media1(Post, 0, Context));

        <<Id:Index/binary, " {", Post/binary>> ->
            process_binary_match(<<>>, <<>>, size(Post), show_media1_opts(Id, Post, 0, Context));
        <<_:Index/binary, _/binary>> ->
            show_media1_id(Input, Index + 1, Context);
        _ ->
            Input
    end.

% scanning for media id
show_media1_opts(Id, Input, Index, Context) ->
    case Input of
        <<Opts:Index/binary, "} -->", Post/binary>> ->
            Opts2 = mochijson:decode("{" ++ binary_to_list(Opts) ++ "}"),
            Html = show_media_html(Id, Opts2, Context),
            process_binary_match(<<>>, Html, size(Post), show_media1(Post, 0, Context));
        <<_:Index/binary, _/binary>> ->
            show_media1_opts(Id, Input, Index + 1, Context);
        _ ->
            Input
    end.



show_media_html(Id, Context) ->
    show_media_html(Id, {struct, []}, Context).

show_media_html(Id, {struct, Args}, Context) ->
    Args2 = [ {list_to_atom(A), B} || {A,B} <- Args],
    Args3 = filter_args(Args2, false, [], Context),
    Id1 = try 
              list_to_integer(z_convert:to_list(Id))
          catch
              _:_ -> Id
          end,
    z_template:render({cat, "_body_media.tpl"}, [ {id, Id1} | Args3 ++ z_context:get_all(Context) ], Context).

filter_args([], true, Acc, _Context) ->
    Acc;
filter_args([], false, Acc, Context) ->
    [_S,M,_L] = get_sizes(Context),
    [{size,M},{sizename,"medium"}|Acc];
filter_args([{size,Size}|Args], _, Acc, Context) ->
    [S,M,L] = get_sizes(Context),
    P = case Size of
            "large" -> L;
            "small" -> S;
            _ -> M
        end,
    filter_args(Args, true, [{size,P},{sizename,Size}|Acc], Context);
filter_args([{crop,"crop"}|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [{crop,true}|Acc], Context);
filter_args([{crop,_}|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [{crop,undefined}|Acc], Context);
filter_args([{link,"link"}|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [{link,true}|Acc], Context);
filter_args([{link,_}|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [{link,undefined}|Acc], Context);
filter_args([P|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [P|Acc], Context).



get_sizes(Context) ->
    Int = fun(S) -> z_convert:to_integer(z_string:trim(S)) end,
    %% Get sizes from site.media_dimensions
    WidthHeightString = z_convert:to_list(m_config:get_value(site, media_dimensions, "200x200,300x300,500x500", Context)),
    [   [{width, Int(W)}, {height, Int(H)}] ||
        [W,H] <- [string:tokens(P, "x") || P <- string:tokens(WidthHeightString, ",")] ].


process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.

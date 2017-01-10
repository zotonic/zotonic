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
-export([show_media/2, show_media/3]).

-include_lib("zotonic.hrl").

show_media(Input, Context) ->
    show_media(Input, "_body_media.tpl", Context).

show_media(undefined, _Template, _Context) ->
    undefined;
show_media(Input, Template, Context) when is_binary(Input) ->
    Context1 = z_context:set(show_media_template, Template, Context),
    show_media1(Input, 0, Context1);
show_media(Input, _Template, _Context) ->
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
            Opts2 = mochijson:binary_decode(<<"{", Opts/binary, "}">>),
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
    Args2 = [ {to_atom(A), B} || {A,B} <- Args],
    Args3 = filter_args(Args2, false, [], Context),
    Id1 = try z_convert:to_integer(Id) catch _:_ -> z_html:escape(Id) end,
    Tpl = z_context:get(show_media_template, Context),
    z_template:render({cat, Tpl}, [ {id, Id1} | Args3 ++ z_context:get_all(Context) ], Context).

to_atom(<<"id">>) -> id;
to_atom(<<"size">>) -> size;
to_atom(<<"sizename">>) -> sizename;
to_atom(<<"caption">>) -> caption;
to_atom(<<"crop">>) -> crop;
to_atom(<<"link">>) -> link;
to_atom(<<"align">>) -> align;
to_atom(A) -> binary_to_existing_atom(A, 'utf8'). 

filter_args([], true, Acc, _Context) ->
    Acc;
filter_args([], false, Acc, Context) ->
    [_S,M,_L] = get_sizes(Context),
    [{size, M}, {sizename, <<"medium">>}|Acc];
filter_args([{size,Size}|Args], _, Acc, Context) ->
    [S,M,L] = get_sizes(Context),
    {P,SizeName} = case Size of
                        <<"large">> -> {L,<<"large">>};
                        <<"small">> -> {S,<<"small">>};
                        _ -> {M,<<"medium">>}
                   end,
    filter_args(Args, true, [{size,P},{sizename,SizeName}|Acc], Context);
filter_args([{crop, <<"crop">>}|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [{crop,true}|Acc], Context);
filter_args([{crop, _}|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [{crop,undefined}|Acc], Context);
filter_args([{link, <<"link">>}|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [{link,true}|Acc], Context);
filter_args([{link, _}|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [{link,undefined}|Acc], Context);
filter_args([{caption, Caption}|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [{caption,z_html:escape(Caption)}|Acc], Context);
filter_args([P|Args], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [P|Acc], Context).


get_sizes(Context) ->
    % Get sizes from site.media_dimensions
    WidthHeightString = z_convert:to_list(m_config:get_value(site, media_dimensions, "200x200,300x300,500x500", Context)),
    [   [{width, to_int(W)}, {height, to_int(H)}] ||
        [W,H] <- [string:tokens(P, "x") || P <- string:tokens(WidthHeightString, ",")] ].

to_int(S) -> z_convert:to_integer(z_string:trim(S)).

process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.

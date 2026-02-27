%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2023 Arjan Scherpenisse
%% @doc 'show_media' filter, show the media inserted with the html editor.
%% @end

%% Copyright 2010-2023 Arjan Scherpenisse
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
-moduledoc("
Convert the image markers in HTML from the Rich Text editor into image tags.

When you add images in the Rich Text editor of the Zotonic admin, the HTML body text does not store `<img\\>` tags, but
instead, special markers containing the picture id plus size and alignment hints.

The `show_media` tag converts these special markers (which are in fact HTML comments) back into image tags. For this, it
uses the template called `_body_media.tpl`. This template is located in `mod_base` and can be overruled with your own.

Images that are inlined in the body text can have these parameters:

`align`

Either `block`, `left` or `right`. Default is `block`.

`size`

Choose between `small`, `medium`, `large`. These are used for selecting the mediaclass of the image. Which will be like
`body-media-large`. The default size is `large`, displayed as a block (see align above).

`crop`

Checkbox if you want to force cropping of the image to the bounding box of the mediaclass. If checked then the crop in
the mediaclass is overruled. If not checked then the crop in the mediaclass is used.

`link`

Checkbox if you want to let the image include a link to its own page.

`link_url`

If `link` is checked then this can be used to set the URL for the link, the link url defaults to the page-url of the
shown medium item.

`caption`

Caption that will be displayed below the media item. Per default the summary of the media item is used for the caption.
If the caption is empty, then no caption is added.

These parameters can be set in the editor dialog ‘Media Properties’. This dialog is shown after clicking on an image
in the wysiwyg editor.





Template arguments
------------------

The `_body_media.tpl` is included using a *catinclude* for the media item. In this way you can switch templates
depending on the item category being displayed.

You can add your own `_body_media.tpl` templates. It will be supplied with the following data:

| Argument   | Description                                 |
| ---------- | ------------------------------------------- |
| id         | The id of the medium item to be displayed.  |
| size       | The size, `small`, `medium` or `large`      |
| mediaclass | Mediaclass, for example `body-media-large`  |
| align      | Alignment, `block`, `left` or `right`       |
| crop       | If crop is forced, `true` or `undefined`    |
| link       | If the image should link to the medium page |
| link\\\\_url | Link to use, defaults to the medium page    |
| caption    | The caption from the editor                 |

Besides the above all context variables are passed, this gives the Erlang code the possibility to change the behavior of
the media rendering.

See also

[embedded_media](/id/doc_template_filter_filter_embedded_media), [without_embedded_media](/id/doc_template_filter_filter_without_embedded_media), [media_for_language](/id/doc_template_filter_filter_media_for_language)").
-export([show_media/2, show_media/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

show_media(Input, Context) ->
    show_media(Input, "_body_media.tpl", Context).

show_media(undefined, _Template, _Context) ->
    undefined;
show_media(Input, Template, Context) when is_binary(Input) ->
    Context1 = z_context:set(show_media_template, Template, Context),
    show_media1(Input, 0, Context1);
show_media(#trans{} = Tr, Template, Context) ->
    Text = z_trans:lookup_fallback(Tr, Context),
    show_media(Text, Template, Context);
show_media(Input, _Template, _Context) ->
    Input.


% scanning for media start marker
% The z-media tag is very strict with spaces, this must stay so for the sanitizer.
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
            Opts2 = z_json:decode(<<"{", Opts/binary, "}">>),
            Html = show_media_html(Id, Opts2, Context),
            process_binary_match(<<>>, Html, size(Post), show_media1(Post, 0, Context));
        <<_:Index/binary, _/binary>> ->
            show_media1_opts(Id, Input, Index + 1, Context);
        _ ->
            Input
    end.

show_media_html(Id, Context) ->
    show_media_html(Id, #{}, Context).

show_media_html(Id, Args, Context) when is_map(Args) ->
    case m_rsc:rid(Id, Context) of
        undefined -> <<>>;
        RscId ->
            case z_acl:rsc_visible(RscId, Context) of
                true -> show_media_html_1(RscId, Args, Context);
                false -> <<>>
            end
    end.

show_media_html_1(Id, Args, Context) ->
    Args2 = [ {to_atom(A), B} || {A,B} <- maps:to_list(Args)],
    Args3 = filter_args(Args2, []),
    Args4 = filter_defaults(Args3),
    Tpl = z_context:get(show_media_template, Context),
    z_template:render({cat, Tpl}, [ {id, Id} | Args4 ++ z_context:get_all(Context) ], Context).

to_atom(<<"size">>) -> size;
to_atom(<<"caption">>) -> caption;
to_atom(<<"crop">>) -> crop;
to_atom(<<"link">>) -> link;
to_atom(<<"link_url">>) -> link_url;
to_atom(<<"link_new">>) -> link_new;
to_atom(<<"align">>) -> align;
to_atom(A) -> binary_to_existing_atom(A, 'utf8').

filter_defaults(Args) ->
    Args1 = case proplists:is_defined(size, Args) of
        false ->
            [   {mediaclass, <<"body-media-large">>},
                {size, <<"large">>}
                |Args
            ];
        true ->
            Args
    end,
    Args2 = case proplists:is_defined(align, Args1) of
        false -> [ {align, <<"middle">>} | Args1 ];
        true -> Args1
    end,
    case proplists:get_value(link, Args2, false) of
        false -> Args2;
        true ->
            LinkUrl = proplists:get_value(link_url, Args2),
            case z_utils:is_empty(LinkUrl) of
                true -> Args2;
                false ->
                    [
                        {link, LinkUrl}
                        | proplists:delete(link, Args2)
                    ]
            end
    end.

filter_args([], Acc) ->
    lists:reverse(Acc);
filter_args([{size, Size}|Args], Acc) ->
    SizeName = case Size of
        <<"large">> -> <<"large">>;
        <<"small">> -> <<"small">>;
        <<"medium">> -> <<"medium">>;
        <<"middle">> -> <<"medium">>;
        _ -> <<"large">>
    end,
    Acc1 = [
        {mediaclass,<<"body-media-",SizeName/binary>>},
        {size, SizeName}
        |Acc
    ],
    filter_args(Args, Acc1);
filter_args([{align, Align}|Args], Acc) ->
    Align1 = case Align of
        <<"left">> -> <<"left">>;
        <<"right">> -> <<"right">>;
        _ -> <<"block">>
    end,
    filter_args(Args, [{align, Align1}|Acc]);
filter_args([{crop, Crop}|Args], Acc) when Crop =:= true; Crop =:= <<"crop">> ->
    filter_args(Args, [{crop, true}|Acc]);
filter_args([{crop, _}|Args], Acc) ->
    filter_args(Args, Acc);
filter_args([{link, Link}|Args], Acc) when Link =:= true; Link =:= <<"link">> ->
    filter_args(Args, [{link,true}|Acc]);
filter_args([{link, _}|Args], Acc) ->
    filter_args(Args, Acc);
filter_args([{link_new, Link}|Args], Acc) when Link =:= true; Link =:= <<"new">> ->
    filter_args(Args, [{link_new,true}|Acc]);
filter_args([{link_new, _}|Args], Acc) ->
    filter_args(Args, [{link_new,false}|Acc]);
filter_args([{link_url, LinkUrl}|Args], Acc) ->
    filter_args(Args, [{link_url,z_html:escape_check(LinkUrl)}|Acc]);
filter_args([{caption, Caption}|Args], Acc) ->
    filter_args(Args, [{caption,z_html:escape_check(Caption)}|Acc]);
filter_args([{K, V}|Args], Acc) when is_binary(V) ->
    % Escape unknown arguments
    filter_args(Args, [{K,z_html:escape_check(V)}|Acc]);
filter_args([{_, V} = Arg|Args], Acc)
    when is_integer(V); is_float(V); is_boolean(V); V =:= undefined ->
    filter_args(Args, [Arg|Acc]);
filter_args([_|Args], Acc) ->
    % Drop complex data types
    filter_args(Args, Acc).

process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.

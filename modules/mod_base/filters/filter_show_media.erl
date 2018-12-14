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
-export([
    show_media/2,
    show_media/3
]).

-include_lib("zotonic.hrl").

show_media(Input, Context) ->
    show_media(Input, "_body_media.tpl", Context).

show_media(undefined, _Template, _Context) ->
    undefined;
show_media({trans, _} = Tr, Template, Context) ->
    Text = z_trans:lookup_fallback(Tr, Context),
    show_media(Text, Template, Context);
show_media(Input, Template, Context) when is_binary(Input) ->
    lists:reverse( show_media_1(Input, Template, [], Context) );
show_media(Input, _Template, _Context) ->
    Input.

show_media_1(Text, Template, Acc, Context) ->
    case find_marker(Text) of
        {Pre, Marker, Rest} ->
            Html = render_marker(Marker, Template, Context),
            Acc1 = [ Html, Pre | Acc ],
            show_media_1(Rest, Template, Acc1, Context);
        Text ->
            [ Text | Acc ]
    end.


-spec find_marker( binary() ) -> binary() | {binary(), binary(), binary()}.
find_marker(Text) ->
    case binary:match(Text, <<"<!-- z-media">>) of
        {Offset, Length} ->
            <<Pre:Offset/binary, _:Length/binary, MarkerRest/binary>> = Text,
            case binary:match(MarkerRest, <<" -->">>) of
                {EndOffset, EndLength} ->
                    <<Marker:EndOffset/binary, _:EndLength/binary, Rest/binary>> = MarkerRest,
                    {Pre, Marker, Rest};
                nomatch ->
                    Text
            end;
        nomatch ->
            Text
    end.

render_marker(Marker, Template, Context) ->
    case binary:match(Marker, <<" {">>) of
        {Offset, _} ->
            <<Id:Offset/binary, Args/binary>> = Marker,
            Id1 = binary:replace(Id, <<" ">>, <<>>, [global]),
            MediaId = m_rsc:rid(Id1, Context),
            {struct, Opts} = mochijson:binary_decode(Args),
            Opts1 = filter_args(Opts, Context),
            render_media(MediaId, Template, Opts1, Context);
        nomatch ->
            Id1 = binary:replace(Marker, <<" ">>, <<>>, [global]),
            MediaId = m_rsc:rid(Id1, Context),
            render_media(MediaId, Template, [], Context)
    end.

render_media(undefined, _Template, _Opts, _Context) ->
    <<>>;
render_media(MediaId, {cat, _} = Template, Opts, Context) ->
    case z_acl:rsc_visible(MediaId, Context) of
        true ->
            Opts1 = [ {id, MediaId} | Opts ] ++ z_context:get_all(Context),
            z_template:render(Template, Opts1, Context);
        false ->
            <<>>
    end;
render_media(MediaId, Template, Opts, Context) when is_binary(Template); is_list(Template) ->
    render_media(MediaId, {cat, Template}, Opts, Context);
render_media(_MediaId, _Template, _Opts, _Context) ->
    <<>>.


filter_args(Args, Context) ->
    Args1 = lists:filter( fun is_valid_arg/1, Args ),
    filter_args(Args1, false, [], Context).

% See _tinymce_dialog_zmedia_props.tpl
is_valid_arg({<<"link">>, _}) -> true;
is_valid_arg({<<"crop">>, _}) -> true;
is_valid_arg({<<"caption">>, _}) -> true;
is_valid_arg({<<"size">>, _}) -> true;
is_valid_arg({<<"align">>, <<"right">>}) -> true;
is_valid_arg({<<"align">>, <<"left">>}) -> true;
is_valid_arg({<<"align">>, <<"block">>}) -> true;
is_valid_arg(_) -> false.

filter_args([], true, Acc, _Context) ->
    Acc;
filter_args([], false, Acc, Context) ->
    [ _S, _M, L ] = get_sizes(Context),
    [ {size, L}, {sizename, <<"large">>} | Acc ];
filter_args([ {<<"size">>, Size} | Args ], _, Acc, Context) ->
    [ S, M, L ] = get_sizes(Context),
    {P, SizeName} = case Size of
                        <<"medium">> -> {M, <<"medium">>};
                        <<"small">> -> {S, <<"small">>};
                        _ -> {L, <<"large">>}
                   end,
    filter_args(Args, true, [ {size, P}, {sizename, SizeName} | Acc ], Context);
filter_args([ {<<"crop">>, <<"crop">>} | Args ], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [ {crop, true} | Acc], Context);
filter_args([ {crop, _} | Args ], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [ {<<"crop">>, undefined} | Acc ], Context);
filter_args([ {<<"link">>, <<"link">>} | Args ], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [ {link, true} | Acc ], Context);
filter_args([ {<<"link">>, _} | Args ], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [ {link, undefined} | Acc ], Context);
filter_args([ P | Args ], HasSize, Acc, Context) ->
    filter_args(Args, HasSize, [P|Acc], Context).


get_sizes(Context) ->
    Int = fun(S) -> z_convert:to_integer(z_string:trim(S)) end,
    %% Get sizes from site.media_dimensions
    WidthHeightString = z_convert:to_list(m_config:get_value(site, media_dimensions, "200x200,300x300,500x500", Context)),
    [   [{width, Int(W)}, {height, Int(H)}] ||
        [W,H] <- [string:tokens(P, "x") || P <- string:tokens(WidthHeightString, ",")] ].


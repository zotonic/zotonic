%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell <marc@worrell.nl>
%% Based on code Copyright (c) 2008-2009 Rusty Klophaus
%% Original author Tom McNulty <tom.mcnulty@cetiforge.com>
%% @doc Generate a HTML element with attributes, correctly escape all attributes.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-module (z_tags).

-include("zotonic.hrl").

-export([render_tag/2, render_tag/3, render_tag/4]).
-export([optional_escape_property/1]).

%% @doc Render a tag with properties, return the tag text.
-spec render_tag(TagName, Props) -> iodata() when
    TagName :: binary() | string(),
    Props :: [ Attribute ],
    Attribute :: {AttrName, AttrValue},
    AttrName :: binary() | atom() | string(),
    AttrValue :: string() | binary() | number() | atom() | undefined.
render_tag(TagName, Props) ->
    case is_self_closing(TagName) of
        true ->
            [$<, TagName, write_props(Props), $> ];
        false ->
            [$<, TagName, write_props(Props), "></", TagName, $> ]
    end.


%% @doc Render a tag into the context
-spec render_tag(TagName, Props, ContextOrContent) -> ContextOrContent1 when
    TagName :: binary() | string(),
    Props :: [ Attribute ],
    Attribute :: {AttrName, AttrValue},
    AttrName :: binary() | atom() | string(),
    AttrValue :: string() | binary() | number() | atom() | undefined,
    ContextOrContent :: z:context() | iodata() | undefined,
    ContextOrContent1 :: z:context() | iodata().
render_tag("div", Props, #context{} = Context) ->
    render_tag(<<"div">>, Props, Context);
render_tag(<<"div">>, Props, #context{} = Context) ->
	Render = [<<"<div ">>, write_props(Props), <<"></div>">> ],
	z_render:render(Render, Context);
render_tag(TagName, Props, #context{} = Context) ->
    Render = render_tag(TagName, Props),
	z_render:render(Render, Context);
render_tag(TagName, Props, undefined) ->
    render_tag(TagName, Props);
render_tag(TagName, Props, Content) ->
    case is_self_closing(TagName) of
        true ->
            [$<, TagName, write_props(Props), $>, Content ];
        false ->
            [$<, TagName, write_props(Props), $>, Content, $<, $/, TagName, $> ]
    end.

is_self_closing(<<"area">>) -> true;
is_self_closing(<<"base">>) -> true;
is_self_closing(<<"br">>) -> true;
is_self_closing(<<"col">>) -> true;
is_self_closing(<<"embed">>) -> true;
is_self_closing(<<"hr">>) -> true;
is_self_closing(<<"img">>) -> true;
is_self_closing(<<"input">>) -> true;
is_self_closing(<<"link">>) -> true;
is_self_closing(<<"meta">>) -> true;
is_self_closing(<<"param">>) -> true;
is_self_closing(<<"source">>) -> true;
is_self_closing(<<"track">>) -> true;
is_self_closing(<<"wbr">>) -> true;
is_self_closing("area") -> true;
is_self_closing("base") -> true;
is_self_closing("br") -> true;
is_self_closing("col") -> true;
is_self_closing("embed") -> true;
is_self_closing("hr") -> true;
is_self_closing("img") -> true;
is_self_closing("input") -> true;
is_self_closing("link") -> true;
is_self_closing("meta") -> true;
is_self_closing("param") -> true;
is_self_closing("source") -> true;
is_self_closing("track") -> true;
is_self_closing("wbr") -> true;
is_self_closing(_) -> false.

%%% Tags with child content %%%

%% @doc Render a tag, append it to the render_state of the Context.
-spec render_tag(TagName, Props, Content, Context) -> Context1 when
    TagName :: binary() | string(),
    Props :: [ Prop ],
    Prop :: {binary()|atom(), string()|binary()},
    Content :: iodata(),
    Context :: z:context(),
    Context1 :: z:context().
render_tag(TagName, Props, undefined, Context) ->
    Render = render_tag(TagName, Props),
    z_render:render(Render, Context);
render_tag(TagName, Props, Content, Context) ->
    Render = case is_self_closing(TagName) of
        true ->
            [$<, TagName, write_props(Props), $>, Content ];
        false ->
            [$<, TagName, write_props(Props), $>, Content, $<, $/, TagName, $> ]
    end,
	z_render:render(Render, Context).


%%% Property display functions %%%

write_props(Props) ->
    [ display_property(optional_escape_property(P)) || P <- Props ].

display_property({_Prop, undefined}) ->
    [];
display_property({Prop, V}) when is_atom(Prop) ->
    display_property({list_to_binary(atom_to_list(Prop)), V});
display_property({Prop, []}) when Prop =/= <<"alt">>, Prop =/= "alt" ->
    [];
display_property({Prop, <<>>}) when Prop =/= <<"alt">>, Prop =/= "alt" ->
    [];
display_property({Prop, Value}) when is_integer(Value) ->
    [32, correct_data(Prop), $=, $', list_to_binary(integer_to_list(Value)), $'];
display_property({Prop, Value}) when is_float(Value) ->
	[32, correct_data(Prop), $=, $', list_to_binary(io_lib:format("~f",[Value])), $'];
display_property({Prop, Value}) when is_list(Value) ->
    case io_lib:char_list(Value) of
        true ->
            [32, correct_data(Prop), $=, $', Value, $'];
        false ->
            [32, correct_data(Prop), $=, $', z_utils:join_defined(32, Value), $']
	end;
display_property({Prop, Value}) when is_atom(Value) ->
	[32, correct_data(Prop), $=, $', atom_to_list(Value), $'];
display_property({Prop, Value}) ->
	[32, correct_data(Prop), $=, $', Value, $'].


optional_escape_property({href, Uri}) -> {href, z_html:escape_check(Uri)};
optional_escape_property({src, Uri}) -> {src, z_html:escape_check(Uri)};
optional_escape_property({<<"href">>, Uri}) -> {<<"href">>, z_html:escape_check(Uri)};
optional_escape_property({<<"src">>, Uri}) -> {<<"src">>, z_html:escape_check(Uri)};
optional_escape_property(P) -> P.


% @doc Correct data_xxxx attributes, so that they are generated as data-xxxx
correct_data("data_"++P) -> "data-"++P;
correct_data(<<"data_",P/binary>>) -> <<"data-",P/binary>>;
correct_data(P) -> P.

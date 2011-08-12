%% @author Konstantin Nikiforov <helllamer@gmail.com>
%% @copyright 2010 Konstantin Nikiforov
%% @doc Render JS-aided inplace textbox.
%%	Example: {% inplace_textbox value="def.val." delegate="my_resource" hint="edit" %}

%% Copyright 2010 Konstantin Nikiforov
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


-module(scomp_base_inplace_textbox).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-define(NODISPLAY, {<<"style">>, "display:none;"}).
-define(NO_DATA_DEFAULT_TEXT, "Click to edit...").
-define(HINT_DEFAULT_TEXT, "").
-define(DEFAULT_VALUE, "").

-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    Delegate       = proplists:get_value(delegate, Params),
    Tag            = proplists:get_value(tag, Params, []),
    Value	   = proplists:get_value(value, Params, ?DEFAULT_VALUE),
    Class_input    = proplists:get_value(class_input, Params),
    Class_label    = proplists:get_value(class_label, Params),
    Class_hint     = proplists:get_value(class_hint,  Params),
    Hint_text      = proplists:get_value(hint_text,   Params, ?HINT_DEFAULT_TEXT),
    Hint_image     = render_image(proplists:get_value(hint_image,  Params)),
    Id             = z_ids:optid( proplists:get_value(id, Params) ),
    No_data_text   = proplists:get_value(no_data_text, Params, ?NO_DATA_DEFAULT_TEXT),
    %% "nodata" label is shown when strlen(trim(value)) == 0 (==value is empty, null or contains only invisible spaces)
    Class_no_data  = proplists:get_value(class_no_data, Params),

    %% hardcoded constants to get shorter ids and avoid possible ids overlapping with random ids
    Div_read_id   = Id ++ "-r",
    Div_edit_id   = Id ++ "-e",
    Div_nodata_id = Id ++ "-n",
    Input_id      = Id ++ "-i",
    
    Attrs_div_read = [
            {<<"id">>,          Div_read_id},
            {<<"onclick">>,     "inplace_textbox_read2write(\"" ++ Id ++ "\");"},
            {<<"onmouseover">>, "inplace_textbox_show_hint(\"" ++ Id ++ "\");" },
            {<<"onmouseout">>,  "inplace_textbox_hide_hint(\"" ++ Id ++ "\");" } ],
    Attrs_div_edit = [
            {<<"id">>, Div_edit_id},
            ?NODISPLAY ],

    %% div_read content
    Span_value_attr = [
            {<<"class">>, Class_label} ],
    Span_value = z_tags:render_tag(<<"span">>, Span_value_attr, Value),
    
    Span_hint_attr = [
                        {<<"class">>, Class_hint},
                        ?NODISPLAY,
                        {<<"id">>, "hint"} ],
    Span_hint  = z_tags:render_tag(<<"span">>, Span_hint_attr, [Hint_image, Hint_text]),

    Span_nodata_attr = [ {<<"class">>, Class_no_data} ],
    Span_nodata = z_tags:render_tag(<<"span">>, Span_nodata_attr, No_data_text),
    Attrs_div_nodata = [
            ?NODISPLAY,
            {<<"onclick">>,     "inplace_textbox_read2write(\"" ++ Id ++ "\");"},
            {<<"id">>, Div_nodata_id} ],
    
    %% div_write content
    Input_attrs = [
            {<<"id">>,      Input_id},
            {<<"type">>,    "text"},
            {<<"class">>,   Class_input},
            {<<"value">>,   Value},
            {<<"onblur">>,  "inplace_textbox_write2read(\"" ++ Id ++ "\");" },
            {<<"onkeyup">>, "if (event.keyCode == 13) { $(this).blur(); }" } ],
    Input = z_tags:render_tag(<<"input">>, Input_attrs),
    
    Inplace_textbox_inner_divs = [
            z_tags:render_tag(<<"span">>, Attrs_div_read, [Span_value, Span_hint]),
            z_tags:render_tag(<<"span">>, Attrs_div_edit, Input),
            z_tags:render_tag(<<"span">>, Attrs_div_nodata, Span_nodata) ],
    
    Inplace_textbox = z_tags:render_tag(<<"span">>, [{<<"id">>, Id}, {<<"class">>, "inplace_textbox"}], Inplace_textbox_inner_divs),
    Context1 = z_render:render(Inplace_textbox, Context),
    
    Postback = case Tag of 
            undefined -> [];
            _ ->         [{postback, Tag} ]
            end,
    Context2 = z_render:wire(Input_id, {event, [{type, blur}, {delegate, Delegate} | Postback]}, Context1),
    
    {ok, Context2}.


%% @doc  renders an image indicating the edit possibility.
%% @spec render_image(Input) -> string()
render_image(undefined) ->
    "";
render_image(Image_url) when is_list(Image_url) ->
    %% e.g. "/lib/images/edit.gif"
    "<img src=\"" ++ Image_url ++ "\"/>".


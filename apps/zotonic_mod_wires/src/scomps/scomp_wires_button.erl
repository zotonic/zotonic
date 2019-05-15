%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2013 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus

%% Copyright 2009-2013 Marc Worrell
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

-module(scomp_wires_button).
-behaviour(zotonic_scomp).

-export([vary/2, render/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

vary(_Params, _Context) -> nocache.
render(Params, _Vars, Context) ->
    Postback  = proplists:get_value(postback, Params),
	Delegate  = proplists:get_value(delegate, Params),
    Text      = proplists:get_value(text, Params, <<"Submit">>),
    Id        = iolist_to_binary(z_ids:optid(proplists:get_value(id, Params))),
    Class     = proplists:get_all_values(class, Params),
    Icon      = proplists:get_all_values(icon, Params),
    Style     = proplists:get_value(style, Params),
    TabIndex  = proplists:get_value(tabindex, Params),
    Type      = proplists:get_value(type, Params, "button"),
    Title     = proplists:get_value(title, Params),
    Disabled  = proplists:get_value(disabled, Params, false),
    Actions   = proplists:get_all_values(action, Params),
    Tag       = proplists:get_value(tag, Params, <<"button">>),

    Class1 = case Class of
        [] -> "btn btn-default";
        _ -> Class
    end,

    Options   = [{action,X} || X <- Actions],
    Options1  = case Postback of
                	undefined -> Options;
                	Postback  -> [{postback,Postback} | Options]
                end,

    Context1 = case Options1 of
                    [] -> Context;
                    _  ->
                       Options2  = case Delegate of
                                       undefined -> Options1;
				       _ -> [{delegate, Delegate} | Options1]
                                   end,
                        Options3  = [ {qarg,X} || {qarg,X} <- Params ] ++ Options2,
                        z_render:wire(Id, {event,[{type,click}|Options3]}, Context)
               end,

    Attrs = [
        {<<"id">>,    Id},
        {<<"name">>,  case proplists:is_defined(id, Params) of true -> Id; false -> "" end},
        {<<"style">>, Style},
        {<<"title">>, Title},
        {<<"tabindex">>, TabIndex}
    ],

    {Class2, Attrs1} = case z_convert:to_bool(Disabled) of
        false -> {Class1, Attrs};
        true -> { ["disabled"|Class1], [ {<<"disabled">>,"disabled"}|Attrs] }
    end,

    Attrs2 = case Type of
        undefined -> Attrs1;
        _ -> [ {<<"type">>, Type} | Attrs1 ]
    end,

    Text1 = case z_utils:is_empty(Icon) of
                true -> Text;
                false ->
                    [z_tags:render_tag(<<"i">>, [{class, Icon}], ""),
                     " ", Text]
            end,
    Context2 = z_tags:render_tag(
                        Tag,
                        [{<<"class">>,Class2}|Attrs2],
                    	Text1,
                    	Context1),
    {ok, Context2}.


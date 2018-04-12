%% This is the MIT license.
%%
%% Copyright (c) 2008-2009 Rusty Klophaus
%% Copyright (c) 2009-2018 Marc Worrell
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is furnished to do
%% so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
%% INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
%% PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
%% OR OTHER DEALINGS IN THE SOFTWARE.

-module(z_script).

-export ([
    combine/2,
    merge/2,
    copy/2,

    clean/1,

    add_content_script/2,
    add_script/2,
    get_script/2
    % get_page_startup_script/1
    % get_stream_start_script/1,
]).


-include_lib("zotonic.hrl").


%% @doc Merge the scripts and the rendered content of two contexts into Context1
-spec combine(#render_state{}, #render_state{}) -> #render_state{}.
combine(C1, C2) ->
    Merged = merge(C2, C1),
    Merged#render_state{
        render = combine1(C1#render_state.render, C2#render_state.render)
    }.

%% @doc Merge the scripts from context C into the context accumulator, used when collecting all scripts in an output stream
-spec merge(#render_state{}, #render_state{}) -> #render_state{}.
merge(C, Acc) ->
    Acc#render_state{
        updates = combine1(Acc#render_state.updates, C#render_state.updates),
        actions = combine1(Acc#render_state.actions, C#render_state.actions),
        content_scripts = combine1(Acc#render_state.content_scripts, C#render_state.content_scripts),
        scripts = combine1(Acc#render_state.scripts, C#render_state.scripts),
        wire = combine1(Acc#render_state.wire, C#render_state.wire),
        validators = combine1(Acc#render_state.validators, C#render_state.validators)
    }.

combine1([],X) -> X;
combine1(X,[]) -> X;
combine1(X,Y) -> [X,Y].

%% @doc Overwrite the scripts in Context with the scripts in From
-spec copy(#render_state{}, #render_state{}) -> #render_state{}.
copy(From, To) ->
    To#render_state{
        updates = From#render_state.updates,
        actions = From#render_state.actions,
        content_scripts = From#render_state.content_scripts,
        scripts = From#render_state.scripts,
        wire = From#render_state.wire,
        validators = From#render_state.validators
    }.



add_content_script([], RenderState) -> RenderState;
add_content_script(<<>>, RenderState) -> RenderState;
add_content_script(undefined, RenderState) -> RenderState;
add_content_script(Script, RenderState) ->
    RenderState#render_state{
        content_scripts = [ Script, "\n" | RenderState#render_state.content_scripts ]
    }.

add_script([], RenderState) -> RenderState;
add_script(<<>>, RenderState) -> RenderState;
add_script(undefined, RenderState) -> RenderState;
add_script(Script, RenderState) ->
    RenderState#render_state{
        scripts = [ Script, "\n" | RenderState#render_state.scripts
    ]}.

% str_user_id(undefined) -> <<"undefined">>;
% str_user_id(UserId) -> [ $", z_convert:to_binary(UserId), $" ].

%% @doc Remove all scripts from the context, resetting it back to a clean sheet.
-spec clean(#render_state{}) -> #render_state{}.
clean(RenderState) ->
    RenderState#render_state{
        scripts = [],
        content_scripts = [],
        updates = [],
        actions = [],
        validators = []
    }.

%% @doc Collect all scripts in the context, returns an iolist with javascript.
-spec get_script( #render_state{}, z:context() ) -> iolist().
get_script(RenderState, Context) ->
    % Translate updates to content scripts
    Update2Script = fun({TargetId, Terms, JSFormatString}, C) ->
                            {Html,C1} = z_render:render_to_iolist(Terms, C),
                            Script    = io_lib:format(JSFormatString, [TargetId, z_utils:js_escape(Html)]),
                            add_content_script(Script, C1);
                        ({Script}, C) ->
                            add_content_script(Script, C)
                    end,
    RS2 = lists:foldl( Update2Script,
                       RenderState#render_state{ updates=[], scripts=[], content_scripts=[] },
                       lists:flatten(RenderState#render_state.updates)),

    % Translate actions to scripts
    Action2Script = fun({TriggerID, TargetID, Actions}, C) ->
                        {Script,C1} = z_render:render_actions(TriggerID, TargetID, Actions, C),
                        add_script(Script, C1)
                    end,
    RS3 = lists:foldl(Action2Script, RS2#render_state{actions=[]}, lists:flatten(RS2#render_state.actions)),

    % Translate validators to scripts
    Validator2Script = fun({TriggerId, TargetId, Validator}, C) ->
                            {Script,C1} = z_render:render_validator(TriggerId, TargetId, Validator, C),
                            add_script(Script, C1)
                       end,
    RS4 = lists:foldl(Validator2Script, RS3#render_state{validators=[]}, lists:flatten(RS3#render_state.validators)),

    case {RS4#render_state.updates, RS4#render_state.actions, RS4#render_state.validators} of
        {[],[],[]} ->
            % Final, we have rendered all content
            [
                lists:reverse(RenderState#render_state.content_scripts),
                lists:reverse(RenderState#render_state.scripts),
                lists:reverse(RS4#render_state.content_scripts),
                lists:reverse(RS4#render_state.scripts)
            ];
        _NonEmpty ->
            % Recurse, as the rendering delivered new scripts and/or content
            [
                lists:reverse(RenderState#render_state.content_scripts),
                lists:reverse(RenderState#render_state.scripts),
                get_script(RS4, Context)
            ]
    end.

% get_stream_start_script(Context) ->
%     get_stream_start_script(z_context:has_websockethost(Context), Context).

% % Make the call of the start script.
% get_stream_start_script(false, _Context) ->
%     [<<"z_stream_start();">>];
% get_stream_start_script(true, Context) ->
%     [<<"z_stream_start('">>, z_context:websockethost(Context), <<"');">>].

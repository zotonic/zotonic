%% This is the MIT license.
%% 
%% Copyright (c) 2008-2009 Rusty Klophaus
%% Copyright (c) 2009-2012 Marc Worrell
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
-include("zotonic.hrl").
-export ([
    split/1,
    add_script/2,
    get_script/1,
    javascript_ast/2,
    get_page_startup_script/1,
    add_content_script/2,
    clean/1
]).


%% @doc Split the scripts from the context. Returns the scripts and a cleaned context.
split(Context) ->
    {iolist_to_binary(get_script(Context)), clean(Context)}.

%% @doc Render a javascript ast, injects a #context{} with a script into the output stream.
-spec javascript_ast(list() | binary(), #context{}) -> #context{}.
javascript_ast(Block, Context) when is_binary(Block) ->
    add_content_script(Block, z_context:new(Context));
javascript_ast(Block, Context) ->
    RenderContext = z_context:new(Context),
    {Script, RenderContext} = z_render:render_to_iolist(Block, z_context:new(Context)),
    add_content_script(Script, RenderContext).

add_content_script([], Context) -> 
    Context;
add_content_script(<<>>, Context) -> 
    Context;
add_content_script(Script, Context) ->
    Context#context{content_scripts=[Script, "\n" | Context#context.content_scripts]}.

add_script([], Context) -> 
    Context;
add_script(<<>>, Context) -> 
    Context;
add_script(Script, Context) ->
    Context#context{scripts=[Script, "\n" | Context#context.scripts]}.

get_page_startup_script(Context) ->
    case z_context:document_domain(Context) of
        undefined ->
            case Context#context.page_id of
                undefined ->
                    %% No page id, so no comet loop started and generated random page id for postback loop
                    [ ?SESSION_PAGE_Q, $=, $", z_ids:id(), $", $; ];
                PageId ->
                    [ ?SESSION_PAGE_Q, $=, $", PageId, $", $; ]
            end;
        DocumentDomain ->
            case Context#context.page_id of
                undefined ->
                    %% No page id, so no comet loop started and generated random page id for postback loop
                    [ ?SESSION_PAGE_Q, $=, $", z_ids:id(), <<"\"; document.domain=\"">>, DocumentDomain, $", $; ];
                PageId ->
                    [ ?SESSION_PAGE_Q, $=, $", PageId, <<"\"; document.domain=\"">>, DocumentDomain ,$", $; ]
            end
    end.

%% @doc Remove all scripts from the context, resetting it back to a clean sheet.
%% @spec clean(Context1) -> Context2
clean(Context) ->
    Context#context{scripts=[], content_scripts=[], updates=[], actions=[], validators=[]}.

%% @doc Collect all scripts in the context, returns an iolist with javascript.
%% @spec get_script(Context) -> iolist()    
get_script(Context) -> 
    get_script1(Context).
    
    get_script1(Context) ->
        % Translate updates to content scripts
        Update2Script = fun({TargetId, Terms, JSFormatString}, C) ->
                                {Html,C1} = z_render:render_to_iolist(Terms, C),
                                Script    = io_lib:format(JSFormatString, [TargetId, z_utils:js_escape(Html)]),
                                add_content_script(Script, C1);
                            ({Script}, C) ->
                                add_content_script(Script, C)
                        end,
        Context2 = lists:foldl( Update2Script, 
                                Context#context{updates=[], scripts=[], content_scripts=[]}, 
                                lists:flatten(Context#context.updates)),
        
        % Translate actions to scripts
        Action2Script = fun({TriggerID, TargetID, Actions}, C) ->
                            {Script,C1} = z_render:render_actions(TriggerID, TargetID, Actions, C),
                            add_script(Script, C1)
                        end,
        Context3 = lists:foldl(Action2Script, Context2#context{actions=[]}, lists:flatten(Context2#context.actions)),
    
        % Translate validators to scripts
        Validator2Script = fun({TriggerId, TargetId, Validator}, C) ->
                                {Script,C1} = z_render:render_validator(TriggerId, TargetId, Validator, C),
                                add_script(Script, C1)
                           end,
        Context4 = lists:foldl(Validator2Script, Context3#context{validators=[]}, lists:flatten(Context3#context.validators)),

        case {Context4#context.updates, Context4#context.actions, Context4#context.validators} of
            {[],[],[]} ->
                [   
                    lists:reverse(Context#context.content_scripts),
                    lists:reverse(Context#context.scripts),
                    lists:reverse(Context4#context.content_scripts),
                    lists:reverse(Context4#context.scripts)
                ];
            _NonEmpty ->
                [   
                    lists:reverse(Context#context.content_scripts),
                    lists:reverse(Context#context.scripts) | get_script1(Context4)
                ]
        end.
    

%% @author Marc Worrell <marc@worrell.nl>
%% @author Rusty Klophaus
%% @copyright 2009 Marc Worrell
%%
%% Based on Nitrogen, which is copyright (c) 2008-2009 Rusty Klophaus

%% This is the MIT license.
%%
%% Copyright (c) 2008-2009 Rusty Klophaus
%% Copyright (c) 2009 Marc Worrell
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


-module(z_render).
-author("Marc Worrell <marc@worrell.nl>").
-author("Rusty Klophaus").

-export ([
    output/3,

    render/3,
    render_actions/4,
    render_to_iolist/3,
    render_to_iolist/4,

    validator/4,
    render_validator/4,

    update/4,
    replace/4,
    insert_top/4,
    insert_bottom/4,
    insert_before/4,
    insert_after/4,

    update_iframe/4,

    appear/4,
    appear_replace/4,
    appear_top/4,
    appear_bottom/4,
    appear_before/4,
    appear_after/4,

    update_selector/4,
    replace_selector/4,
    insert_top_selector/4,
    insert_bottom_selector/4,
    insert_before_selector/4,
    insert_after_selector/4,

    appear_selector/4,
    appear_replace_selector/4,
    appear_top_selector/4,
    appear_bottom_selector/4,
    appear_before_selector/4,
    appear_after_selector/4,

    update_selector_js/2,
    replace_selector_js/2,
    insert_top_selector_js/2,
    insert_bottom_selector_js/2,
    insert_before_selector_js/2,
    insert_after_selector_js/2,

    appear_selector_js/2,
    appear_replace_selector_js/2,
    appear_top_selector_js/2,
    appear_bottom_selector_js/2,
    appear_before_selector_js/2,
    appear_after_selector_js/2,

    set_value/4,
    set_value_selector/4,

    css_selector/1,
    css_selector/2,
    quote_css_selector/1,
    render_css_selector/1,

    dialog/5,
    dialog_close/1,

    overlay/4,
    overlay_close/1,

    growl/2,
    growl_error/2,
    growl/4,

    make_postback/6,
    make_postback/7,
    make_postback_info/6,
    make_validation_postback/2,
    make_validation_postback/3,

    wire/2, wire/3, wire/4
]).


-include("../../include/zotonic.hrl").



%% @doc Replace the contexts in the output with their rendered content and collect all scripts
-spec output( term(), #render_state{}, z:context() ) -> { iolist(), #render_state{} }.
output(<<>>, RenderState, _Context) ->
    {[], RenderState};
output(B, RenderState, _Context) when is_binary(B) ->
    {B, RenderState};
output(List, RenderState, Context) ->
    output1(List, RenderState, Context, []).

%% @doc Recursively walk through the output, replacing all context placeholders with their rendered output
output1(B, RenderState, _Context, Acc) when is_binary(B) ->
    {[lists:reverse(Acc),B], RenderState};
output1([], RenderState, _Context, Acc) ->
    {lists:reverse(Acc), RenderState};
output1([#render_state{}=C|Rest], RenderState, Context, Acc) ->
    {Rendered, RS1} = output1(C#render_state.render, RenderState, Context, []),
    output1(Rest, z_script:merge(C, RS1), Context, [Rendered|Acc]);
output1([{script, Args}|Rest], RenderState, Context, Acc) ->
    output1(Rest, RenderState, Context, [render_script(Args, Context)|Acc]);
output1([List|Rest], RenderState, Context, Acc) when is_list(List) ->
    {Rendered, RS1} = output1(List, RenderState, Context, []),
    output1(Rest, RS1, Context, [Rendered|Acc]);
output1([undefined|Rest], RenderState, Context, Acc) ->
    output1(Rest, RenderState, Context, Acc);
output1([C|Rest], RenderState, Context, Acc) when is_atom(C) ->
    output1(Rest, RenderState, Context, [list_to_binary(atom_to_list(C))|Acc]);
output1([{trans, _} = Trans|Rest], RenderState, Context, Acc) ->
    output1(Rest, RenderState, Context, [z_trans:lookup_fallback(Trans, Context)|Acc]);
output1([{{_,_,_},{_,_,_}} = D|Rest], RenderState, Context, Acc) ->
    output1([filter_date:date(D, "Y-m-d H:i:s", Context)|Rest], RenderState, Context, Acc);
output1([{javascript, Script}|Rest], RenderState, Context, Acc) ->
    RS1 = RenderState#render_state{
        content_scripts = combine1(RenderState#render_state.content_scripts, [Script])
    },
    output1(Rest, RS1, Context, Acc);
output1([T|Rest], RenderState, Context, Acc) when is_tuple(T) ->
    output1([iolist_to_binary(io_lib:format("~p", [T]))|Rest], RenderState, Context, Acc);
output1([C|Rest], RenderState, Context, Acc) ->
    output1(Rest, RenderState, Context, [C|Acc]).

render_script(Args, Context) ->
    NoStartup = z_convert:to_bool(z_utils:get_value(nostartup, Args, false)),
    % NoStream = z_convert:to_bool(z_utils:get_value(nostream, Args, false)),
    Extra = [ S || S <- z_notifier:map(#scomp_script_render{is_nostartup=NoStartup, args=Args}, Context), S /= undefined ],
    Script = [ z_script:get_script(Context), Extra ],
    case z_utils:get_value(format, Args, <<"html">>) of
        <<"html">> ->
            [ <<"\n\n<script type='text/javascript'>\n$(function() {\n">>, Script, <<"\n});\n</script>\n">> ];
        <<"js">> ->
            [ $\n, Script, $\n ];
        <<"escapejs">> ->
            z_utils:js_escape(Script)
    end.

combine1([],X) -> X;
combine1(X,[]) -> X;
combine1(X,Y) -> [X,Y].





%% @doc Render adds output to the render field of the render-state, makes sure that the added output is an iolist
render(undefined, RenderState, _Context) ->
    RenderState;
render(<<>>, RenderState, _Context) ->
    RenderState;
render([], RenderState, _Context) ->
    RenderState;
render({script, _Args} = Script, RenderState, Context) ->
    %% Renders the script tag - might not be correct as it adds everything collected in Context and not what was collected
    %% in the added iolist().  So maybe we should just ignore the {script} tag here.
    %% When the script tag should be rendered then it is better to call z_context:output/2 instead of z_render:render/2.
    { Html, RS1 } = z_context:output([ Script ], Context),
    RenderState#render_state{ render = [ RS1#render_state.render, Html ]};
render(#render_state{} = RS, RenderState, _Context) ->
    z_script:merge(RS, RenderState);
render({javascript, Script}, RenderState, _Context) ->
    z_script:add_content_script(Script, RenderState);
render(B, RenderState, _Context) when is_binary(B) ->
    RenderState#render_state{ render = [ RenderState#render_state.render, B ] };
render(N, RenderState, _Context) when is_integer(N), N >= 0, N =< 255 ->
    RenderState#render_state{ render= [ RenderState#render_state.render, N ] };
render(N, RenderState, _Context) when is_integer(N) ->
    RenderState#render_state{ render = [ RenderState#render_state.render, z_convert:to_binary(N) ] };
render(A, RenderState, _Context) when is_atom(A) ->
    RenderState#render_state{ render = [ RenderState#render_state.render, atom_to_list(A) ] };
render(List=[H|_], RenderState, Context) when is_integer(H) orelse is_binary(H) ->
    %% Optimization for rendering lists of characters, aka strings
    F = fun (C) ->
            is_integer(C) orelse is_binary(C)
        end,
    {String,Rest} = lists:splitwith(F, List),
    RS1 = RenderState#render_state{ render = [ RenderState#render_state.render, String ] },
    render(Rest, RS1, Context);
render({trans, _} = Tr, RenderState, Context) ->
    render(z_trans:lookup_fallback(Tr, Context), RenderState, Context);
render({{_,_,_},{_,_,_}} = D, RenderState, Context) ->
    render(filter_stringify:stringify(D, Context), RenderState, Context);
render(F, RenderState, Context) when is_float(F) ->
    render(filter_stringify:stringify(F, Context), RenderState, Context);
render(T, RenderState, Context) when is_tuple(T) ->
    render(iolist_to_binary(io_lib:format("~p", [T])), RenderState, Context);
render([H|T], RenderState, Context) ->
    RS1 = render(H, RenderState, Context),
    render(T, RS1, Context).


%% @doc Render adds output to the render field of the context state. Do update the context for
%%      possible changes in scripts etc.
-spec render_to_iolist( Template::term(), #render_state{}, z:context() ) -> { iolist(), #render_state{} }.
render_to_iolist(Ts, RenderState, Context) ->
    RS1 = RenderState#render_state{ render = [] },
    RS2 = render(Ts, RS1, Context),
    { RS2#render_state.render, RS2#render_state{ render = RenderState#render_state.render }}.

-spec render_to_iolist( Template::term(), list(), #render_state{}, z:context() ) -> { iolist(), #render_state{} }.
render_to_iolist(Template, Vars, RenderState, Context) ->
    MixedHtml = z_template:render(Template, Vars, Context),
    render_to_iolist(MixedHtml, RenderState, Context).

%%% RENDER ACTIONS %%%

render_actions(_, _, undefined, Context) ->
    {[], Context};
render_actions(_, _, [], Context) ->
    {[], Context};
render_actions(TriggerId, TargetId, [H|T], Context) ->
    {Script1, Context1} = render_actions(TriggerId, TargetId, H, Context),
    {Script2, Context2} = render_actions(TriggerId, TargetId, T, Context1),
    {[Script1,Script2], Context2};
render_actions(TriggerId, TargetId, {Action, Args}, Context) ->
    case z_utils:is_true(proplists:get_value(show_if, Args, true)) of
        true ->
            Trigger = proplists:get_value(trigger, Args, TriggerId),
            Target = proplists:get_value(target,  Args, TargetId),
            case z_module_indexer:find(action, Action, Context) of
                {ok, #module_index{erlang_module=ActionModule}} ->
                    ActionModule:render_action(Trigger, Target, Args, Context);
                {error, enoent} ->
                    lager:info("No action enabled for \"~p\"", [Action]),
                    {[], Context}
            end;
        false ->
            {[],Context}
    end.


%% @spec validator(TriggerID::string(), TargetID::string(), Validator::#validator{}, Context::#context{}) -> #context{}
%% @doc Add an input validator to the list of known validators, used when rendering custom validators
validator(TriggerId, TargetId, Validator, RenderState) ->
    V = {TriggerId, TargetId, Validator},
    case lists:member(V, RenderState#render_state.validators) of
        true -> RenderState;
        false -> RenderState#render_state{ validators = [ V | RenderState#render_state.validators ]}
    end.


%% @doc Render a validator to the correct javascript.  Args are all arguments of the validator scomp.
%%      This renders an allocation of the initial validator and then appends all validations.
%%      'type' holds multiple validations.  Validations are of the form:  {validator, [Args]}
render_validator(TriggerId, TargetId, Args, Context) ->
    Validations = proplists:get_all_values(type, Args),
    Trigger     = proplists:get_value(trigger, Args, TriggerId),
    Target      = proplists:get_value(target,  Args, TargetId),
    Name        = proplists:get_value(name,  Args, Target),

    % The validator object, can have parameters for failureMessage.
    VldOptions  = z_utils:js_object(Args, [type,trigger,id,target], Context),
    VldScript   = [<<"z_init_validator(\"">>,Trigger,<<"\", ">>,VldOptions,<<");\n">>],

    % Now render and append all individual validations
    % The Postback contains all information to perform a server side validation
    % The Script is the script that ties the client side validation to the element
    RValidation = fun({VType,VArgs}, {PostbackAcc, ScriptAcc}) ->
                    VMod = case proplists:get_value(delegate, VArgs) of
                                undefined ->
                                    case z_module_indexer:find(validator, VType, Context) of
                                        {ok, #module_index{erlang_module=Mod}} ->
                                            {ok, Mod};
                                        {error, enoent} ->
                                            lager:info("No validator found for \"~p\"", [VType])
                                    end;
                                Delegate  ->
                                    {ok, Delegate}
                             end,
                     case VMod of
                        {ok, ValidatorModule} ->
                            ValidatorModuleAsAtom = z_convert:to_atom(ValidatorModule),
                            {VPostback, VScript} = ValidatorModuleAsAtom:render_validator(VType, Trigger, Target, VArgs, Context),
                            {[{VType,ValidatorModuleAsAtom,VPostback}|PostbackAcc],[VScript|ScriptAcc]};
                        _ ->
                            {PostbackAcc, ScriptAcc}
                     end
                 end,

    { Postback, Append } = lists:foldl(RValidation, {[], []}, Validations),
    case Postback of
        [] ->
            {[VldScript|Append]};
        _ ->
            Pickled  = z_utils:pickle({Trigger,Name,Postback}, Context),
            PbScript = [<<"z_set_validator_postback('">>,Trigger,<<"', '">>, Pickled, <<"');\n">>],
            {[PbScript,VldScript|Append]}
    end.



%%% AJAX UPDATES %%%

%% @doc Set the contents of an element to the the html fragment
-spec update(string(), #render{} | string(), #render_state{}, z:context()) -> z:context().
update(TargetId, Html, RenderState, Context) ->
    update_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Replace an element to the the html fragment
replace(TargetId, Html, RenderState, Context) ->
    replace_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Insert a html fragment at the top of the contents of an element
insert_top(TargetId, Html, RenderState, Context) ->
    insert_top_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Append a html fragment at the bottom of the contents of an element
insert_bottom(TargetId, Html, RenderState, Context) ->
    insert_bottom_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Add a html before the target element
insert_before(TargetId, Html, RenderState, Context) ->
    insert_before_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Add a html after the target element
insert_after(TargetId, Html, RenderState, Context) ->
    insert_after_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Set the contents of an element to the the html fragment
appear(TargetId, Html, RenderState, Context) ->
    appear_selector(css_selector(TargetId), Html, RenderState, Context).

appear_replace(TargetId, Html, RenderState, Context) ->
    appear_replace_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Insert a html fragment at the top of the contents of an element
appear_top(TargetId, Html, RenderState, Context) ->
    appear_top_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Append a html fragment at the bottom of the contents of an element
appear_bottom(TargetId, Html, RenderState, Context) ->
    appear_bottom_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Append a html fragment at the bottom of the contents of an element
appear_before(TargetId, Html, RenderState, Context) ->
    appear_before_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Add a html after the target element
appear_after(TargetId, Html, RenderState, Context) ->
    appear_after_selector(css_selector(TargetId), Html, RenderState, Context).

%% @doc Set the contents of an iframe to the generated html.
update_iframe(IFrameId, Html, RenderState, Context) ->
    {Html1, RS1} = render_html(Html, RenderState, Context),
    Update = [
        <<"z_update_iframe('">>,IFrameId,
        <<"','">>, z_utils:js_escape(Html1), <<"');">>
    ],
    RS1#render_state{ updates = [ {Update} | RS1#render_state.updates ] }.

%% @doc Set the contents of all elements matching the css selector to the the html fragment
update_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"html">>, <<".widgetManager()">>, RenderState, Context).

insert_before_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"insertBefore">>, <<".widgetManager()">>, RenderState, Context).

insert_after_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"insertAfter">>, <<".widgetManager()">>, RenderState, Context).

replace_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"replaceWith">>, <<".widgetManager()">>, RenderState, Context).

insert_top_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"prependTo">>, <<".widgetManager()">>, RenderState, Context).

insert_bottom_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"appendTo">>, <<".widgetManager()">>, RenderState, Context).

appear_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"html">>, <<".fadeIn().widgetManager()">>, RenderState, Context).

appear_replace_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"replaceWith">>, <<".fadeIn().widgetManager()">>, RenderState, Context).

appear_top_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"prependTo">>, <<".fadeIn().widgetManager()">>, RenderState, Context).

appear_bottom_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"appendTo">>, <<".fadeIn().widgetManager()">>, RenderState, Context).

appear_before_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"insertBefore">>, <<".fadeIn().widgetManager()">>, RenderState, Context).

appear_after_selector(CssSelector, Html, RenderState, Context) ->
    update_render_state(CssSelector, Html, <<"insertAfter">>, <<".fadeIn().widgetManager()">>, RenderState, Context).


%% @doc Set the value of an input element.
set_value(TargetId, Value, RenderState, Context) ->
    set_value_selector(css_selector(TargetId), Value, RenderState, Context).

set_value_selector(CssSelector, undefined, RenderState, Context) ->
    set_value_selector(CssSelector, "", RenderState, Context);
set_value_selector(CssSelector, Value, RenderState, Context) ->
    update_render_state(CssSelector, Value, <<"val">>, "", RenderState, Context).


%% @doc Render an update js as into the render state
update_render_state(CssSelector, Html, Function, AfterEffects, RenderState, Context) ->
    {Html1, RS1} = render_html(Html, RenderState, Context),
    Update = update_js(CssSelector, Html1, Function, AfterEffects),
    RS1#render_state{ updates = [ {Update} | RS1#render_state.updates ] }.


%% @doc Set the contents of all elements matching the css selector to the the html fragment
update_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"html">>, <<".widgetManager()">>).

replace_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"replaceWith">>, <<".widgetManager()">>).

insert_top_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"prependTo">>, <<".widgetManager()">>).

insert_bottom_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"appendTo">>, <<".widgetManager()">>).

insert_before_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"insertBefore">>, <<".widgetManager()">>).

insert_after_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"insertAfter">>, <<".widgetManager()">>).

appear_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"html">>, <<".fadeIn().widgetManager()">>).

appear_replace_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"replaceWith">>, <<".fadeIn().widgetManager()">>).

appear_top_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"prependTo">>, <<".fadeIn().widgetManager()">>).

appear_bottom_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"appendTo">>, <<".fadeIn().widgetManager()">>).

appear_before_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"insertBefore">>, <<".fadeIn().widgetManager()">>).

appear_after_selector_js(CssSelector, Html) ->
    update_js(CssSelector, Html, <<"insertAfter">>, <<".fadeIn().widgetManager()">>).


%% @doc Helper functions for the insert/appear/set_value functions
update_js(CssSelector, Html, <<"html">>, AfterEffects) ->
    update_js_selector_first(CssSelector, Html, <<"html">>, AfterEffects);
update_js(CssSelector, Html, <<"val">>, AfterEffects) ->
    update_js_selector_first(CssSelector, Html, <<"val">>, AfterEffects);
update_js(CssSelector, Html, <<"replaceWith">>, AfterEffects) ->
    update_js_selector_first(CssSelector, Html, <<"replaceWith">>, AfterEffects);
update_js(CssSelector, Html, Function, AfterEffects) ->
    [ <<"z_text_to_nodes(\"">>, z_utils:js_escape(Html), $", $),
      $., Function, $(, quote_css_selector(CssSelector), $),
      AfterEffects,
      $;].

update_js_selector_first(CssSelector, Html, Function, AfterEffects) ->
    [ $$, $(, quote_css_selector(CssSelector),
      <<").">>, Function, <<"(\"">>, z_utils:js_escape(Html), $", $),
      AfterEffects,
      $;].

render_html(#render{template=Template, is_all=All, vars=Vars}, RenderState, Context) ->
    render_html_opt_all(z_convert:to_bool(All), Template, Vars, RenderState, Context);
render_html(undefined, RenderState, _Context) ->
    {"", RenderState};
render_html(Html, RenderState, _Context) when is_binary(Html) ->
    {Html, RenderState};
render_html(Html, RenderState, Context) ->
    {Html1, RS1} = render_to_iolist(Html, RenderState, Context),
    {iolist_to_binary(Html1), RS1}.


render_html_opt_all(false, Template, Vars, RenderState, Context) ->
    MixedHtml = z_template:render(Template, Vars, Context),
    {Html, RS1} = render_to_iolist(MixedHtml, RenderState, Context),
    {iolist_to_binary(Html), RS1};
render_html_opt_all(true, Template, Vars, RenderState, Context) ->
    Templates = z_module_indexer:find_all(template, Template, RenderState, Context),
    Html = [ z_template:render(Tpl, Vars, RenderState, Context) || Tpl <- Templates ],
    render_html(Html, RenderState, Context).


%%% SIMPLE FUNCTION TO SHOW DIALOG OR GROWL (uses the dialog and growl actions) %%%

dialog(Title, Template, Vars, RenderState, Context) ->
    MixedHtml = z_template:render(Template, Vars, Context),
    {Html, RS1} = render_to_iolist(MixedHtml, RenderState, Context),
    Args = [{title, z_trans:lookup_fallback(Title, Context)}, {text, Html}],
    Args1 = case proplists:get_value(width, Vars) of
                undefined -> Args;
                Width -> [{width, Width} | Args]
            end,
    Args2 = case proplists:get_value(class, Vars) of
                undefined -> Args1;
                Class -> [{addclass, Class} | Args1]
            end,
    Args3 = case proplists:get_value(backdrop, Vars) of
                undefined -> Args2;
                Backdrop -> [{backdrop, Backdrop} | Args2]
            end,
    Args4 = case proplists:get_value(center, Vars) of
                undefined -> Args3;
                Center -> [{center, Center} | Args3]
            end,
    wire({dialog, Args4}, RS1).

dialog_close(RenderState) ->
    wire({dialog_close, []}, RenderState).

overlay(Template, Vars, RenderState, Context) ->
    MixedHtml = z_template:render(Template, Vars, Context),
    {Html, RS1} = render_to_iolist(MixedHtml, RenderState, Context),
    OverlayArgs = [
        {html, Html},
        {class, proplists:get_value(class, Vars, <<>>)}
    ],
    Script = [<<"z_dialog_overlay_open(">>, z_utils:js_object(OverlayArgs, Context), $), $; ],
    z_render:wire({script, [{script, Script}]}, RS1, Context).

overlay_close(RenderState) ->
    z_render:wire({overlay_close, []}, RenderState).

growl(Text, RenderState) ->
    z_render:wire({growl, [{text, Text}]}, RenderState).

growl_error(Text, RenderState) ->
    z_render:wire({growl, [{text, Text}, {type, "error"}]}, RenderState).

growl(Text, Type, Stay, RenderState) ->
    z_render:wire({growl, [{text, Text}, {type, Type}, {stay, Stay}]}, RenderState).



%%% POSTBACK ENCODING %%%

%% @doc Make an encoded string containing information which module and function to call.
make_postback_info(Tag, EventType, TriggerId, TargetId, Delegate, Context) ->
    Delegate1 = case Delegate of
                    undefined -> z_context:get_controller_module(Context);
                    _         -> z_convert:to_atom(Delegate)
                end,
    PostbackInfo = {EventType, TriggerId, TargetId, Tag, Delegate1},
    z_utils:pickle(PostbackInfo, Context).


%% @doc Make a javascript to call the postback, posting an encoded string containing callback information.
%% The PostbackTag is send to the server, EventType is normally the atom 'postback'.
%% @spec make_postback(PostbackTag, EventType, TriggerId, TargetId, Delegate, Context) -> {JavascriptString, PickledPostback}
make_postback(PostbackTag, EventType, TriggerId, TargetId, Delegate, Context) ->
    make_postback(PostbackTag, EventType, TriggerId, TargetId, Delegate, [], Context).

make_postback(undefined, _EventType, _TriggerId, _TargetId, _Delegate, _QArgs, _Context) ->
    {[],[]};
make_postback(PostbackTag, EventType, TriggerId, TargetId, Delegate, QArgs, Context) ->
    PickledPostbackInfo = make_postback_info(PostbackTag, EventType, TriggerId, TargetId, Delegate, Context),
    {ZEvtArgsPre, ZEvtArgs} = make_postback_zevtargs(QArgs),
    {[
        ZEvtArgsPre,
        <<"z_queue_postback(">>,
            postback_trigger_id(TriggerId),
            <<", '">>,PickledPostbackInfo,
            <<"', ">>, ZEvtArgs,
            <<");">>
     ],
     PickledPostbackInfo}.

postback_trigger_id(undefined) -> <<"undefined">>;
postback_trigger_id(A) when is_atom(A) -> [$', atom_to_list(A), $'];
postback_trigger_id([32|_CssSel]) -> <<"$(this).attr('id')">>;
postback_trigger_id(L) -> [$', L, $'].

make_postback_zevtargs([]) ->
    {<<>>, <<"typeof(zEvtArgs) != 'undefined' ? zEvtArgs : undefined">>};
make_postback_zevtargs(QArgs) when is_list(QArgs) ->
    {
        [
            <<"var zEvtQArgs = typeof(zEvtArgs) != 'undefined' ? zEvtArgs : [];">>,
            [
                begin
                    QArgB = z_convert:to_binary(QArg),
                    [<<"zEvtQArgs.push({name:$('#">>, QArgB, <<"').attr('name'), value:$('#">>,QArgB,<<"').val()});">>]
                end
                || QArg <- QArgs
            ]
        ],
        <<"zEvtQArgs">>
    }.

make_validation_postback(Validator, Context) ->
    make_validation_postback(Validator, {}, Context).
make_validation_postback(Validator, Args, Context) ->
    z_utils:pickle({Validator, Args}, Context).


%%% ACTION WIRING %%%

%% Add to the queue of wired actions. These will be rendered in get_script().

-spec wire(tuple() | [tuple()], #render_state{}) -> #render_state{}.
wire(Actions, RenderState) ->
    wire(<<>>, <<>>, Actions, RenderState).

wire(undefined, Actions, RenderState) ->
    wire(<<>>, <<>>, Actions, RenderState);
wire(TriggerId, Actions, RenderState) ->
    wire(TriggerId, TriggerId, Actions, RenderState).

wire(undefined, TargetId, Actions, RenderState) ->
    wire(<<>>, TargetId, Actions, RenderState);
wire(TriggerId, undefined, Actions, RenderState) ->
    wire(TriggerId, <<>>, Actions, RenderState);
wire(_TriggerId, _TargetId, [], RenderState) ->
    RenderState;
wire(TriggerId, TargetId, Actions, RenderState) ->
    RenderState#render_state{
        actions = [
            {TriggerId, TargetId, flatten_list(Actions)}
            | RenderState#render_state.actions
        ]}.

flatten_list(L) when is_list(L) ->
    lists:flatten(L);
flatten_list(Other) ->
    Other.


%% @doc Map a target id to a css selector
css_selector(TargetId) ->
    css_selector(TargetId, []).

%% @doc Map a target id to a css selector, check the Args proplist for additional selectors
css_selector(TargetId, Args) ->
    case proplists:get_value(selector, Args) of
        Empty when Empty =:= undefined; Empty =:= <<>>; Empty =:= "" ->
            css_selector_1(TargetId);
        <<"window">> -> window;
        "window" -> window;
        Selector -> Selector
    end.

css_selector_1(undefined) -> <<>>;
css_selector_1(<<>>) -> <<>>;
css_selector_1("") -> <<>>;
css_selector_1(window) -> window;
css_selector_1("window") -> window;
css_selector_1(<<"window">>) -> window;
css_selector_1(<<"#", _/binary>> = Sel) -> Sel;
css_selector_1(<<" ", _/binary>> = Sel) -> Sel;
css_selector_1(Sel) when is_list(Sel) -> css_selector_1(iolist_to_binary(Sel));
css_selector_1(Sel) -> <<"#", Sel/binary>>.


%% @doc Quote a css selector (assume no escaping needed...)
quote_css_selector(window) -> <<"window">>;
quote_css_selector(<<>>) -> <<>>;
quote_css_selector(<<$', _/binary>> = S) -> S;
quote_css_selector(<<$", _/binary>> = S) -> S;
quote_css_selector(<<$$, _/binary>> = S) -> S;
quote_css_selector([]) -> [];
quote_css_selector([$'|_] = S) -> S;
quote_css_selector([$"|_] = S) -> S;
quote_css_selector([$$|_] = S) -> S;
quote_css_selector(S) -> [$", S, $"].


%% @doc Render a css selector, allow direct expressions like
render_css_selector(Selector) ->
    case quote_css_selector(Selector) of
        <<$$, _/binary>> = Sel -> Sel;
        [$$|_] = Sel -> Sel;
        CssSel -> [$$, $(, CssSel, $)]
    end.


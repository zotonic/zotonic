%% @author Marc Worrell <marc@worrell.nl>
%% @author Rusty Klophaus
%% @copyright 2008-2009 Rusty Klophaus, 2009-2023 Marc Worrell
%% @doc Render routines using wires and actions.
%%      Based on Nitrogen, which is copyright (c) 2008-2009 Rusty Klophaus
%% @end

%% This is the MIT license.
%%
%% Copyright (c) 2008-2009 Rusty Klophaus
%% Copyright (c) 2009-2024 Marc Worrell
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
    render_html/2,
    output/2,

    render/2,
    render_actions/4,
    render_to_iolist/2,
    render_to_iolist/3,

    validator/4,
    render_validator/4,

    update/3,
    replace/3,
    insert_top/3,
    insert_bottom/3,
    insert_before/3,
    insert_after/3,

    update_iframe/3,

    appear/3,
    appear_replace/3,
    appear_top/3,
    appear_bottom/3,
    appear_before/3,
    appear_after/3,

    update_selector/3,
    replace_selector/3,
    insert_top_selector/3,
    insert_bottom_selector/3,
    insert_before_selector/3,
    insert_after_selector/3,

    appear_selector/3,
    appear_replace_selector/3,
    appear_top_selector/3,
    appear_bottom_selector/3,
    appear_before_selector/3,
    appear_after_selector/3,

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

    set_value/3,
    set_value_selector/3,

    css_selector/1,
    css_selector/2,
    quote_css_selector/1,
    render_css_selector/1,

    dialog/4,
    dialog_close/1,
    dialog_close/2,

    overlay/3,
    overlay_close/1,

    growl/2,
    growl_error/2,
    growl/4,

    make_postback/6,
    make_postback/7,
    make_postback_info/6,
    make_validation_postback/2,
    make_validation_postback/3,

    wire/2, wire/3, wire/4,

    % From script
    add_script/2,
    get_script/1,
    clean/1

    % script_combine/2,
    % script_merge/2,
    % script_copy/2,

    % add_content_script/2,
]).


-include_lib("../../include/zotonic.hrl").

%% The state below is the render state, can be cached and/or merged
%% State of the current rendered template/scomp/page
-record(render_state, {
        updates = []         :: list(),
        actions = []         :: list(),
        content_scripts = [] :: list(),
        scripts = []         :: list(),
        wire = []            :: list(),
        validators = []      :: list(),

        %% nested list with the accumulated html, xml or whatever output (mixed values)
        render = []          :: list()
    }).

-type html_element_id() :: binary() | string() | undefined.
-type render_state() :: #render_state{}.
-type ctx_rs() :: render_state() | z:context().
-type action() :: {atom(), proplists:proplist()}.

-export_type([
    render_state/0,
    html_element_id/0,
    ctx_rs/0,
    action/0
]).


%% @doc Replace the placeholders with their rendered content and collect all scripts from the mixed html and context.
%%      The result is a clean HTML tree and can be used for return by controllers.
-spec output( term(), z:context() ) -> { iolist(), z:context() }.
output(MixedHtml, Context) ->
    RS = get_render_state(Context),
    {Html, _RS1, Context1} = output(MixedHtml, RS, Context),
    {Html, reset_render_state(Context1)}.


%% @doc Replace the #render_state's in the output with their rendered content and collect all scripts
-spec output( term(), #render_state{}, z:context() ) -> { iolist(), #render_state{}, z:context() }.
output(<<>>, RenderState, Context) ->
    {[], RenderState, Context};
output(B, RenderState, Context) when is_binary(B) ->
    {B, RenderState, Context};
output(List, RenderState, Context) ->
    output1(List, RenderState, Context, []).

%% @doc Recursively walk through the output, replacing all context placeholders with their rendered output
output1(B, RenderState, Context, Acc) when is_binary(B) ->
    {[lists:reverse(Acc),B], RenderState, Context};
output1([], RenderState, Context, Acc) ->
    {lists:reverse(Acc), RenderState, Context};
output1([#context{} = C|Rest], RenderState, Context, Acc) ->
    output1([z_context:get_render_state(C)|Rest], RenderState, Context, Acc);
output1([#render_state{}=RS0|Rest], RenderState, Context, Acc) ->
    {Rendered, RS1, Context1} = output1(RS0#render_state.render, RenderState, Context, []),
    output1(Rest, merge_scripts(RS0, RS1), Context1, [Rendered|Acc]);
output1([{script, Args}|Rest], RenderState, Context, Acc) ->
    Context1 = set_render_state(RenderState, Context),
    output1(Rest, #render_state{}, Context, [render_script(Args, Context1)|Acc]);
output1([ [ {_, _} | _ ] = List | Rest ], RenderState, Context, Acc) ->
    JSON = z_json:encode(List),
    output1(Rest, RenderState, Context, [JSON|Acc]);
output1([List|Rest], RenderState, Context, Acc) when is_list(List) ->
    {Rendered, RS1, Context1} = output1(List, RenderState, Context, []),
    output1(Rest, RS1, Context1, [Rendered|Acc]);
output1([undefined|Rest], RenderState, Context, Acc) ->
    output1(Rest, RenderState, Context, Acc);
output1([C|Rest], RenderState, Context, Acc) when is_atom(C) ->
    output1(Rest, RenderState, Context, [list_to_binary(atom_to_list(C))|Acc]);
output1([#trans{} = Trans|Rest], RenderState, Context, Acc) ->
    output1(Rest, RenderState, Context, [z_trans:lookup_fallback(Trans, Context)|Acc]);
output1([{{_,_,_},{_,_,_}} = D|Rest], RenderState, Context, Acc) ->
    output1([filter_date:date(D, "Y-m-d H:i:s", Context)|Rest], RenderState, Context, Acc);
output1([{javascript, Script}|Rest], RenderState, Context, Acc) ->
    RS1 = RenderState#render_state{
        content_scripts = combine1(RenderState#render_state.content_scripts, [Script])
    },
    output1(Rest, RS1, Context, Acc);
output1([T|Rest], RenderState, Context, Acc) when is_tuple(T) ->
    Printed = iolist_to_binary(io_lib:format("~p", [T])),
    output1(Rest, RenderState, Context, [Printed|Acc]);
output1([M|Rest], RenderState, Context, Acc) when is_map(M) ->
    JSON = z_json:encode(M),
    output1(Rest, RenderState, Context, [JSON|Acc]);
output1([C|Rest], RenderState, Context, Acc) ->
    output1(Rest, RenderState, Context, [C|Acc]).


%% @doc Make the script that is included in the page and initializes all wires
-spec render_script(TagArgs, Context) -> Script when
    TagArgs :: proplists:proplist(),
    Context :: z:context(),
    Script :: iodata().
render_script(Args, Context) ->
    NoStartup = z_convert:to_bool(z_utils:get_value(nostartup, Args, false)),
    % NoStream = z_convert:to_bool(z_utils:get_value(nostream, Args, false)),
    Extra = [ S || S <-
        z_notifier:map(
            #scomp_script_render{
                is_nostartup = NoStartup,
                args = Args
            }, Context),
        S =/= undefined
    ],
    Script = [ get_script(Context), Extra ],
    case z_utils:get_value(format, Args, <<"html">>) of
        <<"html">> ->
            CspNonce = z_context:csp_nonce(Context),
            [
                <<"\n\n<script type='text/javascript' nonce='">>, CspNonce, <<"'>\n">>,
                <<"z_script_nonce = \'">>, z_utils:js_escape(CspNonce), "';\n",
                <<"window.zotonicPageInit = function() {\n">>,
                        Script,
                <<"\n};\n</script>\n">>
            ];
        <<"js">> ->
            [ $\n, Script, $\n ];
        <<"escapejs">> ->
            z_utils:js_escape(Script)
    end.

combine1([],X) -> X;
combine1(X,[]) -> X;
combine1(X,Y) -> [X,Y].



%% @doc Render adds output to the render-state of the context. Makes sure that the added output is an iolist
%%      The stored render state is later used in output/2
-spec render( term(), z:context() ) -> z:context().
render(Mixed, Context) ->
    RS = get_render_state(Context),
    RS1 = append_render_state(Mixed, RS, Context),
    set_render_state(RS1, Context).

append_render_state(undefined, RenderState, _Context) ->
    RenderState;
append_render_state(<<>>, RenderState, _Context) ->
    RenderState;
append_render_state([], RenderState, _Context) ->
    RenderState;
append_render_state({script, _Args}, RenderState, _Context) ->
    %% Renders the script tag - this won't be correct as it adds everything collected in Context and not what was collected
    %% in the added iolist().  So it is better to ignore the {script} tag here.
    %% If the script tag should be rendered then it is better to call output/2 instead of render/2.
    RenderState;
append_render_state(#render_state{} = RS, RenderState, _Context) ->
    merge_scripts(RS, RenderState);
append_render_state({javascript, Script}, RenderState, _Context) ->
    add_content_script(Script, RenderState);
append_render_state(B, RenderState, _Context) when is_binary(B) ->
    RenderState#render_state{ render = [ RenderState#render_state.render, B ] };
append_render_state(N, RenderState, _Context) when is_integer(N), N >= 0, N =< 255 ->
    RenderState#render_state{ render= [ RenderState#render_state.render, N ] };
append_render_state(N, RenderState, _Context) when is_integer(N) ->
    RenderState#render_state{ render = [ RenderState#render_state.render, z_convert:to_binary(N) ] };
append_render_state(A, RenderState, _Context) when is_atom(A) ->
    RenderState#render_state{ render = [ RenderState#render_state.render, atom_to_list(A) ] };
append_render_state(List=[H|_], RenderState, Context) when is_integer(H) orelse is_binary(H) ->
    %% Optimization for rendering lists of characters, aka strings
    F = fun (C) ->
            is_integer(C) orelse is_binary(C)
        end,
    {String,Rest} = lists:splitwith(F, List),
    RS1 = RenderState#render_state{ render = [ RenderState#render_state.render, String ] },
    append_render_state(Rest, RS1, Context);
append_render_state(#trans{} = Tr, RenderState, Context) ->
    append_render_state(z_trans:lookup_fallback(Tr, Context), RenderState, Context);
append_render_state({{_,_,_},{_,_,_}} = D, RenderState, Context) ->
    append_render_state(filter_stringify:stringify(D, Context), RenderState, Context);
append_render_state(F, RenderState, Context) when is_float(F) ->
    append_render_state(filter_stringify:stringify(F, Context), RenderState, Context);
append_render_state(T, RenderState, Context) when is_tuple(T) ->
    append_render_state(iolist_to_binary(io_lib:format("~p", [T])), RenderState, Context);
append_render_state([H|T], RenderState, Context) ->
    RS1 = append_render_state(H, RenderState, Context),
    append_render_state(T, RS1, Context).


%% @doc Render adds output to the render field of the context state. Do update the context for
%%      possible changes in scripts etc.
-spec render_to_iolist( MixedHtml::term(), z:context() ) -> { iolist(), z:context() }.
render_to_iolist(MixedHtml, #context{} = Context) ->
    RS = get_render_state(Context),
    RS1 = append_render_state(MixedHtml, RS#render_state{ render = [] }, Context),
    {
        RS1#render_state.render,
        set_render_state(RS1#render_state{ render = RS#render_state.render }, Context)
    }.

-spec render_to_iolist( Template::term(), list(), z:context() ) -> { iolist(), z:context() }.
render_to_iolist(Template, Vars, Context) ->
    MixedHtml = z_template:render(Template, Vars, Context),
    render_to_iolist(MixedHtml, Context).

%%% RENDER ACTIONS %%%

-spec render_actions( html_element_id(), html_element_id(), list(), z:context()) ->
    { iolist(), z:context() }.
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
                {ok, #module_index{ erlang_module = ActionModule }} ->
                    ActionModule:render_action(Trigger, Target, Args, Context);
                {error, enoent} ->
                    ?LOG_WARNING(#{
                        text => <<"Action not enabled">>,
                        in => zotonic_core,
                        result => error,
                        reason => enoent,
                        action => Action
                    }),
                    {[], Context}
            end;
        false ->
            {[],Context}
    end.


%% @spec validator(TriggerID::string(), TargetID::string(), Validator::#validator{}, Context::#context{}) -> #context{}
%% @doc Add an input validator to the list of known validators, used when rendering custom validators
validator(TriggerId, TargetId, Validator, Context) ->
    V = {TriggerId, TargetId, Validator},
    RenderState = get_render_state(Context),
    case lists:member(V, RenderState#render_state.validators) of
        true ->
            Context;
        false ->
            RS1= RenderState#render_state{ validators = [ V | RenderState#render_state.validators ]},
            z_context:set_render_state(RS1, Context)
    end.


%% @doc Render a validator to the correct javascript.  Args are all arguments of the validator scomp.
%%      This renders an allocation of the initial validator and then appends all validations.
%%      'type' holds multiple validations.  Validations are of the form:  {validator, [Args]}
-spec render_validator( undefined | binary(), undefined | binary(), list(), z:context() ) -> iolist().
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
                                        {ok, #module_index{ erlang_module = Mod }} ->
                                            {ok, Mod};
                                        {error, enoent} = Error ->
                                            ?LOG_WARNING(#{
                                                text => <<"Validator not found">>,
                                                in => zotonic_core,
                                                result => error,
                                                reason => enoent,
                                                validator => VType
                                            }),
                                            Error
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
            [VldScript|Append];
        _ ->
            Pickled  = z_crypto:pickle({Trigger,Name,Postback}, Context),
            PbScript = [<<"z_set_validator_postback('">>,Trigger,<<"', '">>, Pickled, <<"');\n">>],
            [PbScript,VldScript|Append]
    end.



%%% AJAX UPDATES %%%

%% @doc Set the contents of an element to the the html fragment
-spec update(string()|binary(), #render{} | iodata(), z:context()) -> z:context().
update(TargetId, Html, Context) ->
    update_selector(css_selector(TargetId), Html, Context).

%% @doc Replace an element to the the html fragment
replace(TargetId, Html, Context) ->
    replace_selector(css_selector(TargetId), Html, Context).

%% @doc Insert a html fragment at the top of the contents of an element
insert_top(TargetId, Html, Context) ->
    insert_top_selector(css_selector(TargetId), Html, Context).

%% @doc Append a html fragment at the bottom of the contents of an element
insert_bottom(TargetId, Html, Context) ->
    insert_bottom_selector(css_selector(TargetId), Html, Context).

%% @doc Add a html before the target element
insert_before(TargetId, Html, Context) ->
    insert_before_selector(css_selector(TargetId), Html, Context).

%% @doc Add a html after the target element
insert_after(TargetId, Html, Context) ->
    insert_after_selector(css_selector(TargetId), Html, Context).

%% @doc Set the contents of an element to the the html fragment
appear(TargetId, Html, Context) ->
    appear_selector(css_selector(TargetId), Html, Context).

appear_replace(TargetId, Html, Context) ->
    appear_replace_selector(css_selector(TargetId), Html, Context).

%% @doc Insert a html fragment at the top of the contents of an element
appear_top(TargetId, Html, Context) ->
    appear_top_selector(css_selector(TargetId), Html, Context).

%% @doc Append a html fragment at the bottom of the contents of an element
appear_bottom(TargetId, Html, Context) ->
    appear_bottom_selector(css_selector(TargetId), Html, Context).

%% @doc Append a html fragment at the bottom of the contents of an element
appear_before(TargetId, Html, Context) ->
    appear_before_selector(css_selector(TargetId), Html, Context).

%% @doc Add a html after the target element
appear_after(TargetId, Html, Context) ->
    appear_after_selector(css_selector(TargetId), Html, Context).

%% @doc Set the contents of an iframe to the generated html.
update_iframe(IFrameId, Html, Context) ->
    {Html1, Context1} = render_html(Html, Context),
    Update = [
        <<"z_update_iframe('">>,IFrameId,
        <<"','">>, z_utils:js_escape(Html1), <<"');">>
    ],
    RS = get_render_state(Context1),
    RS1 = RS#render_state{ updates = [ {Update} | RS#render_state.updates ] },
    set_render_state(RS1, Context1).

%% @doc Set the contents of all elements matching the css selector to the the html fragment
update_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"html">>, <<".widgetManager()">>, Context).

insert_before_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"insertBefore">>, <<".widgetManager()">>, Context).

insert_after_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"insertAfter">>, <<".widgetManager()">>, Context).

replace_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"replaceWith">>, <<".widgetManager()">>, Context).

insert_top_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"prependTo">>, <<".widgetManager()">>, Context).

insert_bottom_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"appendTo">>, <<".widgetManager()">>, Context).

appear_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"html">>, <<".fadeIn().widgetManager()">>, Context).

appear_replace_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"replaceWith">>, <<".fadeIn().widgetManager()">>, Context).

appear_top_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"prependTo">>, <<".fadeIn().widgetManager()">>, Context).

appear_bottom_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"appendTo">>, <<".fadeIn().widgetManager()">>, Context).

appear_before_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"insertBefore">>, <<".fadeIn().widgetManager()">>, Context).

appear_after_selector(CssSelector, Html, Context) ->
    update_render_state(CssSelector, Html, <<"insertAfter">>, <<".fadeIn().widgetManager()">>, Context).


%% @doc Set the value of an input element.
set_value(TargetId, Value, Context) ->
    set_value_selector(css_selector(TargetId), Value, Context).

set_value_selector(CssSelector, undefined, Context) ->
    set_value_selector(CssSelector, "", Context);
set_value_selector(CssSelector, Value, Context) ->
    update_render_state(CssSelector, Value, <<"val">>, "", Context).


%% @doc Render an update js as into the render state
update_render_state(CssSelector, Html, Function, AfterEffects, Context) ->
    {Html1, Context1} = render_html(Html, Context),
    Update = update_js(CssSelector, Html1, Function, AfterEffects),
    RS = get_render_state(Context1),
    RS1 = RS#render_state{ updates = [ {Update} | RS#render_state.updates ] },
    set_render_state(RS1, Context1).


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

render_html(#render{template=Template, is_all=All, vars=Vars}, Context) ->
    render_html_opt_all(z_convert:to_bool(All), Template, Vars, Context);
render_html(undefined, Context) ->
    {<<>>, Context};
render_html(Html, Context) when is_binary(Html) ->
    {Html, Context};
render_html(Html, Context) ->
    {Html1, Context1} = render_to_iolist(Html, Context),
    {sanitize_utf8(iolist_to_binary(Html1)), Context1}.


render_html_opt_all(false, Template, Vars, Context) ->
    MixedHtml = z_template:render(Template, Vars, Context),
    {Html, Context1} = render_to_iolist(MixedHtml, Context),
    {sanitize_utf8(iolist_to_binary(Html)), Context1};
render_html_opt_all(true, Template, Vars, Context) ->
    Templates = z_module_indexer:find_all(template, Template, Context),
    Html = [ z_template:render(Tpl, Vars, Context) || Tpl <- Templates ],
    render_html(Html, Context).

sanitize_utf8(B) ->
    case is_utf8(B) of
        true -> B;
        false -> z_string:sanitize_utf8(B)
    end.

is_utf8(<<>>) -> true;
is_utf8(<<_/utf8, S/binary>>) -> is_utf8(S);
is_utf8(_) -> false.


%%% SIMPLE FUNCTION TO SHOW DIALOG OR GROWL (uses the dialog and growl actions) %%%

dialog(undefined, Template, Vars, Context) ->
    dialog(<<>>, Template, Vars, Context);
dialog(Title, Template, Vars, Context) ->
    IsCatinclude = z_convert:to_bool(get_value(catinclude, Vars)),
    Template1 = case Template of
        {cat, _} -> Template;
        _ when IsCatinclude -> {cat, Template};
        _ -> Template
    end,
    MixedHtml = z_template:render(Template1, Vars, Context),
    {Html, Context1} = render_to_iolist(MixedHtml, Context),
    Args = [
        {title, z_trans:lookup_fallback(Title, Context1)},
        {text, Html}
    ],
    Args1 = case get_value(width, Vars) of
                undefined -> Args;
                Width -> [{width, Width} | Args]
            end,
    Args2 = case get_value(class, Vars) of
                undefined -> Args1;
                Class -> [{addclass, Class} | Args1]
            end,
    Args3 = case get_value(backdrop, Vars) of
                undefined -> Args2;
                "true" -> [{backdrop, true} | Args2 ];
                <<"true">> -> [{backdrop, true} | Args2 ];
                "false" -> [{backdrop, false} | Args2 ];
                <<"false">> -> [{backdrop, false} | Args2 ];
                static -> [ {backdrop, <<"static">>} | Args2 ];
                Backdrop -> [{backdrop, Backdrop} | Args2]
            end,
    Args4 = case get_value(center, Vars) of
                undefined -> Args3;
                Center -> [{center, Center} | Args3]
            end,
    Args5 = case get_value(level, Vars) of
                undefined -> Args4;
                Level -> [{level, Level} | Args4]
            end,
    wire({dialog, Args5}, Context1).

get_value(K, Map) when is_map(Map) ->
    maps:get(K, Map, undefined);
get_value(K, List) when is_list(List) ->
    proplists:get_value(K, List).

dialog_close(Context) ->
    wire({dialog_close, []}, Context).

dialog_close(Level, Context) ->
    wire({dialog_close, [{level, Level}]}, Context).

overlay(Template, Vars, Context) ->
    MixedHtml = z_template:render(Template, Vars, Context),
    {Html, Context1} = render_to_iolist(MixedHtml, Context),
    OverlayArgs = [
        {html, Html},
        {class, proplists:get_value(class, Vars, <<>>)},
        {level, proplists:get_value(level, Vars, 0)}
    ],
    Script = [<<"z_dialog_overlay_open(">>, z_utils:js_object(OverlayArgs, Context1), $), $; ],
    wire({script, [{script, Script}]}, Context1).

overlay_close(Context) ->
    wire({overlay_close, []}, Context).

growl(Text, Context) ->
    wire({growl, [{text, Text}]}, Context).

growl_error(Text, Context) ->
    wire({growl, [{text, Text}, {type, "error"}]}, Context).

growl(Text, Type, Stay, Context) ->
    wire({growl, [{text, Text}, {type, Type}, {stay, Stay}]}, Context).



%%% POSTBACK ENCODING %%%

%% @doc Make an encoded string containing information which module and function to call.
make_postback_info(Tag, EventType, TriggerId, TargetId, Delegate, Context) ->
    Delegate1 = case Delegate of
                    undefined -> z_context:get_controller_module(Context);
                    _         -> z_convert:to_atom(Delegate)
                end,
    PostbackInfo = {EventType, TriggerId, TargetId, Tag, Delegate1},
    z_crypto:pickle(PostbackInfo, Context).


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
    z_crypto:pickle({Validator, Args}, Context).


%%% ACTION WIRING %%%

%% Add to the queue of wired actions. These will be rendered in get_script().

-spec wire(action() | [action()], ctx_rs()) -> ctx_rs().
wire(Actions, Context) ->
    wire(<<>>, <<>>, Actions, Context).

wire(undefined, Actions, Context) ->
    wire(<<>>, <<>>, Actions, Context);
wire(TriggerId, Actions, Context) ->
    wire(TriggerId, TriggerId, Actions, Context).

wire(undefined, TargetId, Actions, Context) ->
    wire(<<>>, TargetId, Actions, Context);
wire(TriggerId, undefined, Actions, Context) ->
    wire(TriggerId, <<>>, Actions, Context);
wire(_TriggerId, _TargetId, [], Context) ->
    Context;
wire(TriggerId, TargetId, Actions, Context) ->
    RS = get_render_state(Context),
    RS1 = RS#render_state{
        actions = [
            {TriggerId, TargetId, flatten_list(Actions)}
            | RS#render_state.actions
        ]},
    set_render_state(RS1, Context).

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



get_render_state(undefined) -> #render_state{};
get_render_state(#render_state{} = RS) -> RS;
get_render_state(Context) ->
    case z_context:get_render_state(Context) of
        undefined -> #render_state{};
        #render_state{} = RS -> RS
    end.

set_render_state(RS, undefined) -> RS;
set_render_state(RS, #render_state{})  -> RS;
set_render_state(RS, Context)  ->
    z_context:set_render_state(RS, Context).

-spec reset_render_state( z:context() ) -> z:context().
reset_render_state(Context)  ->
    z_context:set_render_state(undefined, Context).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% scripts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Merge the scripts from context C into the context accumulator, used when collecting all scripts in an output stream
-spec merge_scripts(#render_state{}, #render_state{}) -> #render_state{}.
merge_scripts(RS, Acc) ->
    Acc#render_state{
        updates = combine1(Acc#render_state.updates, RS#render_state.updates),
        actions = combine1(Acc#render_state.actions, RS#render_state.actions),
        content_scripts = combine1(Acc#render_state.content_scripts, RS#render_state.content_scripts),
        scripts = combine1(Acc#render_state.scripts, RS#render_state.scripts),
        wire = combine1(Acc#render_state.wire, RS#render_state.wire),
        validators = combine1(Acc#render_state.validators, RS#render_state.validators),
        render= combine1(Acc#render_state.render, RS#render_state.render)
    }.

-spec add_content_script(Script, RenderState) -> NewRenderState when
    Script :: iodata() | undefined,
    RenderState :: #render_state{} | z:context(),
    NewRenderState :: #render_state{} | z:context().
add_content_script([], Context) -> Context;
add_content_script(<<>>, Context) -> Context;
add_content_script(undefined, Context) -> Context;
add_content_script(Script, Context) ->
    RS = get_render_state(Context),
    RS1 = RS#render_state{
        content_scripts = [ Script, "\n" | RS#render_state.content_scripts ]
    },
    set_render_state(RS1, Context).

-spec add_script(Script, Context) -> NewContext when
    Script :: iodata() | undefined,
    Context :: z:context(),
    NewContext :: z:context().
add_script([], Context) -> Context;
add_script(<<>>, Context) -> Context;
add_script(undefined, Context) -> Context;
add_script(Script, Context) ->
    RS = get_render_state(Context),
    RS1 = RS#render_state{
        scripts = [ Script, "\n" | RS#render_state.scripts
    ]},
    set_render_state(RS1, Context).

%% @doc Remove all scripts from the context, resetting it back to a clean sheet.
-spec clean( z:context() ) -> z:context().
clean(Context) ->
    z_context:set_render_state(undefined, Context).

%% @doc Collect all scripts in the context, returns an iolist with javascript.
-spec get_script( z:context() ) -> iolist().
get_script(#context{} = Context) ->
    RenderState = get_render_state(Context),
    RS1 = RenderState#render_state{
        scripts = [],
        content_scripts = []
    },
    % Translate updates to content scripts
    RS2 = lists:foldl(
        fun
            ({TargetId, MixedHtml, JSFormatString}, RSAcc) ->
                RSAcc1 = append_render_state(MixedHtml, RSAcc#render_state{ render = [] }, Context),
                RSAcc2 = RSAcc1#render_state{ },
                Script = io_lib:format(
                    JSFormatString,
                    [TargetId, z_utils:js_escape(RSAcc1#render_state.render)]),
                RSAcc2#render_state{
                    content_scripts = [ Script, "\n" | RSAcc2#render_state.content_scripts ],
                    render = RSAcc#render_state.render
                };
            ({Script}, RSAcc) ->
                RSAcc#render_state{
                    content_scripts = [ Script, "\n" | RSAcc#render_state.content_scripts ]
                }
        end,
        RS1#render_state{ updates = [] },
        lists:flatten(RenderState#render_state.updates)),

    % Translate actions to scripts
    RS3 = lists:foldl(
        fun
            ({TriggerID, TargetID, Actions}, RSAcc) ->
                C = set_render_state(RSAcc, Context),
                {Script3, C1} = render_actions(TriggerID, TargetID, Actions, C),
                RSAcc1 = get_render_state(C1),
                RSAcc1#render_state{
                    scripts = [ Script3, "\n" | RSAcc1#render_state.scripts ]
                }
        end,
        RS2#render_state{ actions = [] },
        lists:flatten(RS2#render_state.actions)),

    % Translate validators to scripts
    RS4 = lists:foldl(
        fun
            ({TriggerId, TargetId, Validator}, RSAcc) ->
                Cx = set_render_state(RSAcc, Context),
                ValidatorScript = render_validator(TriggerId, TargetId, Validator, Cx),
                RSAcc#render_state{
                    scripts = [ ValidatorScript, "\n" | RSAcc#render_state.scripts ]
                }
        end,
        RS3#render_state{ validators = [] },
        lists:flatten(RS3#render_state.validators)),

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
                get_script(set_render_state(RS4, Context))
            ]
    end.

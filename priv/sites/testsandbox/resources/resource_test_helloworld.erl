%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(resource_test_helloworld).
-export([event/2, periodic/2]).

-include_lib("resource_html.hrl").

html(Context) ->
    {Incr, Context2} = z_context:incr_session(helloworld_counter, 1, Context),
    Html = z_template:render("test_helloworld.tpl", [{helloworld_counter,Incr}], Context2),
    % Html = z_template:render("idtest.tpl", Context2),
    % _Pid = z_context:spawn_link_page(?MODULE, periodic, [], Context3),
    z_context:output(Html, Context2).


event(#postback{message=show_confirm}, Context) ->
    z_render:wire({confirm, [{text,"This is a Javascript Confirm"},{postback,confirm_ok}, {delegate, ?MODULE}]}, Context);
event(#postback{message=show_alert}, Context) ->
    z_render:wire({alert, [{text,"This is a Javascript Alert"}]}, Context);
event(#postback{message=confirm_ok}, Context) ->
    z_render:wire({alert, [{text,"You confirmed"}]}, Context);
event(#postback{message=show_growl}, Context) ->
    z_render:wire({growl, [{text,"This is a Growl Alert that disappears automatically after some time."}]}, Context);
event(#postback{message=show_growl_stay}, Context) ->
    z_render:wire({growl, [{text,"This is a Growl Alert that stays until you close it."},{stay,1}]}, Context);

event(#postback{message=fill_content}, Context) ->
    z_render:update("content", "Hello World<br/>", Context);
event(#postback{message=insert_top}, Context) ->
    z_render:insert_top("content", "At the top<br/>", Context);
event(#postback{message=insert_bottom}, Context) ->
    z_render:insert_bottom("content", "At the bottom<br/>", Context);

event(#drop{drag=Drag, drop=Drop}, Context) ->
    z_render:wire({growl, [{text,["You dropped ",Drag#dragdrop.tag," on ",Drop#dragdrop.tag]}]}, Context);
event(#drag{drag=Drag, drop=Drop}, Context) ->
    z_render:wire({growl, [{text,["You dragged ",Drag#dragdrop.tag," to ",Drop#dragdrop.tag]}]}, Context);

event(#sort{items=Drags, drop=Drop}, Context) ->
    DragIds = [ Id || #dragdrop{id=Id} <- Drags],
    Msg = io_lib:format("Result ~p on ~p",[DragIds,Drop#dragdrop.id]),
    z_render:wire({growl, [{text,Msg}]}, Context);

event(#submit{}, Context) ->
    Email = z_context:get_q_validated("email", Context),
    z_render:wire({growl, [{text,["You posted a valid email address \"",Email,"\""]}]}, Context);
    
event(Event, Context) ->
    Error = io_lib:format("~p: unknown event ~p", [?MODULE,Event]),
    z_render:wire({growl, [{text,Error},{stay,1}]}, Context).



periodic(_Args, Context) ->
    Date = httpd_util:rfc1123_date(),
    z_context:add_script_session([<<"z_growl_add('According to the server, the Universal Sprout Time is now<br/><strong>">>,Date,<<"</strong>', 0);">>], Context),
    timer:sleep(10000),
    ?MODULE:periodic([], Context).


    

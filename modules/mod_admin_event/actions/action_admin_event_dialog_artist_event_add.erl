%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-18
%% @doc Easy dialog to add an event for an artist.

%% Copyright 2009 Marc Worrell
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

-module(action_admin_event_dialog_artist_event_add).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    OnSuccess = proplists:get_all_values(on_success, Args),
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    Postback = {dialog_artist_event_add, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


event({postback, {dialog_artist_event_add, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    Vars = [
        {id, Id},
        {on_success, OnSuccess}
    ],
    z_render:dialog("Add an event for the artist.", "_action_dialog_artist_event_add.tpl", Vars, Context);


%% @doc Delete an username from an user.
%% @spec event(Event, Context1) -> Context2
event({submit, {event_add, Props}, _TriggerId, _TargetId}, Context) ->
    ArtistId = proplists:get_value(id, Props),
    Title = z_context:get_q_validated("title", Context),
    Venue = z_context:get_q("venue", Context),
    Genres = z_context:get_q_all("genre", Context),
    GroupId = list_to_integer(z_context:get_q("group_id", Context)),

    EventProps = [
        {is_published, true},
        {visible_for, 0},
        {category, event},
        {title, lists:flatten(Title)},
        {group_id, GroupId}
    ],
    
    F = fun(Ctx) ->
        case m_rsc:insert(EventProps, Ctx) of
            {ok, EventId} ->
                m_edge:insert(EventId, performer, ArtistId, Ctx),
                case Venue of
                    undefined -> nop;
                    _ -> m_edge:insert(EventId, atvenue, list_to_integer(Venue), Ctx)
                end,
                [ m_edge:insert(EventId, hasgenre, list_to_integer(Genre), Ctx) || Genre <- Genres, Genre /= [] ],
                {ok, EventId};
            {error, InsReason} ->
                throw({error, InsReason})
        end
    end,
    
    case z_db:transaction(F, Context) of
        {ok, EventId} ->
            Context1 = z_render:growl(["Created the event ",z_html:escape(Title), "."], Context),
            z_render:wire([
                    {dialog_close, []},
                    {redirect, [{dispatch, admin_edit_rsc}, {id, EventId}]}
                    | proplists:get_all_values(on_success, Props) ], Context1);
        {rollback, {Error, _CallStack}} ->
            case Error of
                _OtherError ->
                    ?DEBUG(Error),
                    z_render:growl_error("Could not create the event. Sorry.", Context)
            end
    end.

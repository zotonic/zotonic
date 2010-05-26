%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-01-15
%% @doc Simple comment module. Adds comments to any rsc.

%% Copyright 2010 Marc Worrell
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

-module(mod_comment).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Comments").
-mod_description("Comments for pages. Implements a simple comment system with comments stored locally.").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    install/1,
    event/2,
    search/2
]).

-include_lib("zotonic.hrl").

-record(state, {context}).


%% @doc Handle the submit event of a new comment
event({submit, {newcomment, Args}, _TriggerId, _TargetId}, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    Name = z_context:get_q_validated("name", Context),
    Email = z_context:get_q_validated("mail", Context),
    Message = z_context:get_q_validated("message", Context),
    case m_comment:insert(Id, Name, Email, Message, Context) of
        {ok, CommentId} ->
            Comment = m_comment:get(CommentId, Context),
            Props = [
                {id, Id},
                {comment, Comment},
                {creator, m_rsc:p(Id, creator_id, Context)},
                {hidden, true}
            ],
            Html = z_template:render("_comments_comment.tpl", Props, Context),
            Context1 = z_render:insert_bottom("comments-list", Html, Context),
            z_render:wire([
                {set_value, [{target, "message"}, {value, ""}]},
                {slide_down, [{target, "comment-"++integer_to_list(CommentId)}]},
                {redirect, [{location, "#comment-"++integer_to_list(CommentId)}]}
                ], Context1);
        {error, _} ->
            Context
    end.


%% @doc Return the list of recent comments.  Returned values are the complete records.
search({search_query, {recent_comments, []}, OffsetLimit}, Context) ->
    m_comment:search({recent_comments, []}, OffsetLimit, Context);
search(_, _Context) ->
    undefined.

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    Context = proplists:get_value(context, Args),
    Context1 = z_context:new(Context),
    z_notifier:observe(search_query, {?MODULE, search}, Context),
    install(Context1),
    {ok, #state{context=Context1}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:observe(search_query, {?MODULE, search}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


%% @doc Check the installation of the comment table. A bit more complicated because 0.1 and 0.2 had a table
%% in the default installer, this module installs a different table.
install(Context) ->
    ok = z_db:transaction(fun install1/1, Context),
    z_depcache:flush(Context),
    ok.
    
    install1(Context) ->
        ok = remove_old_comment_rsc_fields(Context),
        ok = remove_old_rating_table(Context),
        ok = install_comment_table(z_db:table_exists(comment, Context), Context),
        ok.


remove_old_rating_table(Context) ->
    case z_db:table_exists(rating, Context) of
        false ->
            ok;
        true ->
            case z_db:column_names(rating, Context) of
                [comment_id,created,id,ip_address,rsc_id,visitor_id] ->
                    z_db:q("drop table rating", Context),
                    ok;
                _ ->
                    ok
            end
    end.

install_comment_table(true, Context) ->
    % Check for old table
    case z_db:column_names(comment, Context) of
        [created,creator_id,id,ip_address,notify_id,props,rating,rsc_id] ->
            z_db:q("drop table comment", Context),
            install_comment_table(false, Context);
        [created,id,email,gravatar_code,ip_address,is_visible,keep_informed,
         name,props,rsc_id,user_agent,user_id,visitor_id] ->
            z_db:q("alter table comment drop column visitor_id cascade, "
                   "add column persistent_id character varying (32), "
                   "add constraint fk_comment_persistent_id foreign key (persistent_id) "
                   "  references persistent(id) on delete set null on update cascade", Context),
            z_db:q("create index fki_comment_persistent_id on comment(persistent_id)", Context),
            ok;
        _ ->
            % todo: add list of current fields here
            ok
    end;
install_comment_table(false, Context) ->
    z_db:q("
        create table comment (
            id serial not null,
            is_visible boolean not null default true,
            rsc_id int not null,
            user_id int,
            persistent_id character varying(32),
            gravatar_code character varying(40) not null default ''::character varying,
            email character varying(80) not null default ''::character varying,
            name character varying(80) not null default ''::character varying,
            user_agent character varying(250) not null default ''::character varying,
            ip_address character varying(40) not null default ''::character varying,
            keep_informed boolean not null default false,
            props bytea,
            created timestamp with time zone not null default now(),
            
            constraint comment_pkey primary key (id),
            constraint fk_comment_rsc_id foreign key (rsc_id)
                references rsc(id)
                on delete cascade on update cascade,
            constraint fk_comment_user_id foreign key (user_id)
                references rsc(id)
                on delete set null on update cascade,
            constraint fk_comment_persistent_id foreign key (persistent_id)
                references persistent(id)
                on delete set null on update cascade
        )
    ", Context),
    Indices = [
        {"fki_comment_rsc_id", "rsc_id"},
        {"fki_comment_user_id", "user_id"},
        {"fki_comment_persistent_id", "persistent_id"},
        {"fki_comment_ip_address", "ip_address"},
        {"comment_rsc_created_key", "rsc_id, created"},
        {"comment_created_key", "created"}
    ],
    [ z_db:q("create index "++Name++" on comment ("++Cols++")", Context) || {Name, Cols} <- Indices ],
    ok.


%% @doc In the 0.1.0 and 0.2.0 releases we had some pivot information in the rsc table. Remove this.
remove_old_comment_rsc_fields(Context) ->
    Cols = z_db:column_names(rsc, Context),
    R = [],
    R1 = case lists:member(comment_by, Cols) of true -> ["drop column comment_by"|R]; false -> R end,
    R2 = case lists:member(comments, Cols) of true -> ["drop column comments"|R1]; false -> R1 end,
    R3 = case lists:member(rating, Cols) of true -> ["drop column rating"|R2]; false -> R2 end,
    R4 = case lists:member(rating_count, Cols) of true -> ["drop column rating_count"|R3]; false -> R3 end,
    case R4 of
        [] ->
            ok;
        L -> 
            z_db:q("alter table rsc " ++ string:join(L, ", "), Context),
            ok
    end.

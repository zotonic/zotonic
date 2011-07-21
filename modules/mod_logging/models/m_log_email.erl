%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%%
%% @doc Model for email log messages.

%% Copyright 2011 Marc Worrell
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

-module(m_log_email).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    search/2,
    install/1
]).

-include_lib("zotonic.hrl").


search(Filter, Context) ->
    {Where, Args} = lists:foldl(fun(F, Acc) -> map_search(F, Acc, Context) end, {[], []}, Filter),
    #search_sql{
        select="*",
        from="log_email",
        where=lists:flatten(string:join(Where, " and ")),
        order="id desc",
        args=lists:reverse(Args),
        assoc=true
    }.

    map_search({severity, [C]}, {Ws,As}, _Context) when C >= $0, C =< $9 ->
        {["severity <= "++[C]|Ws], As};
    map_search({severity, _}, {Ws,As}, _Context) ->
        {["severity <= 1"|Ws], As};
    map_search({_, ""}, Acc, _Context) -> Acc;
    map_search({_, undefined}, Acc, _Context) -> Acc;
    map_search({status, Status}, {Ws,As}, _Context) ->
        {As1,N} = arg(Status, As),
        {[["mailer_status = $",N]|Ws], As1};
    map_search({message_nr, MsgNr}, {Ws,As}, _Context) ->
        {As1,N} = arg(MsgNr, As),
        {[["message_nr like ($",N," || '%')"]|Ws], As1};
    map_search({template, Tpl}, {Ws,As}, _Context) ->
        {As1,N} = arg(Tpl, As),
        {[["message_template like ($",N," || '%')"]|Ws], As1};
    map_search({to, To}, {Ws,As}, _Context) ->
        case z_utils:only_digits(To) of
            true ->
                {As1,N} = arg(list_to_integer(To), As),
                {[["to_id = $",N]|Ws], As1};
            false ->
                {As1,N} = arg(To, As),
                {[["envelop_to like ($",N," || '%')"]|Ws], As1}
        end;
    map_search({from, From}, {Ws,As}, _Context) ->
        case z_utils:only_digits(From) of
            true ->
                {As1,N} = arg(list_to_integer(From), As),
                {[["from_id = $",N]|Ws], As1};
            false ->
                {As1,N} = arg(From, As),
                {[["envelop_from like ($",N," || '%')"]|Ws], As1}
        end;
    map_search({content, RscId}, {Ws,As}, Context) ->
        case m_rsc:rid(RscId, Context) of
            undefined -> {["content_id = -1"|Ws], As};
            Id -> 
                {As1,N} = arg(integer_to_list(Id), As),
                {[["content_id = $",N]|Ws], As1}
        end;
    map_search({other, RscId}, {Ws,As}, Context) ->
        case m_rsc:rid(RscId, Context) of
            undefined -> {["other_id = -1"|Ws], As};
            Id -> 
                {As1,N} = arg(integer_to_list(Id), As),
                {[["other_id = $",N]|Ws], As1}
        end.
    
    
    arg(V, As) ->
        As1 = [V|As],
        N = integer_to_list(length(As1)),
        {As1, N}.
        

install(Context) ->
    case z_db:table_exists(log_email, Context) of
        true -> 
            ok;
        false ->
            z_db:q("
                create table log_email (
                    id bigserial not null,
                    severity int not null default 1,
                    message_nr character varying(32),
                    mailer_status character varying(32),
                    mailer_message bytea,
                    mailer_host character varying(128),
                    envelop_to character varying(128) not null,
                    envelop_from character varying(128) not null,
                    to_id int,
                    from_id int,
                    content_id int,
                    other_id int,
                    message_template character varying(64),
                    props bytea,
                    created timestamp with time zone not null default now(), 

                    constraint log_email_pkey primary key (id)
                )
            ", Context),
            Indices = [
                       {"log_email_severity", "severity, created"},
                       {"log_email_envelop_to", "envelop_to, created"},
                       {"log_email_to_id", "to_id, created"},
                       {"log_email_from_id", "from_id, created"},
                       {"log_email_content_id", "content_id, created"},
                       {"log_email_other_id", "other_id, created"},
                       {"log_email_created", "created"}
                      ],
            [ z_db:q("create index "++Name++" on log_email ("++Cols++")", Context) || {Name, Cols} <- Indices ]
    end.


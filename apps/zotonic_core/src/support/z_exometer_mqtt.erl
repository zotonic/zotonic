%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015 Maas-Maarten Zeeman
%% @doc Exometer reporter which publishes exometer stats to mqtt topics.

%% Copyright 2015 Maas-Maarten Zeeman
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
-module(z_exometer_mqtt).

%% exometer_report callback API
-export([
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_report_bulk/3,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_terminate/2,
    exometer_newentry/2,
    exometer_setopts/4
]).

-include("zotonic.hrl").
-include_lib("exometer_core/include/exometer.hrl").

-record(state, { 
    topic_prefix = [],
    context % The context
}).

exometer_init(Opts) ->
    {context, Context} = proplists:lookup(context, Opts),
    TopicPrefix = proplists:get_value(topic_prefix, Opts, []),
    {ok, #state{topic_prefix=TopicPrefix, context=Context}}.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, State) ->
    {ok, State}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    {ok, State}.

exometer_report(Metric, DataPoint, _Extra, Value, State) ->
    Topic = make_topic(Metric, DataPoint, State#state.topic_prefix),
    z_mqtt:publish(Topic, Value, State#state.context),
    {ok, State}.

exometer_report_bulk(Found, _Extra, State) ->
    [ begin
          Topic = make_topic(Metric, State#state.topic_prefix),
          z_mqtt:publish(Topic, DataPoint, State#state.context)
      end || {Metric, DataPoint} <- Found],

    {ok, State}.

exometer_call(Unknown, From, State) ->
    lager:debug("Unknown call ~p from ~p", [Unknown, From]),
    {ok, State}.

exometer_cast(Unknown, State) ->
    lager:debug("Unknown cast: ~p", [Unknown]),
    {ok, State}.

exometer_info(Unknown, State) ->
    lager:debug("Unknown info: ~p", [Unknown]),
    {ok, State}.

exometer_newentry(_Entry, State) ->
    {ok, State}.

exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.

exometer_terminate(_, _State) ->
    ok.

%%
%% Helpers
%%


make_topic(Metric, Prefix) ->
    Topic = [ Prefix, [z_convert:to_binary(M) || M <- Metric]],
    join_topic(Topic).

make_topic(Metric, DataPoint, Prefix) ->
    Topic = [ Prefix, [z_convert:to_binary(M) || M <- Metric],
              z_convert:to_binary(DataPoint)],
    join_topic(Topic).

join_topic(Topic) ->
    join(lists:flatten(Topic), <<"/">>).

join(L, Sep) ->
    join(L, Sep, <<>>).

join([], _Sep, Acc) -> Acc;
join([B| Rest], Sep, <<>>) ->
    join(Rest, Sep, B);
join([B| Rest], Sep, Acc) ->
    join(Rest, Sep, <<Acc/binary, Sep/binary, B/binary>>).



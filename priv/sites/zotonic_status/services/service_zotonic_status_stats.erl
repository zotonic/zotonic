%% @author Maas-Maarten <arjan@scherpenisse.net>
%% @copyright 2014 Maas-Maarten Zeeman
%% Date: 2014-08-20
%% @doc Retrieve a dump of all available exometer statistics of this node.

%% Copyright 2014 Maas-Maarten Zeeman
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

-module(service_zotonic_status_stats).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-svc_title("Retrieve all exometer statistics of this zotonic node.").
-svc_needauth(false). % We use authorization via z_sid

-export([
    process_get/2
]).

-include_lib("zotonic.hrl").


process_get(_ReqData, Context) ->
    case z_acl:user(Context) of
        undefined ->
            <<>>;
        _UserId ->
            Stats = [{metric_to_name(Metric), Value} || {Metric, Value} <- exometer:get_values([])],
            erlang:iolist_to_binary(mochijson2:encode(Stats))
    end.


%%
%% Helpers
%%

metric_to_name(Metric) ->
    metric_to_name(Metric, <<>>).

metric_to_name([], Acc) ->
    Acc;
metric_to_name([A|Rest], <<>>) ->
    metric_to_name(Rest, a2b(A));
metric_to_name([A|Rest], Acc) ->
    metric_to_name(Rest, <<Acc/binary, $$, (a2b(A))/binary>>).

a2b(Atom) ->
    erlang:atom_to_binary(Atom, utf8).



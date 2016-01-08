%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Match &amp; explain request dispatching.

%% Copyright 2014 Marc Worrell
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

-module(z_development_dispatch).

-export([
	event/2
	]).

-include_lib("zotonic.hrl").
-include_lib("wm_host_dispatch_list.hrl").

event(#submit{message=explain_dispatch}, Context) ->
	case z_acl:is_allowed(use, mod_development, Context) of
		true ->
			ReqPath = ensure_abs(z_convert:to_list(z_string:trim(z_context:get_q("explain_req", Context)))),
			Protocol = list_to_existing_atom(z_context:get_q("explain_protocol", Context)),
			TracerPid = erlang:spawn_link(fun tracer/0),
			z_sites_dispatcher:dispatch(
								z_context:hostname(Context),
								ReqPath,
								make_reqdata(Protocol, ReqPath),
								TracerPid),
			TracerPid ! {fetch, self()},
			receive
				{trace, Trace} ->
					Vars = [
						{trace, Trace},
						{path, z_convert:to_binary(ReqPath)},
						{protocol, Protocol}
					],
					Context1 = z_render:update(
									"explain-dispatch-output", 
									#render{template="_development_dispatch_trace.tpl", vars=Vars}, 
									Context),
					z_render:wire({fade_in, [{target, "explain-dispatch-output"}]}, Context1)
				after 5000 ->
					z_render:growl(?__("Could not fetch tracer output, please try again.", Context), Context) 
			end;
		false ->
			z_render:growl(?__("You are not allowed to use the dispatch debugging.", Context), Context) 
	end.

ensure_abs([]) -> [$/];
ensure_abs([$/|_] = P) -> P;
ensure_abs(P) -> [$/|P].

make_reqdata(https, Path) ->
	wrq:create({ssl, debug}, 'GET', https, {1,1}, Path, gb_trees:empty());
make_reqdata(http, Path) ->
	wrq:create(debug, 'GET', http, {1,1}, Path, gb_trees:empty()).

tracer() ->
	tracer_loop([]).

tracer_loop(Acc) ->
	receive
		{trace, Path, What, Args} ->
			Trace = {trace, maybe_flatten(Path), What, Args},
			tracer_loop([Trace|Acc]);
		{fetch, Pid} ->
			Acc1 = lists:reverse(Acc),
			Pid ! {trace, Acc1}
	end.

maybe_flatten(undefined) -> undefined;
maybe_flatten(Path) when is_binary(Path) -> Path;
maybe_flatten([X|_] = Path) when is_binary(X) -> Path;
maybe_flatten([X|_] = Path) when is_integer(X) -> z_convert:to_binary(Path);
maybe_flatten([]) -> <<>>.


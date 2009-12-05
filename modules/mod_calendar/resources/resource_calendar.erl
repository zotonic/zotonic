%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Calendar page.  Shows a week overview.

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

-module(resource_calendar).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	resource_exists/2,
	previously_existed/2,
	moved_temporarily/2
]).

-include_lib("resource_html.hrl").

resource_exists(ReqData, Context) ->
	Context1 = ?WM_REQ(ReqData, Context),
	Context2 = z_context:ensure_all(Context1),
	WeekStart = weekstart(Context2),
	case z_context:get_q("date", Context2, []) of
		[] ->
			{Date,_} = z_datetime:week_boundaries(erlang:localtime(), WeekStart),
			Context3 = z_context:set(date, Date, Context2),
			?WM_REPLY(false, Context3);
		Date ->
			case z_utils:only_digits(Date) andalso length(Date) == 8 of
				true ->
					%% Date given, check if is the start of a week.
					try
						Num = list_to_integer(Date),
						Y = Num div 10000,
						M = (Num rem 10000) div 100,
						D = Num rem 100,
						case z_datetime:week_boundaries({{Y,M,D},{0,0,0}}, WeekStart) of
							{{{Y,M,D},_} = Date1,_} ->
								Context3 = z_context:set(date, Date1, Context2),
								?WM_REPLY(true, Context3);
							{StartOfWeek, _} ->
								Context3 = z_context:set(date, StartOfWeek, Context2),
								?WM_REPLY(false, Context3)
						end
					catch 
						_:_ -> ?WM_REPLY(false, Context2)
					end;
				false ->
					?WM_REPLY(false, Context2)
			end
	end.

previously_existed(ReqData, Context) ->
	case z_context:get(date, Context) of
		undefined -> {false, ReqData, Context};
		_ -> {true, ReqData, Context}
	end.

moved_temporarily(ReqData, Context) ->
	{{Y,M,D},_} = z_context:get(date, Context),
	Date = integer_to_list(Y*10000 + M*100 + D),
	Location = z_dispatcher:url_for(calendar, [{date, Date}], Context),
	{{true, z_context:abs_url(Location, Context)}, ReqData, Context}.
	

%% @doc Show the page.
html(Context) ->
	Template = z_context:get(template, Context, "calendar_week.tpl"),
	Vars = [
		{weekstart, weekstart(Context)},
		{daystart, daystart(Context)},
		{date, z_context:get(date, Context)}
	],
	Html = z_template:render(Template, Vars, Context),
	z_context:output(Html, Context).
	
weekstart(Context) ->
	z_convert:to_integer(m_config:get_value(mod_calendar, weekstart, 1, Context)).
	
daystart(Context) ->
	z_convert:to_integer(m_config:get_value(mod_calendar, daystart, 0, Context)).
	

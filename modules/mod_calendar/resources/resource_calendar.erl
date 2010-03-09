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
	Period = z_context:get(period, Context2, week),
	setup_period(Period, Context2).
	
setup_period(week, Context) ->
	WeekStart = weekstart(Context),
	case z_context:get_q("date", Context, []) of
		[] ->
			{Date,_} = z_datetime:week_boundaries(erlang:localtime(), WeekStart),
			Context1 = z_context:set(date, Date, Context),
			?WM_REPLY(false, Context1);
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
								Context1 = z_context:set(date, Date1, Context),
								?WM_REPLY(true, Context1);
							{StartOfWeek, _} ->
								Context1 = z_context:set(date, StartOfWeek, Context),
								?WM_REPLY(false, Context1)
						end
					catch 
						_:_ -> ?WM_REPLY(false, Context)
					end;
				false ->
					?WM_REPLY(false, Context)
			end
	end;
setup_period(month, Context) ->
	case z_context:get_q("date", Context, []) of
		[] ->
		    {{Y,M,_}, _} = erlang:localtime(),
			Context1 = z_context:set(date, {{Y,M,1},{0,0,0}}, Context),
			?WM_REPLY(false, Context1);
		Date ->
			case z_utils:only_digits(Date) andalso length(Date) == 6 of
				true ->
					%% Date given, check if is the start of a month.
					try
						Num = list_to_integer(Date),
						Y = Num div 100,
						M = Num rem 100,
						true = (M >= 1 andalso M =< 12),
						Context1 = z_context:set(date, {{Y,M,1},{0,0,0}}, Context),
    					?WM_REPLY(true, Context1)
					catch 
						_:_ -> ?WM_REPLY(false, Context)
					end;
				false ->
					?WM_REPLY(false, Context)
			end
	end.


previously_existed(ReqData, Context) ->
	case z_context:get(date, Context) of
		undefined -> {false, ReqData, Context};
		_ -> {true, ReqData, Context}
	end.

moved_temporarily(ReqData, Context) ->
    case z_context:get(period, Context, week) of
        week ->
        	{{Y,M,D},_} = z_context:get(date, Context),
        	Date = integer_to_list(Y*10000 + M*100 + D),
        	Location = z_dispatcher:url_for(calendar, [{date, Date}], Context),
        	{{true, z_context:abs_url(Location, Context)}, ReqData, Context};
        month ->
        	{{Y,M,_},_} = z_context:get(date, Context),
        	Date = integer_to_list(Y*100 + M),
        	Location = z_dispatcher:url_for(calendar_month, [{date, Date}], Context),
        	{{true, z_context:abs_url(Location, Context)}, ReqData, Context}
    end.


%% @doc Show the page.
html(Context) ->
    Template = case z_context:get(period, Context, week) of
            week  -> z_context:get(template, Context, "calendar_week.tpl");
            month -> z_context:get(template, Context, "calendar_month.tpl")
        end,
    Vars = [
		{weekstart, weekstart(Context)},
		{daystart, daystart(Context)},
		{period, z_context:get(period, Context, week)},
		{date, z_context:get(date, Context)}
	],
	Html = z_template:render(Template, Vars, Context),
	z_context:output(Html, Context).

weekstart(Context) ->
	z_convert:to_integer(m_config:get_value(mod_calendar, weekstart, 1, Context)).
	
daystart(Context) ->
	z_convert:to_integer(m_config:get_value(mod_calendar, daystart, 0, Context)).

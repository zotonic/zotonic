-module(scomp_survey_poll).

-behaviour(gen_scomp).
-export([vary/2, render/3]).

-include("zotonic.hrl").
-include("../survey.hrl").

vary(_,_) -> nocache.

render(Args, _Vars, Context) ->
    {id, SurveyId} = proplists:lookup(id, Args),
    ElementId = proplists:get_value(element_id, Args, "survey-question"),
    Render = mod_survey:render_next_page(SurveyId, 1, [], Context),
    {ok, z_template:render(Render#render{vars=[{element_id, ElementId}|Render#render.vars]}, Context)}.

%% @doc Return a html fragment that can be used as an example for a survey question.
%% Call in the template as {% survey_example type="likert" %} 

-module(scomp_survey_survey_example).

-behaviour(gen_scomp).
-export([vary/2, render/3]).

-include("../survey.hrl").

vary(_,_) -> nocache.

render(Args, _Vars, Context) ->
    Type = proplists:get_value(type, Args),
    Module = list_to_atom("survey_q_"++z_convert:to_list(Type)),
    Q = Module:new(),
    {ok, filter_replace:replace(Q#survey_question.html, ["class=\"", "class=\"nosubmit "], Context)}.

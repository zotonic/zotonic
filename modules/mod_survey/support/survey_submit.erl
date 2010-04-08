%% @author Marc Worrell
%% @copyright 2010 Marc Worrell
%% @date 2010-04-06
%% @doc Handle an incoming submit of a survey. Checks for completeness and eventual errors.

-module(survey_submit).

-export([submit/3]).

-include_lib("zotonic.hrl").
-include("../survey.hrl").

submit(SurveyId, FormId, Context) ->
    case m_rsc:p(SurveyId, survey, Context) of
        undefined ->
            z_render:growl_error("Could not read survey information.", Context);
        {survey, QIds, Qs} ->
            {Answers, Missing, Accepted} = collect_answers(QIds, Qs, Context),
            ?DEBUG({Answers, Missing}),
            mark_accepted(Accepted, mark_missing(Missing, Context))
    end.


mark_accepted([], Context) ->
    Context;
mark_accepted([Id|Ids], Context) ->
    Context1 = z_render:wire({remove_class, [{target, Id},{class,"survey-missing"}]}, Context),
    mark_accepted(Ids, Context1).
    
mark_missing([], Context) ->
    Context;
mark_missing([Id|Ids], Context) ->
    Context1 = z_render:wire({add_class, [{target, Id},{class,"survey-missing"}]}, Context),
    mark_missing(Ids, Context1).


%% @doc Collect all answers, report any missing answers.
%% @type collect_answers(proplist(), Context) -> {AnswerList, MissingIdsList}
collect_answers(QIds, Qs, Context) ->
    collect_answers(QIds, Qs, Context, [], [], []).


collect_answers([], Qs, Context, Answers, Missing, Accepted) ->
    {Answers, Missing, Accepted};
collect_answers([QId|QIds], Qs, Context, Answers, Missing, Accepted) ->
    Q = proplists:get_value(QId, Qs),
    Module = module_name(Q),
    case Module:answer(Q, Context) of
        {ok, []} -> collect_answers(QIds, Qs, Context, Answers, Missing, [QId|Accepted]);
        {ok, AnswerList} -> collect_answers(QIds, Qs, Context, [{QId, AnswerList}|Answers], Missing, [QId|Accepted]);
        {error, missing} -> collect_answers(QIds, Qs, Context, Answers, [QId|Missing], Accepted)
    end.


module_name(#survey_question{type=Type}) ->
    list_to_atom("survey_q_"++atom_to_list(Type)).

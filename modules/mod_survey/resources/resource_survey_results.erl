%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Download a CSV file with the results of a survey

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

-module(resource_survey_results).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    encodings_provided/2,
    resource_exists/2,
    forbidden/2,
    expires/2,
    content_types_provided/2,
    charsets_provided/2,
    provide_content/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

%% Let cached versions expire in an hour.
-define(MAX_AGE, 3600).


init(DispatchArgs) ->
    {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_all(Context1),
    ?WM_REPLY(true, Context2).


allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.


charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.


encodings_provided(ReqData, Context) ->
    {[
        {"identity", fun(X) -> X end}, 
        {"gzip", fun(X) -> zlib:gzip(X) end}
    ], ReqData, Context}.


content_types_provided(ReqData, Context) ->
    {[{"text/csv", provide_content}], ReqData, Context}.


resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    {Id, Context2} = ensure_id(Context1),
    ?WM_REPLY(m_rsc:exists(Id, Context2), Context2).


%% @doc Check if the current user is allowed to view the resource. 
forbidden(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    {Id, Context2} = ensure_id(Context1),
    ?WM_REPLY(not m_survey:is_allowed_results_download(Id, Context2), Context2).

expires(ReqData, State) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs - 1), ReqData, State}.


provide_content(ReqData, Context) ->
    {Id, _} = ensure_id(Context),
    Filename = lists:flatten([
                    "survey-",
                    integer_to_list(Id),
                    case m_rsc:p(Id, slug, Context) of 
                        undefined -> "";
                        <<>> -> "";
                        Slug -> [$-|z_convert:to_list(Slug)]
                    end,
                    ".csv"
                ]),
    RD1 = wrq:set_resp_header("Content-Disposition", "inline; filename="++Filename, ReqData),
    Context1 = ?WM_REQ(RD1, Context),
    Data = m_survey:survey_results(Id, Context1),
    Content = to_csv(Data),
    ?WM_REPLY(Content, Context1).



ensure_id(Context) ->
    case z_context:get(id, Context) of
        undefined ->
            Id = m_rsc:rid(z_context:get_q("id", Context), Context),
            {Id, z_context:set(id, {ok, Id}, Context)};
        {ok, Id} ->
            {Id, Context}
    end.


to_csv(Rows) ->
    to_csv(Rows, <<>>).

to_csv([], Acc) ->
    Acc;
to_csv([R|Rows], Acc) ->
    RB = to_csv_row(R),
    to_csv(Rows, <<Acc/binary, RB/binary, 10>>).


to_csv_row(R) ->
    to_csv_row(R, <<>>).

to_csv_row([], Acc) ->
    Acc;
to_csv_row([V], Acc) ->
    B = filter(V),
    <<Acc/binary, B/binary>>;
to_csv_row([V|Vs], Acc) ->
    B = filter(V),
    to_csv_row(Vs, <<Acc/binary, B/binary, $,>>).


filter(V) when is_integer(V); is_atom(V); is_float(V) ->
    z_convert:to_binary(V);
filter(V) ->
    B = escape(z_convert:to_binary(V), <<$">>),
    <<B/binary, $">>.
    

escape(<<>>, Acc) ->
    Acc;
escape(<<10, Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, 10>>);
escape(<<13, Rest/binary>>, Acc) ->
    escape(Rest, Acc);
escape(<<$\\, Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, $\\, $\\>>);
escape(<<$", Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, $", $">>);
escape(<<C, Rest/binary>>, Acc) when C =< 32 ->
    escape(Rest, <<Acc/binary, 32>>);
escape(<<C, Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, C>>).





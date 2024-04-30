%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2024 Marc Worrell
%% @doc 'date' filter, display a date
%% @end

%% Copyright 2010-2024 Marc Worrell
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

-module(filter_date).

-export([
  date/3,
  date/4
]).

-include_lib("kernel/include/logger.hrl").

date(Date, Format, Id, Context) when Id =:= 0; Id =:= 1 ->
    ?LOG_INFO(#{
        in => zotonic_mod_base,
        text => <<"Filter 'date' now accepts a resource id as the timezone. Use 'UTC' to not convert timezones.">>
    }),
    Tz = m_rsc:p(Id, <<"tz">>, Context),
    date(Date, Format, z_context:set_tz(Tz, Context));
date(Date, Format, Id, Context) when is_integer(Id) ->
    Tz = m_rsc:p(Id, <<"tz">>, Context),
    date(Date, Format, z_context:set_tz(Tz, Context));
date(Date, Format, Tz, Context) ->
    date(Date, Format, z_context:set_tz(Tz, Context)).

date(undefined, _FormatStr, _Context) ->
    undefined;
date(<<>>, _FormatStr, _Context) ->
    <<>>;
date([Y,M,D], FormatStr, Context) when is_integer(Y), M >= 1, M =< 12, D >= 1, D =< 31 ->
    date({{z_convert:to_integer(Y),
           z_convert:to_integer(M),
           z_convert:to_integer(D)}, {0,0,0}}, FormatStr, Context);
date([[Y,M,D],[H,I,S]], FormatStr, Context)
    when is_integer(Y), M >= 1, M =< 12, D >= 1, D =< 31,
         H >= 0, H =< 23, I >= 0, I =< 59, S >= 0, S =< 60 ->
    date({{z_convert:to_integer(Y),
           z_convert:to_integer(M),
           z_convert:to_integer(D)},
          {z_convert:to_integer(H),
           z_convert:to_integer(I),
           z_convert:to_integer(S)}},
         FormatStr, Context);
date(Input, FormatStr, Context) when is_binary(Input) ->
    date(z_datetime:to_datetime(Input), FormatStr, Context);
date({{_,_,_} = Date,{_,_,_} = Time} = DT, FormatStr, Context) ->
    try
        z_convert:to_binary( z_datetime:format({Date, Time}, z_convert:to_list(FormatStr), Context) )
    catch
        error:Error ->
            ?LOG_WARNING(#{
                text => <<"Date format on illegal date">>,
                in => zotonic_mod_base,
                format => FormatStr,
                date => DT,
                result => error,
                reason => Error
            }),
            undefined
    end;
date({_,_,_} = Date, FormatStr, Context) ->
    try
        z_convert:to_binary( z_datetime:format({Date, {0,0,0}}, z_convert:to_list(FormatStr), Context) )
    catch
        error:Error ->
            ?LOG_WARNING(#{
                text => <<"Date format on illegal date">>,
                in => zotonic_mod_base,
                format => FormatStr,
                date => Date,
                result => error,
                reason => Error
            }),
            undefined
    end;
date(_Input, _FormatStr, _Context) ->
    undefined.

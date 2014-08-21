%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2014 Marc Worrell
%% @doc 'date' filter, display a date

%% Copyright 2010-2014 Marc Worrell
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

date(Date, Format, true, Context) ->
    date(Date, Format, z_context:set_tz(<<"UTC">>,Context));
date(Date, Format, false, Context) ->
    date(Date, Format, Context);
date(Date, Format, undefined, Context) ->
    date(Date, Format, Context);
date(Date, Format, [], Context) ->
    date(Date, Format, Context);
date(Date, Format, <<>>, Context) ->
    date(Date, Format, Context);
date(Date, Format, Tz, Context) ->
    date(Date, Format, z_context:set_tz(Tz,Context)).

date(undefined, _FormatStr, _Context) ->
    undefined;
date([Y,M,D], FormatStr, Context) ->
    date({{z_convert:to_integer(Y),
           z_convert:to_integer(M),
           z_convert:to_integer(D)}, {0,0,0}}, FormatStr, Context);
date([[Y,M,D],[H,I,S]], FormatStr, Context) ->
    date({{z_convert:to_integer(Y),
           z_convert:to_integer(M),
           z_convert:to_integer(D)},
          {z_convert:to_integer(H),
           z_convert:to_integer(I),
           z_convert:to_integer(S)}},
         FormatStr, Context);
date(Input, FormatStr, Context) when is_binary(FormatStr) ->
    date(Input, binary_to_list(FormatStr), Context);
date(Input, FormatStr, Context) when is_binary(Input) ->
    z_convert:to_binary(date(binary_to_list(Input), FormatStr, Context));
date({{_,_,_} = Date,{_,_,_} = Time} = DT, FormatStr, Context) ->
    try
        erlydtl_dateformat:format({Date, Time}, FormatStr, Context)
    catch
        error:Error ->
            lager:warning("Date format on illegal date ~p (format ~p), error: ~p", [DT, FormatStr, Error]),
            undefined
    end;
date({_,_,_} = Date, FormatStr, Context) ->
    try
        erlydtl_dateformat:format(Date, FormatStr, Context)
    catch
        error:Error ->
            lager:warning("Date format on illegal date ~p (format ~p), error: ~p", [Date, FormatStr, Error]),
            undefined
    end;
date(_Input, _FormatStr, _Context) ->
    undefined.

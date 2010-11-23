%%----------------------------------------------------------------------------------------------------------------
%% @author Jason Tanner <jt4websites@googlemail.com>
%% @copyright 2010 Jason Tanner
%% @doc 'datediff' filter, produce the difference between two dates selecting which date part is interesting.
%%----------------------------------------------------------------------------------------------------------------

%%----------------------------------------------------------------------------------------------------------------
%% Copyright 2010 Jason Tanner
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
%%----------------------------------------------------------------------------------------------------------------

-module(filter_datediff).
-export([datediff/3]).

datediff(undefined, _X, _Context) ->
    undefined;
datediff(_X, undefined, _Context) ->
    undefined;
datediff( {_Y,_M,_D} = Date, DatePart, Context) ->
    datediff( [Date,{0,0,0}], DatePart, Context );
datediff( {{_Y,_M,_D},{_H,_I,_S}} = DateTime, DatePart, Context) ->
    datediff( [ DateTime, calendar:local_time() ], DatePart, Context );
datediff( [ {{_Y,_M,_D},{_H,_I,_S}} = DateTimeA, {_YB,_MB,_DB} = DateB ], DatePart, Context) ->
    datediff( [ DateTimeA, {DateB,{0,0,0}} ], DatePart, Context );
datediff( [ {{_YA,_MA,_DA},{_HA,_IA,_SA}} = DateTimeA, {{_YB,_MB,_DB},{_HB,_IB,_SB}} = DateTimeB ], DatePart, _Context ) ->
    {{Y,M,D},{H,I,S}} = z_datetime:diff( DateTimeA, DateTimeB ),
    case DatePart of
        "Y" -> Y;
        "M" -> M;
        "D" -> D;
        "H" -> H;
        "I" -> I;
        "S" -> S;
        _   -> Y    % Defaults to YEAR
    end;
datediff( [ DateStringA, DateStringB ], DatePart, Context ) when is_list(DateStringA), is_tuple(DateStringB) ->
    datediff( [ z_convert:to_datetime(DateStringA), DateStringB ], DatePart, Context);
datediff( [ DateStringA, DateStringB ], DatePart, Context ) when is_tuple(DateStringA), is_list(DateStringB) ->
    datediff( [ DateStringA, z_convert:to_datetime(DateStringB) ], DatePart, Context);
datediff( [ DateStringA, DateStringB ], DatePart, Context ) when is_list(DateStringA), is_list(DateStringB) ->
    datediff( [ z_convert:to_datetime(DateStringA), z_convert:to_datetime(DateStringB) ], DatePart, Context);
datediff( DateString, DatePart, Context ) when is_list(DateString) ->
    datediff(z_convert:to_datetime(DateString), DatePart, Context).

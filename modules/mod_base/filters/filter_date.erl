%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'date' filter, display a date

%% Copyright 2010 Marc Worrell
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
-export([date/3]).


date(undefined, _FormatStr, _Context) ->
    undefined;
date(Input, FormatStr, Context) when is_binary(FormatStr) ->
    date(Input, binary_to_list(FormatStr), Context);
date(Input, FormatStr, Context) when is_binary(Input) ->
    z_convert:to_binary(date(binary_to_list(Input), FormatStr, Context));
date({{_,_,_} = Date,{_,_,_} = Time}, FormatStr, Context) ->
     erlydtl_dateformat:format({Date, Time}, FormatStr, Context);
date({_,_,_} = Date, FormatStr, Context) ->
    erlydtl_dateformat:format(Date, FormatStr, Context);
date(_Input, _FormatStr, _Context) ->
    undefined.

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-16
%% @doc Simple interface to the pie type of Google chart.  Allows for simpler data entry.
%% Parameters:  data=[{label,value}, ...] colors=some_color
%% and then all parameters

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

-module(scomp_base_chart_pie).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> default.

render(Params, Vars, Context) ->
    Data   = proplists:get_value(data, Params, []),
    Colors = proplists:get_value(colors, Params),
    ThreeD = proplists:get_value(threed, Params, false),
    
    Params1 = proplists:delete(labels, 
                proplists:delete(data,
                    proplists:delete(colors,
                        proplists:delete(threed, Params)))),

    {Labels,Values} = case Data of
        [] -> {[],[]};
        [{_,_}|_] -> lists:unzip(Data);
        [[_,_]|_] -> lists:foldr(fun([A,B],{Acc,Bcc}) -> {[A|Acc],[B|Bcc]} end, {[],[]}, Data)
    end,
    Axes  = [ {axis, [{position,"bottom"},{labels,Labels}]} ],
    Type  = case erlydtl_runtime:is_false(ThreeD) of
        true -> "pie";
        false -> "pie3d"
    end,
    
    Data2 = case is_list(Colors) of
        true ->  [{data, [{values, Values}, {color, Colors}]}];
        false -> [{data, [{values, Values}]}]
    end,
    
    Params2 = [{axis,Axes}, {type,Type}, {data,Data2} | Params1],
    scomp_base_google_chart:render(Params2, Vars, Context).

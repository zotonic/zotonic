%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Wire an action to an element.

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

-module(scomp_base_wire).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> nocache.

render(Params, _Vars, Context) ->
    Id        = proplists:get_value(id, Params, 'window'),
    Type      = proplists:get_value(type,Params, case Id of 'window' -> inline; _ -> click end),
    TargetId  = proplists:get_value(target,Params,Id),
    Actions   = proplists:get_all_values(action,Params),
    Postback  = proplists:get_value(postback,Params),
    Delegate  = proplists:get_value(delegate,Params),
    Propagate = z_convert:to_bool(proplists:get_value(propagate, Params, false)),
    
    Options   = [{action,X} || X <- Actions],
    Options1  = case Postback of
                    undefined -> Options;
                    Postback  -> [{postback,Postback} | Options]
                end,
    Options2  = case proplists:get_value(name, Params) of
                    undefined -> [{type,Type}|Options1];
                    Name -> [{type,named},{name,Name}|Options1]
                end,
    Options3  = case proplists:get_value(propagate, Params) of
                    undefined -> Options2;
                    Propagate -> [{propagate,Propagate}|Options2]
                end,
    Delegate1 = case Delegate of
        undefined -> undefined;
        _ -> z_convert:to_atom(Delegate)
    end,

    case Options1 of
        [] -> {error, "scomp wire: please give either an <em>action</em> or a <em>postback</em> parameter."};
        _  -> {ok, z_render:wire(Id, TargetId, {event,[{delegate,Delegate1}|Options3]}, Context)}
    end.

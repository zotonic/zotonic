%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Include a template by category, with possible caching
%%
%%      Example: include "some_file.tpl" and cache it for 3600 seconds
%%      {% include depend="something" maxage=3600 file="some_file.tpl" %}
%%
%%      Give a maxage of 0 for slam dunk protection but no caching.

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

-module(scomp_base_catinclude).
-behaviour(gen_scomp).

-export([vary/2, render/3]).

-include("zotonic.hrl").

vary(_Params, _Context) -> default.

render(Params, Vars, Context) ->
    All = proplists:get_value('$all', Params, false),
    File = proplists:get_value('$file', Params),
    Id = proplists:get_value('$id', Params),
    Context1 = case proplists:get_value(sudo, Params) of
        true -> z_acl:sudo(Context);
        _ -> Context
    end,
    Params1 = Params ++ Vars,
    render1(All, File, Id, Params1, Context1).

render1(IsAll, File, [{Cat,true}|_] = Cats0, Params, Context) when is_atom(Cat) ->
    Cats = [ C || {C,true} <- Cats0 ],
    render1(IsAll, File, Cats, Params, Context);
render1(false, File, [Cat|_] = Cats, Params, Context) when is_atom(Cat) ->
    {ok, z_template:render({cat, File, Cats}, Params, Context)};
render1(false, File, Id, Params, Context) ->
    RscId = m_rsc:rid(Id, Context),
    {ok, z_template:render({cat, File}, [{id,RscId}|Params], Context)};
render1(true, File, [Cat|_] = Cats, Params, Context) when is_atom(Cat) ->
    render1_all(File, Cats, Params, Context);
render1(true, File, Id, Params, Context) ->
    RscId = m_rsc:rid(Id, Context),
    IsA = m_rsc:is_a(RscId, Context),
    render1_all(File, IsA, [{id,RscId}|Params], Context).

render1_all(File0, IsA, Params, Context) ->
    File = z_convert:to_binary(File0),
    Root = filename:rootname(File),
    Ext = filename:extension(File),
    Templates = lists:foldr(fun(Cat, Templates) -> 
                                Templates ++ z_template:find_template(
                                                    <<Root/binary,  $., (z_convert:to_binary(Cat))/binary, Ext/binary>>, 
                                                    true,
                                                    Context) 
                            end,
                            [],
                            IsA),
    Templates1 = Templates ++ z_template:find_template(File, true, Context),
    {ok, [ z_template:render(Tpl, Params, Context) || Tpl <- Templates1 ]}.


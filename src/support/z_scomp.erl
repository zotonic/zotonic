%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell
%%
%% @doc Module for rendering and caching scomps.  Scomps can be caching and
%%      non caching, depending on the passed arguments and the results of the
%%      scomp's varies/2 function.

%% Copyright 2009-2010 Marc Worrell
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

-module(z_scomp).
-author("Marc Worrell <marc@worrell.nl>").

-export([render/4, render_all/4]).

-include_lib("zotonic.hrl").


%% @spec render(ScompName, Args, Vars, Context) -> {ok, Context} | {ok, io_list} | {error, Reason}
%% @doc Render the names scomp, Args are the scomp arguments and Vars are the variables given to the template
render(ScompName, Args, Vars, Context) ->
    case z_module_indexer:find(scomp, ScompName, Context) of
        {ok, #module_index{erlang_module=ModuleName}} ->
            ScompContext = z_context:prune_for_scomp(z_acl:args_to_visible_for(Args), Context), 
            render_scomp_module(ModuleName, Args, Vars, ScompContext, Context);
        {error, enoent} ->
            %% No such scomp, as we can switch on/off functionality we do a quiet skip
            ?LOG("No scomp enabled for \"~p\"", [ScompName]),
            {ok, <<>>}
    end.

render_all(ScompName, Args, Vars, Context) ->
    case z_module_indexer:find_all(scomp, ScompName, Context) of
        [] -> 
            [];
        ModuleNames when is_list(ModuleNames) ->
            ScompContext = z_context:prune_for_scomp(z_acl:args_to_visible_for(Args), Context),
            Args1 = [{'$all', true} | Args],
            RenderFun = fun(#module_index{erlang_module=ModuleName}) ->
                            {ok, Result} = render_scomp_module(ModuleName, Args1, Vars, ScompContext, Context),
                            Result
                        end,
            [ RenderFun(ModuleName) || ModuleName <- ModuleNames ]
    end.

render_scomp_module(ModuleName, Args, Vars, ScompContext, Context) ->
    ScompContextWM = ScompContext#context{wm_reqdata=Context#context.wm_reqdata},
    case vary(ModuleName, Args, ScompContext) of
        nocache ->
            case ModuleName:render(Args, Vars, ScompContextWM) of
                {ok, Result} -> {ok, z_context:prune_for_template(Result)};
                {error, Reason} -> throw({error, Reason})
            end;
        {CachKeyArgs, MaxAge, Varies} ->
            Key = key(ModuleName, CachKeyArgs, ScompContextWM),
            RenderFun =  fun() ->
                            case ModuleName:render(Args, Vars, ScompContextWM) of
                                {ok, Result} -> {ok, z_context:prune_for_template(Result)};
                                {error, Reason} -> throw({error, Reason})
                            end
                         end,
            z_depcache:memo(RenderFun, Key, MaxAge, Varies, Context)
    end.


%% @doc Create an unique key for the scomp and the visibility level it is rendered for
%% @spec key(atom(), proplist(), context()) -> term()
key(ScompName, EssentialParams, Context) ->
    {ScompName, EssentialParams, z_acl:cache_key(Context), Context#context.language}.


%% @doc Check how and if the scomp wants to be cached.
vary(ModuleName, Args, ScompContext) ->
    case ModuleName:vary(Args, ScompContext) of
        default ->
            %% Scomp asks default behaviour, check the arguments for caching args
            MaxAge = proplists:get_value(maxage, Args),
            case z_convert:to_integer(MaxAge) of
                undefined -> 
                    nocache; 
                Max ->
                    Vary  = proplists:get_all_values(vary, Args),
                    Args1 = proplists:delete(maxage, Args),
                    Args2 = proplists:delete(vary, Args1),
                    {Args2, Max, Vary}
            end;
        Other ->
            Other
    end.

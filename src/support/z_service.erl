%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-03
%% @doc Support functions for API calls.

%% Copyright 2009 Arjan Scherpenisse
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

-module(z_service).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
         needauth/1, 
         title/1, 
         all/1, 
         all/2, 
         serviceinfo/1,
         method/1,
         module/1,
         http_methods/1,
         handler/1,
         grouped/1,
         applies/2
        ]).

-include_lib("zotonic.hrl").

%%
%% Give information about all API services.
%%
all(Context) ->
    F = fun() ->
                  {_Ms, Services} = lists:unzip(z_module_indexer:find_all(service, true, Context)),
                  lists:filter(fun(S) -> z_module_manager:active(module(S), Context) end, Services)
          end,
    z_depcache:memo(F, {z_services}, ?WEEK, [z_modules], Context).



%%
%% All services grouped by module
%%
grouped(Context) ->
    grouped(all(Context), Context).

grouped(Services, Context) ->
    Mods = sets:to_list(sets:from_list([ module(S) || S <- all(Context) ])),
    Grouped = [ {Mod, lists:filter( fun(S) -> module(S) == Mod end, Services)} || Mod <-Mods ],
    lists:filter(fun({_,L}) -> not(L==[]) end, Grouped).

%%
%% All services augmented as serviceinfo/1 record.
%%
all(info, Context) ->
    Services = all(Context),
    [ serviceinfo(S) || S <- Services ];


%%
%% All services as authentication values
%%
all(authvalues, Context) ->
    All = all(Context),
    All2 = lists:filter( fun(S) -> needauth(S) end, All),
    Grouped = grouped(All2, Context),
    S = lists:flatten([ {"*", "Everything"} | 
                        [ [ {string:substr(atom_to_list(Module), 5) ++ "/*",proplists:get_value(mod_title, Module:module_info(attributes))} | 
                            [{method(S), title(S)} || S <- Services]]   || {Module, Services} <- Grouped]]),
    S.


%%
%% All information about a service.
%%%
serviceinfo(M) ->    
    [ {method, method(M)},
      {module, module(M)},
      {title,  title(M)},
      {needauth, needauth(M)},
      {http, string:join([atom_to_list(MM) || MM <- http_methods(M)],",")} 
     ].

    
%%
%% Whether a service needs an authenticated user. Defaults to false.
%%
needauth(Service) ->
    module_attr(Service, svc_needauth, false, atom).


%%
%% Title of the service
%%    
title(Service) ->
    module_attr(Service, svc_title, "(untitled)", list).


%%
%% The public method name of the service. 
%% 
method(Service) ->
    S = atom_to_list(Service),
    S2 = string:substr(S, 9),
    string:join(string:tokens(S2, "_"), "/").

%%
%% In which Zotonic module this API call resides. First checks to see
%% if a module exists which starts with 'mod_'; if this is not the
%% case the module is just the part of the service until the first
%% underscore.
%% 
module(Service) ->
    S = atom_to_list(Service),
    [M|_Rest] = string:tokens(string:substr(S, 9), "_"),
    Mod = list_to_atom("mod_" ++ M),
    case z_module_manager:module_exists(Mod) of
        true ->
            Mod;
        false ->
            list_to_atom(M)
    end.


%%
%% Which HTTP methods does this API support?
%%
http_methods(Service) ->
    F = Service:module_info(functions),
    lists:filter(fun (M) -> lists:member(handler(M), F) end, ['GET', 'POST', 'HEAD', 'PUT', 'DELETE']).

%% define the handler mapping for the module.
handler('POST') ->
    {process_post, 2};
handler('GET') -> 
    {process_get, 2};
handler('HEAD') ->
    {process_get, 2};
handler('PUT') ->
    {process_post, 2};
handler('DELETE') ->
    {process_post, 2}.


module_attr(Service, Attr, Default, T) ->
    code:ensure_loaded(Service),
    try
        Info = Service:module_info(attributes),
        V = proplists:get_value(Attr, Info),
        case T of
            list -> 
                case V of
                    undefined -> Default;
                    V -> V
                end;
            atom ->
                case V of
                    [X] -> X;
                    _ -> Default
                end
        end
    catch
        _M:_E -> 
            Default
    end.

%%
%% Whether a services applies to a pattern.  applies(Pattern, Service).
%%
applies([Pattern|Rest], Service) when is_list(Pattern) ->
    applies(Pattern, Service) orelse applies(Rest, Service);
applies(Pattern, Service) ->
    applies1(string:tokens(Pattern, "/"), string:tokens(method(Service), "/")).

applies1(["*"], _) ->
    true;
applies1([], _) ->
    false;
applies1([Part], [Part]) ->
    true;
applies1([Part|Rest], [Part|Rest2]) ->
    applies1(Rest, Rest2);
applies1(_, _) ->
    false.


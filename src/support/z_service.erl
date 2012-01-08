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
	 serviceinfo/2,
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
		[ServiceModule || {_Method, ServiceModule} <- z_module_indexer:find_all(service, true, Context)]
	end,
    z_depcache:memo(F, {z_services}, ?WEEK, [z_modules], Context).

%%
%% All services grouped by module
%%
grouped(Context) ->
    grouped(all(info, Context), Context).

grouped(Services, _Context) ->
    P = [{proplists:get_value(module, Service), Service} || Service <- Services],
    [{Mod, proplists:get_all_values(Mod, P)} || Mod <- proplists:get_keys(P) ].

%%
%% All services augmented as serviceinfo/1 record.
%%
all(info, Context) ->
    F = fun() ->
		Info = z_module_indexer:find_all(service, true, Context),
		[serviceinfo(M, zotonic_module(M, S), S) || {M, S} <- Info]
          end,
    z_depcache:memo(F, {z_services_info}, ?WEEK, [z_modules], Context);


%%
%% All services as authentication values
%%
all(authvalues, Context) ->
    All = all(info, Context),
    All2 = lists:filter( fun(S) -> proplists:get_value(needauth, S) end, All),
    Grouped = grouped(All2, Context),

    lists:flatten([ {"*", "Everything"} | 
		    [ [authvalue_module(Module) | [authvalue_service(S) || S <- Services]]   || {Module, Services} <- Grouped]]).

authvalue_module(Module) ->
    ModuleTitle = proplists:get_value(mod_title, Module:module_info(attributes)),
    {string:substr(atom_to_list(Module), 5) ++ "/*", ModuleTitle}.

authvalue_service(ServiceInfo) ->
    ServiceTitle = title(proplists:get_value(service, ServiceInfo)),
    {proplists:get_value(method, ServiceInfo), ServiceTitle}.
    

%%
%% Returns the zotonic module of the service
%%
zotonic_module(Method, ServiceModule) ->
    L = length(atom_to_list(ServiceModule)) - 9 - length(atom_to_list(Method)),
    Name = [$m, $o, $d, $_ | string:substr(atom_to_list(ServiceModule), 9, L)],
    list_to_atom(Name).

%%
%% All information about a service
%%
serviceinfo(ServiceModule, Context) ->
    All = all(info, Context),
    case lists:filter(fun(I) -> proplists:get_value(service, I) =:= ServiceModule end, All) of
	[Info] -> Info;
	_ ->
	    undefined
    end.

serviceinfo(Method, ZotonicModule, ServiceModule) ->
    ZotonicModuleName = string:substr(atom_to_list(ZotonicModule), 5),
    [ {method, string:join([ZotonicModuleName, atom_to_list(Method)], "/")},
      {module, ZotonicModule}, 
      {service, ServiceModule},
      {title,  title(ServiceModule)},
      {needauth, needauth(ServiceModule)},
      {http, string:join([atom_to_list(MM) || MM <- http_methods(ServiceModule)],",")} 
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
applies([Pattern|Rest], ServiceMethod) when is_list(Pattern) ->
    applies(Pattern, ServiceMethod) orelse applies(Rest, ServiceMethod);
applies(Pattern, ServiceMethod) ->
    applies1(string:tokens(Pattern, "/"), string:tokens(ServiceMethod, "/")).

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


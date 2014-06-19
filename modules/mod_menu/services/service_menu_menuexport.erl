%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2014-06-19
%% @doc Export a menu's structure as JSON

%% Copyright 2014 Arjan Scherpenisse
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

-module(service_menu_menuexport).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Export a menu resource structure as JSON.").
-svc_needauth(false).

-export([process_get/2, menu_export/2]).

-include_lib("zotonic.hrl").


process_get(_ReqData, Context) ->
    case z_context:get_q("id", Context) of
        undefined ->
            {error, missing_arg, "id"};
        [] ->
            {error, missing_arg, "id"};
        Id ->
            case m_rsc:exists(Id, Context) of 
                true ->
                    case m_rsc:is_visible(Id, Context) of
                        true ->
                            menu_export(m_rsc:p(Id, menu, Context), Context);
                        false ->
                            {error, access_denied, undefined}
                    end;
                false ->
                    {error, not_exists, Id}
            end
    end.

menu_export(Menu, Context) ->
    {array, menu_export(Menu, Context, [])}.

menu_export([], _, Acc) ->
    lists:reverse(Acc);
menu_export([{ItemId, Children} | Rest], Context, Acc) ->
    [menu_item(ItemId, Children, Context) | menu_export(Rest, Context, Acc)].


menu_item(Id, Children, Context) ->
    {struct,
     [
      {title, z_trans:trans(m_rsc:p(Id, title, Context), Context)},
      {url, m_rsc:p(Id, page_url, Context)},
      {id, Id}]
     ++ case Children of
            [] -> [];
            _ -> [{children, menu_export(Children, Context)}]
        end
    }.


        
    


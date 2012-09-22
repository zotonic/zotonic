%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-02-12

%% @doc Upload media items. Pass in the "file" argument for the actual file.

%% Copyright 2012 Arjan Scherpenisse
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

-module(service_base_media_upload).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Upload media items.").
-svc_needauth(true).

-export([process_post/2]).

-include_lib("zotonic.hrl").

process_post(_ReqData, Context) ->
    Upload = #upload{} = z_context:get_q("file", Context),
    
    Props
        = lists:flatten(
            lists:map(
              fun(Prop) ->
                      Value = z_context:get_q(atom_to_list(Prop), Context, <<>>),
                      case z_utils:is_empty(Value) of
                          false -> [{Prop, Value}];
                          true -> []
                      end
              end,
              [title, summary, body, chapeau, subtitle, website, page_path]
             )
           ), 

    case m_media:insert_file(Upload, Props, Context) of
        {ok, Id} ->
            Id;
        {error, R} ->
            {error, R, <<>>}
    end.

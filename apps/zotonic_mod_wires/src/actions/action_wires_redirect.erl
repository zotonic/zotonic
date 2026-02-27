%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Perform a redirect to another page.

%% Copyright 2009-2017 Marc Worrell
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

-module(action_wires_redirect).
-moduledoc("
This action redirects the browser to another page or back to the previous page.

Example:


```erlang
{% button text=\"home\" action={redirect location=\"/\"} %}
```

Redirects back to the home page when the button is clicked.

Back in history example:


```erlang
{% button text=\"Back\" action={redirect back} %}
```

After clicking the button the browser will go back to the last page using the JavaScript history.

Example of using dispatch rules for the redirect location:


```erlang
{% button text=\"edit\" action={redirect dispatch=\"admin_edit_rsc\" id=my_id} %}
```

When clicked the browser is redirected to the admin edit page for the [resource](/id/doc_glossary#term-resource) with
the id of my_id.

This action can have the following arguments:

| Argument | Description                                                                      | Example                         |
| -------- | -------------------------------------------------------------------------------- | ------------------------------- |
| back     | When given then the browser is directed to the previous page.                    | back                            |
| dispatch | The name of a dispatch rule. All other parameters are assumed to be parameters for the dispatch rule. | dispatch=”admin”                |
| id       | When back and dispatch are not defined then the redirect uri will be the page\\\\_url of the resource. | id=42                           |
| location | The http address to redirect to. Can be an url with or without host name.        | location=”<http://example.com>“ |
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_action/4,
    redirect_location/2
]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Script = case proplists:get_value(back, Args) of
        true ->
            case get_location(proplists:delete(back, Args), Context) of
                undefined ->
                    <<"history.go(-1);">>;
                Location ->
                    [<<"if (history.length > 1) history.go(-1); else window.location = \"">>,z_utils:js_escape(Location),$",$;]
            end;
        _ ->
            case get_location(Args, Context) of
                undefined ->
                    [];
                Location ->
                    [<<"window.location = \"">>,z_utils:js_escape(Location),$",$;]
            end
    end,
    {Script, Context}.


get_location(Args, Context) ->
    case proplists:get_value(dispatch, Args) of
        undefined ->
            case proplists:lookup(id, Args) of
                none ->
                    sanitize( proplists:get_value(location, Args, <<"/">>) );
                {id, undefined} ->
                    undefined;
                {id, Id} ->
                    m_rsc:p(Id, page_url, Context)
            end;
        DispatchString ->
            Dispatch = z_convert:to_atom(DispatchString),
            Args1 = proplists:delete(dispatch, Args),
            z_dispatcher:url_for(Dispatch, Args1, none, Context)
    end.

%% @doc Return the location for the redirect, iff the location is an url
-spec redirect_location({redirect, list()}, z:context()) -> {ok, binary()} | false.
redirect_location({redirect, Args}, Context) ->
    case get_location(Args, Context) of
        undefined -> false;
        Location -> {ok, iolist_to_binary(Location)}
    end.

sanitize(undefined) ->
    undefined;
sanitize(Url) ->
    z_html:sanitize_uri(Url).



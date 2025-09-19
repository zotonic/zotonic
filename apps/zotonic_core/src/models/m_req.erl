%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2022 Marc Worrell
%% @doc Model for the accessing the HTTP request properties. Exposes Cowmachine's wrq.erl

%% Copyright 2010-2022 Marc Worrell
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

-module(m_req).
-moduledoc("
This model gives access to the request variables from within a template.

Sometimes you need direct access to request variables in your template. The m\\_req model is meant for this. It exposes
some values from the Cowmachine request record.



Fetching a single value
-----------------------

You can fetch individual values by key, for example:


```django
{{ m.req.host|escape }}
```



Viewing all request variables
-----------------------------

Use the [print](/id/doc_template_tag_tag_print) tag to get a complete overview of all request variables:


```django
{% print m.req|make_list %}
```

This will show something like:


```django
[{method,<<\"GET\">>},
 {version,{1,1}},
 {peer,<<\"127.0.0.1\">>},
 {is_ssl,false},
 {host,<<\"mysite.test\">>},
 {raw_path,<<\"/en/page/1234?foo=bar\">>},
 {path,<<\"/en/page/1234\">>},
 {qs,[{<<\"foo\">>,<<\"bar\">>}]},
 {referrer,<<\"http://mysite.test:8000/\">>},
 {user_agent,<<\"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/601.4.4 (KHTML, like Gecko) Version/9.0.3 Safari/601.4.4\">>},
 {is_crawler,false},
 {req_id,525158920},
 {headers,[{<<\"accept\">>,
            <<\"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\">>},
           {<<\"accept-encoding\">>,<<\"gzip, deflate\">>},
           {<<\"accept-language\">>,<<\"en-us\">>},
           {<<\"cache-control\">>,<<\"max-age=0\">>},
           {<<\"connection\">>,<<\"keep-alive\">>},
           {<<\"cookie\">>,
            \"z_logon=; z_sid=LopWHBmHXCs94virnboZhBHLKV6m1Cga; z_ua=c%3Ddesktop%26u%3D1%26t%3D1%26w%3D1920%26h%3D1200\"},
           {<<\"dnt\">>,<<\"1\">>},
           {<<\"host\">>,<<\"mysite.test:8000\"},
           {<<\"referer\">>,<<\"http://mysite.test:8000/\">>},
           {<<\"user-agent\">>,
            <<\"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/601.4.4 (KHTML, like Gecko) Version/9.0.3 Safari/601.4.4\">>}]},
 {timezone,<<\"UTC\">>},
 {language,en}]
```

Please note that all values are raw and not escaped, take care to escape the values before you use them in your
templates, using the [escape](/id/doc_template_filter_filter_escape) filter.

The [make\\_list](/id/doc_template_filter_filter_make_list) filter is used to force the evaluation of the model;
otherwise it would just print `{m,req,undefined}`.
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,

    get/2
]).

-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ Key | Rest ], _Msg, Context) when is_binary(Key) ->
    try
        KeyAtom = erlang:binary_to_existing_atom(Key, utf8),
        {ok, {get(KeyAtom, Context), Rest}}
    catch
        error:badarg ->
            {error, unknown_key}
    end;
m_get([], _Msg, Context) ->
    {ok, {values(Context), []}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


%% @doc Fetch the field from the cowmachine_req interface.
-spec get(atom(), z:context() | cowboy_req:req() | undefined) -> any().
get(undefined, _) -> undefined;
get(_, undefined) -> undefined;
get(site, #context{} = Context) -> z_context:site(Context);
get(timezone, #context{} = Context) -> z_context:tz(Context);
get(language, #context{} = Context) -> z_context:language(Context);
get(csp_nonce, Context) -> z_context:csp_nonce(Context);
get(is_crawler, #context{} = Context) -> z_user_agent:is_crawler(Context);
get(is_http_request, #context{} = Context) -> z_context:get(is_http_request, Context);
get(session_id, #context{} = Context) ->
    case z_context:session_id(Context) of
        {ok, SId} -> SId;
        {error, _} -> undefined
    end;
get(peer_ip, #context{} = Context) ->
    case z_context:get(peer_ip, Context) of
        undefined -> maybe_get_req(peer_ip, Context);
        PeerIP -> PeerIP
    end;
get(peer, #context{} = Context) ->
    case z_context:get(peer_ip, Context) of
        undefined -> maybe_get_req(peer, Context);
        PeerIP -> ip_ntob(PeerIP)
    end;
get(user_agent, #context{} = Context) ->
    case z_context:get(user_agent, Context) of
        undefined -> maybe_get_req(user_agent, Context);
        PeerIP -> PeerIP
    end;
get(What, #context{} = Context) ->
    maybe_get_req(What, Context).

maybe_get_req(What, Context) ->
    case z_context:is_request(Context) of
        true ->  get_req(What, Context);
        false -> undefined
    end.

-spec get_req( atom(), z:context() ) -> any().
get_req(method, Context) -> cowmachine_req:method(Context);
get_req(version, Context) -> cowmachine_req:version(Context);
get_req(peer, Context) -> cowmachine_req:peer(Context);
get_req(peer_ip, Context) -> cowmachine_req:peer_ip(Context);
get_req(is_ssl, Context) -> cowmachine_req:is_ssl(Context);
get_req(scheme, Context) -> cowmachine_req:scheme(Context);
get_req(host, Context) -> cowmachine_req:host(Context);
get_req(port, Context) ->
    case cowmachine_req:is_proxy(Context) of
        true -> cowmachine_req:port(Context);
        false ->
            case cowmachine_req:is_ssl(Context) of
                true -> z_config:get(ssl_port);
                false -> z_config:get(port)
            end
    end;
get_req(raw_path, Context) -> cowmachine_req:raw_path(Context);
get_req(path, Context) -> cowmachine_req:path(Context);
get_req(qs, Context) -> cowmachine_req:req_qs(Context);
get_req(headers, Context) -> cowmachine_req:get_req_headers(Context);
get_req(user_agent, Context) ->
    case cowmachine_req:get_req_header(<<"user-agent">>, Context) of
        UA when is_binary(UA), size(UA) > 500 -> z_string:truncatechars(UA, 500);
        UA -> UA
    end;
get_req(referer, Context) -> cowmachine_req:get_req_header(<<"referer">>, Context);
get_req(referrer, Context) -> get_req(referer, Context);
get_req(_Key, _Context) -> undefined.

ip_ntob(IP) when is_tuple(IP) ->
    case inet:ntoa(IP) of
        {error, _} -> undefined;
        IPS -> z_convert:to_binary(IPS)
    end;
ip_ntob(_) ->
    undefined.

-spec values( z:context() ) -> list({atom(), any()}).
values(Context) ->
    [ {K, get(K, Context)} || K <- [
            method, version, peer, is_ssl, host, port, raw_path, path, qs, referrer, user_agent, is_crawler,
            headers, timezone, language
        ]
    ].

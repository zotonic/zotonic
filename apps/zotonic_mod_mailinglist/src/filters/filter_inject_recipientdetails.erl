%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012-2023 Arjan Scherpenisse <arjan@scherpenisse.net>
%% @doc Add details of the mailinglist recipient to the URL.
%% @end

%% Copyright 2012-2023 Arjan Scherpenisse
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

-module(filter_inject_recipientdetails).
-moduledoc("
Adds recipient query string details to hyperlinks.

This filter is meant for use inside e-mail templates. It replaces each occurrence of `##` with the details of the
subscriber that the mail is sent to, encoded as query string arguments.

Each occurrence of `##` will be transformed to recipient details in the following form:


```django
?email=foo@bar.com&name_first=John&name_surname=Doe
```

Its use case is when sending a mailing with a link in it which arrives at a webpage where the user has to enter his
e-mail address and name details. Using this filter, those parameters can be conveniently be pre-filled.
").
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([inject_recipientdetails/3]).

inject_recipientdetails(Body, undefined, _Context) ->
    Body;
inject_recipientdetails(Body, Recipient, _Context) ->
    case z_utils:is_empty(Body) of
        false ->
            Val = case proplists:get_value(props, Recipient, []) of
                      Props when is_list(Props) ->
                          [{email, proplists:get_value(email, Recipient)},
                           {name_first, proplists:get_value(name_first, Props)},
                           {name_surname, proplists:get_value(name_surname, Props)},
                           {name_surname_prefix, proplists:get_value(name_surname_prefix, Props)}];
                      _ ->
                          [{email, proplists:get_value(email, Recipient)}]
                  end,
            Val1 = lists:filter(fun({_, V}) -> not(z_utils:is_empty(V)) end, Val),
            Part = [$?, [ [z_url:url_encode(z_convert:to_list(K)), $=, z_url:url_encode(z_convert:to_list(V)), $\\, $&] || {K, V} <- Val1] ],
            Part1 = lists:flatten(Part),
            Part2 = lists:reverse(tl(lists:reverse(Part1))),
            iolist_to_binary(re:replace(Body, "##\"", Part2 ++ "\"", [global]));
        true ->
            Body
    end.


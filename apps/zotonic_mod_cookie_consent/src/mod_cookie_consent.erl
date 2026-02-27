%% @copyright 2022-2023 Driebit BV
%% @doc Manage cookies and hide/show elements depending on the
%% cookie consent preferences of users. Also add simple consent form to
%% accept or deny cookies.
%%
%% The cookie consent permissions request is only shown if there are
%% elements on the page that set cookies (or track visitors) and the cookie
%% consent is not yet requested.
%% @end

%% Copyright 2022-2023 Driebit BV
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

-module(mod_cookie_consent).
-moduledoc("
Wrap external content in such a way that it is only loaded if the user consented to the inclusion of the content (and
subsequent cookies).

The consent is one of:

*   `functional` this is always allowed
*   `stats` if consent for statistics tracking is given
*   `all` for any other kind of cookies

For elements this defaults to `all`. This means that they are only rendered if all consent is given.



How to use
----------

Ensure that your base template has an all-include of `_html_head.tpl` and `_html_body.tpl`.

Also, if you are using IFRAMEs, JS or CSS that sets non-functional cookies, check the changes below.



HTML
----

Media embedded via mod\\_oembed or mod\\_video\\_embed are automatically wrapped according to this method:


```erlang
<figure class=\"cookie-consent-preview do_cookie_consent mediaclass-...\" data-cookie-consent=\"all\">
    <img src=\"...\" alt=\"Media preview\">
    <figcaption>Please consent to cookies to display external content.</figcaption>
    <script type=\"text/x-cookie-consented\">
        {% filter escape %}
        <iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/IdIb5RPabjw\" title=\"YouTube video player\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen></iframe>
        {% endfilter %}
    </script>
</figure>
```

If there is no script tag then the page is reloaded after cookies are accepted.



IFRAME
------

Use the `data-cookie-consent-src` attribute to define the `src` if the cookie consent has been given:


```erlang
<iframe width=\"560\" height=\"315\" data-cookie-consent-src=\"https://www.youtube.com/embed/....\"></iframe>
```



JAVASCRIPT
----------

Use the special `type=\"text/x-cookie-consent\"` and optionally the `data-cookie-consent=\"...\"` attribute:


```erlang
<script type=\"text/x-cookie-consent\" data-cookie-consent=\"stats\" src=\"https://...\"></script>
```



CSS
---

Use the special `type=\"text/x-cookie-consent\"` and optionally the `data-cookie-consent=\"...\"` attribute:


```erlang
<link type=\"text/x-cookie-consent\" data-cookie-consent=\"stats\" href=\"https://...\"></script>
```

Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_media_viewer_consent`: Handle `media_viewer_consent` notifications using `z_template:render_to_iolist`.

").

-export([
    observe_media_viewer_consent/2,
    manage_schema/2
]).

-mod_title("Cookie Consent").
-mod_description("Handle cookie consent for stats and embedded content").
-mod_prio(400).
-mod_schema(3).

-include_lib("zotonic_core/include/zotonic.hrl").

observe_media_viewer_consent(#media_viewer_consent{ consent = functional }, _Context) ->
    % Always allow content that depends on functional cookies
    undefined;
observe_media_viewer_consent(#media_viewer_consent{
        id = Id,
        consent = Consent,
        html = EmbedCode,
        viewer_props = Props,
        viewer_options = Options
    }, Context) ->
    Vars = [
        {id, Id},
        {consent, Consent},
        {html, iolist_to_binary(EmbedCode)},
        {viewer_props, Props},
        {viewer_options, Options}
    ],
    {Wrapped, _} = z_template:render_to_iolist("_media_cookie_consent.tpl", Vars, Context),
    {ok, Wrapped}.

manage_schema(_Version, _Context) ->
    #datamodel{
        resources = [
            {cookie_consent, other, #{
                <<"is_published">> => true,
                <<"seo_noindex">> => true,
                <<"title">> => #trans{
                    tr = [
                        {nl, <<"Deze website maakt gebruik van cookies"/utf8>>},
                        {en, <<"This website uses cookies"/utf8>>}
                    ]
                },
                <<"summary">> => #trans{
                    tr = [
                        {nl, <<"Cookies zorgen ervoor dat de website goed kan fuctioneren, dat de website informatie kan onthouden die van invloed is op de werking of uitstraling ervan en om anoniem gegevens te verzamelen over hoe onze bezoekers door de site navigeren."/utf8>>},
                        {en, <<"Cookies help make a website usable, enable a website to remember information that changes the way the website behaves or looks and to understand how visitors interact with the website by collecting and reporting information anonymously."/utf8>>}
                    ]
                },
                <<"cookies_functional">> => #trans{
                    tr = [
                        {nl, <<"Functionele cookies zorgen ervoor dat de website goed kan functioneren, door basisfuncties als paginanavigatie en toegang tot beveiligde gedeelten van de website mogelijk te maken. Zonder deze cookies kan de website niet naar behoren werken."/utf8>>},
                        {en, <<"Necessary cookies help make a website usable by enabling basic functions like page navigation and access to secure areas of the website. The website cannot function properly without these cookies."/utf8>>}
                    ]
                },
                <<"cookies_statistics">> => #trans{
                    tr = [
                        {nl, <<"Statistische of analytische cookies helpen ons te begrijpen hoe bezoekers de website gebruiken, door anoniem gegevens te verzamelen en te rapporteren."/utf8>>},
                        {en, <<"Statistics cookies help us to understand how visitors interact with the website by collecting and reporting information anonymously."/utf8>>}
                    ]
                },
                <<"cookies_marketing">> => #trans{
                    tr = [
                        {nl, <<"Marketingcookies worden gebruikt om bezoekers te volgen wanneer ze verschillende websites bezoeken. Hun doel is advertenties weergeven die zijn toegesneden op en relevant zijn voor de individuele gebruiker. Deze advertenties worden zo waardevoller voor uitgevers en externe adverteerders."/utf8>>},
                        {en, <<"Marketing cookies are used to track visitors across websites. The intention is to display ads that are relevant and engaging for the individual user and thereby more valuable for publishers and third party advertisers."/utf8>>}
                    ]
                }
            }}
        ]
    }.

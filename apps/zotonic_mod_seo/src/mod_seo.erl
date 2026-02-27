%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Search engine optimization.  Provides admin interface for the SEO modules.
%% @end

%% Copyright 2009-2026 Marc Worrell
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

-module(mod_seo).
-moduledoc("
Adds basic search engine optimization to the base templates and provides an admin interface for configuring SEO options
and Google Universal Analytics.



SEO data
--------

mod_seo adds metadata to your pages to improve your website’s display and ranking in search engine (e.g. Google)
results. This metadata includes [structured data](https://developers.google.com/search/docs/guides/intro-structured-data):

Google uses structured data that it finds on the web to understand the content of the page, as well as to gather
information about the web and the world in general.

Following Google’s recommendations, mod_seo adds data in the [Schema.org](https://schema.org) vocabulary, using the
JSON-LD format. This enables [search features](https://developers.google.com/search/docs/guides/search-features) such as
rich results, carousels and the sitelinks searchbox.

Tip

If you wish to alter the structured data, you can do so by overriding the `schema_org/schema.*.tpl` templates. When you
do so, make sure to validate your changes with Google’s [structured data testing tool](https://search.google.com/structured-data/testing-tool).



Configuration
-------------



### Disable search engine indexing

To prevent search engines from indexing a page, check the ‘Ask Google not to index this page’ checkbox in the admin.
Programmatically, you can set the `seo_noindex` flag on a resource to do the same.

To disable indexing for the site as a whole, go to Modules > SEO in the Admin (`https://yoursite/admin/seo`) and tick
the ‘Exclude this site from search engines’ checkbox.



### Google Analytics

To enable [Google Universal Analytics](https://support.google.com/analytics/answer/2790010) tracking on your Zotonic
website, go to `https://yoursite/admin/seo` in your browser and enter your Google Analytics tracking code.

Zotonic does not automatically supports the [User ID Analytics
feature](https://support.google.com/analytics/answer/3123662). You have to [enable User
ID](https://support.google.com/analytics/answer/3123666) in your Analytics account and override the `_ga_params.tpl`
template to add the parameter:


```django
{#
    Override this template to provide extra Google Analytics parameters.
    See https://developers.google.com/analytics/devguides/collection/analyticsjs/field-reference
#}
{
    {% if m.acl.user %}
        \"userId\": \"{{ m.acl.user|escapejs }}\",
    {% endif %}
}
```



### Extra parameters

If you wish to add extra [Google Analytics
parameters](https://developers.google.com/analytics/devguides/collection/analyticsjs/field-reference), override the
`_ga_params.tpl` file and add the parameters:


```django
{#
    Override this template to provide extra Google Analytics parameters.
    See https://developers.google.com/analytics/devguides/collection/analyticsjs/field-reference
#}
{
    {% if m.acl.user %}
        \"userId\": \"{{ m.acl.user|escapejs }}\",
    {% endif %}
    \"sessionControl\": \"start\"
}
```
SEO module that adds metadata generation and admin SEO configuration options.


Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_admin_menu`: Contribute module entries to the admin menu tree.
- `observe_content_security_header`: Ensure that the Google Tag manager works correctly with our CSP using `m_config:get_value`.

").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Search Engine Optimization (SEO)").
-mod_description("Structured data, Google Analytics").
-mod_prio(600).
-mod_depends([base, admin]).
-mod_provides([seo]).
-mod_config([
        #{
            module => seo,
            key => noindex,
            type => boolean,
            default => false,
            description => "Set to true to request search engines to not index this site. Sets the robots meta tag to 'noindex' and 'nofollow'."
        },
        #{
            module => seo,
            key => keywords,
            type => string,
            default => "",
            description => "SEO keywords for this site, will be added to the SEO keywords meta tag."
        },
        #{
            module => seo,
            key => description,
            type => string,
            default => "",
            description => "SEO description for this site, will be added to the SEO description meta tag."
        },
        #{
            module => seo,
            key => search_action_hide,
            type => boolean,
            default => false,
            description => "Set to true to not add the search action to the JSON-LD structured data."
        },
        #{
            module => seo,
            key => search_action_url,
            type => string,
            default => "",
            description => "URL for the search action in the JSON-LD structured data. The default is the 'search' dispatch rule."
        },
        #{
            module => seo_plausible,
            key => analytics,
            type => boolean,
            default => false,
            description => "If set, this will generate the Plausible tracking code with the hostname as the domain."
        },
        #{
            module => seo_google,
            key => gtm,
            type => string,
            default => "",
            description => "Google Tag Manager ID. Generate GTM tracking code if set."
        },
        #{
            module => seo_google,
            key => gtm_insecure,
            type => string,
            default => "",
            description => "Flag for Google Tag Manager ID, allows Google full access to your pages."
        },
        #{
            module => seo_google,
            key => analytics,
            type => string,
            default => "",
            description => "Google Analytics ID. Generate GA tracking code if set."
        },
        #{
            module => seo_google,
            key => webmaster_verify,
            type => string,
            default => "",
            description => "Google Webmaster Tools verification code. Add a meta tag to verify your site with Google Webmaster Tools."
        },
        #{
            module => seo_bing,
            key => webmaster_verify,
            type => string,
            default => "",
            description => "Bing Webmaster Tools verification code. Add a meta tag to verify your site with Bing Webmaster Tools."
        },
        #{
            module => seo_yandex,
            key => webmaster_verify,
            type => string,
            default => "",
            description => "Yandex Webmaster Tools verification code. Add a meta tag to verify your site with Yandex Webmaster Tools."
        }
    ]).

%% interface functions
-export([
    observe_content_security_header/3,
    observe_admin_menu/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


%% @doc Ensure that the Google Tag manager works correctly with our CSP.
%% https://developers.google.com/tag-platform/security/guides/csp
observe_content_security_header(#content_security_header{}, CSP, Context) ->
    CSP1 = case m_config:get_value(seo_google, gtm, Context) of
        undefined -> CSP;
        <<>> -> CSP;
        _ ->
            ScriptSrcGTM = case m_config:get_boolean(seo_google, gtm_insecure, Context) of
                true ->
                    [
                        <<"https://www.googletagmanager.com">>,
                        <<"'strict-dynamic'">>
                        | CSP#content_security_header.script_src
                    ];
                false ->
                    [
                        <<"https://www.googletagmanager.com">>
                        | CSP#content_security_header.script_src
                    ]
            end,
            CSP#content_security_header{
                script_src = ScriptSrcGTM,
                img_src = [
                    <<"https://www.googletagmanager.com">>
                    | CSP#content_security_header.img_src
                ],
                connect_src = [
                    <<"https://www.googletagmanager.com">>
                    | CSP#content_security_header.connect_src
                ]
            }
    end,
    case m_config:get_value(seo_google, analytics, Context) of
        undefined -> CSP1;
        <<>> -> CSP1;
        _ ->
            ImgSrc = [
                <<"https://*.googletagmanager.com">>,
                <<"https://*.google-analytics.com">>
                | CSP1#content_security_header.img_src
            ],
            ImgSrc1 = lists:delete(<<"https://www.googletagmanager.com">>, ImgSrc),
            ScriptSrc = [
                <<"https://*.googletagmanager.com">>
                | CSP1#content_security_header.script_src
            ],
            ScriptSrc1 = lists:delete(<<"https://www.googletagmanager.com">>, ScriptSrc),
            ConnectSrc = [
                <<"https://*.google-analytics.com">>,
                <<"https://*.analytics.google.com">>,
                <<"https://*.googletagmanager.com">>
                | CSP1#content_security_header.connect_src
            ],
            ConnectSrc1 = lists:delete(<<"https://www.googletagmanager.com">>, ConnectSrc),
            CSP1#content_security_header{
                img_src = ImgSrc1,
                script_src = ScriptSrc1,
                connect_src = ConnectSrc1
            }
    end.


observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_seo,
                parent=admin_modules,
                label=?__("SEO", Context),
                url={admin_seo},
                visiblecheck={acl, use, ?MODULE}}

     |Acc].

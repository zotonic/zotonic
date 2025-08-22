%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc Search engine optimization.  Provides admin interface for the SEO modules.
%% @end

%% Copyright 2009-2025 Marc Worrell
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

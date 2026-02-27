%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc The base module, implementing basic Zotonic scomps, actions, models and validators.
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

-module(mod_base).
-moduledoc("
mod_base is the base module, which acts as a container module holding most of Zotonic basic [dispatch
rules](/id/doc_dispatch_index#all-dispatch-rules), [Actions](/id/template_action#actions) and [Module tags](/id/template_tag#scomps).

Note that the amount of templates has been kept to a minimum in this module, so that sites are free to implement
whatever templates they want.
Core base module providing fundamental dispatch rules, actions, and template components used across sites.


Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_content_types_dispatch`: Add an extra content-type to the 'id' controller using `controller_api:content_types_provided`.
- `observe_dispatch`: Check if there is a controller or template matching the path using `z_url:url_path_encode`.
- `observe_edge_delete`: If an edge is deleted, then force a repivot of the subject using `z_pivot_rsc:insert_queue`.
- `observe_edge_insert`: If an edge is inserted, then force a repivot of the subject using `z_pivot_rsc:insert_queue`.
- `observe_hierarchy_updated`: Handle `hierarchy_updated` notifications using `m_category:renumber`.
- `observe_media_stillimage`: Return the filename of a still image to be used for image tags using `z_media_preview:can_generate_preview`.

See also

[dispatch rules](/id/doc_dispatch_index#all-dispatch-rules).").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Zotonic Base").
-mod_description("Base supplies all basic scomps, actions and validators.").
-mod_prio(9999).
-mod_depends([]).
-mod_provides([base]).
-mod_schema(3).
-mod_config([
        #{
            module => site,
            key => title,
            type => string,
            default => "",
            description => "The title of the site, used in the HTML <title> tag and in the header."
        },
        #{
            module => site,
            key => subtitle,
            type => string,
            default => "",
            description => "The sub-title of the site, can used in the HTML <title> tag and in the header."
        },
        #{
            module => site,
            key => pagelen,
            type => integer,
            default => 20,
            description => "The default page length of paginated searches. Default is 20 items per page."
        },
        #{
            module => site,
            key => session_expire_inactive,
            type => integer,
            default => 14_400,
            description => "The time in seconds after which an inactive session expires. Default is 14400 seconds (4 hours)."
        },
        #{
            module => site,
            key => autologon_expire,
            type => integer,
            default => 15_552_000,
            description => "The time in seconds after which an autologon cookie expires. Default is 15552000 seconds (180 days)."
        },
        #{
            module => site,
            key => password_force_different,
            type => boolean,
            default => false,
            description => "If true, a user must choose a different password when changing their password."
        },
        #{
            module => site,
            key => state_cookie_secret,
            type => string,
            default => "",
            description => "The secret used to encrypt the state cookie with application state data remembered between page reloads. "
                           "It is automatically set, and must be a random string that is kept secret."
        },
        #{
            module => site,
            key => security_expires,
            type => string,
            default => "+1 month",
            description => "The relative time after which the .well-known/security.txt expires. Default is '+1 month'."
        },
        #{
            module => site,
            key => security_email,
            type => string,
            default => "",
            description => "The contact email address for security issues, used in the .well-known/security.txt file. "
                           "Defaults to the Zotonic config security_email, and if not set to the public mail_email address of the admin (id 1) user."
        },
        #{
            module => site,
            key => security_url,
            type => string,
            default => "",
            description => "The URL to the security contact, used in the .well-known/security.txt file. "
                           "Defaults to the URL of the page_security_contact page, and if not set to the Zotonic config security_url."
        },
        #{
            module => site,
            key => security_policy_url,
            type => string,
            default => "",
            description => "The URL to the security policy, used in the .well-known/security.txt file."
                           "Defaults to the URL of the page_security_policy page, and if not set to the Zotonic config security_policy_url."
        },
        #{
            module => site,
            key => security_hiring_url,
            type => string,
            default => "",
            description => "The URL to the security hiring, used in the .well-known/security.txt file."
                           "Defaults to the URL of the page_security_hiring page, and if not set to the Zotonic config security_hiring_url."
        },
        #{
            module => site,
            key => smtphost,
            type => string,
            default => "",
            description => "The domain used for the 'from' when sending email and the SMTP hostname used for receiving email. "
                           "Defaults to the site's configured hostname."
        },
        #{
            module => site,
            key => bounce_email_override,
            type => string,
            default => "",
            description => "If set, this email address is used as the envelop address on emails and will receive bounced emails. "
                           "Defaults to the Zotonic config smtp_bounce_email_override, and if not set to the noreply with the smtphost as the domain."
        },
        #{
            module => site,
            key => email_from,
            type => string,
            default => "",
            description => "The email address used as the 'from' address when sending emails. "
                           "Defaults to noreply with the smtphost as the domain."
        },
        #{
            module => site,
            key => email_override,
            type => string,
            default => "",
            description => "If set, all outgoing email will be sent to this address. Useful on development systems to catch all email. "
                           "The Zotonic config email_override always takes precedence over this setting."
        },
        #{
            module => site,
            key => email_override_exceptions,
            type => string,
            default => "",
            description => "Comma or space separated list of email addresses and @domains that are not overridden by the email_override. "
                           "Useful to allow some emails to be sent to their original destination, while others are caught by the email_override."
        },
        #{
            module => site,
            key => client_smtphost,
            type => string,
            default => "",
            description => "The domain hostname as identification when sending email to other SMTP servers. "
                           "Defaults to the site's configured hostname."
        },
        #{
            module => site,
            key => smtp_relay,
            type => boolean,
            default => false,
            description => "If true, the site will relay email through the SMTP server configured in smtp_relay_host. "
                           "If false, the site will send email directly to the recipient's mail server."
        },
        #{
            module => site,
            key => smtp_relay_host,
            type => string,
            default => "",
            description => "If smtp_relay is set, the hostname of the SMTP server to relay email through. "
                           "Defaults to localhost."
        },
        #{
            module => site,
            key => smtp_relay_port,
            type => integer,
            default => 25,
            description => "If smtp_relay is set, the port of the SMTP server to relay email through. "
                           "Defaults to 25 or 587, depending on smtp_relay_ssl."
        },
        #{
            module => site,
            key => smtp_relay_ssl,
            type => boolean,
            default => false,
            description => "If smtp_relay is set, always use SSL/TLS when connecting to the relaying SMTP server. "
                           "Defaults to false, which means STARTTLS is only used if supported by the relaying server."
        },
        #{
            module => site,
            key => smtp_relay_username,
            type => string,
            default => "",
            description => "If smtp_relay is set, the username to authenticate with the relaying SMTP server. "
                           "Defaults to an empty string, which means no authentication is used."
        },
        #{
            module => site,
            key => smtp_relay_password,
            type => string,
            default => "",
            description => "If smtp_relay is set, the password to authenticate with the relaying SMTP server. "
        },
        #{
            module => site,
            key => hsts,
            type => boolean,
            default => false,
            description => "If true, the site will send the Strict-Transport-Security header to enforce HTTPS connections. "
                           "Defaults to false."
        },
        #{
            module => site,
            key => hsts_maxage,
            type => integer,
            default => 17_280_000,
            description => "The maximum age in seconds for the Strict-Transport-Security header. "
                           "This is only useful if hsts is also set to true. Defaults to 17280000 seconds (200 days)."
        },
        #{
            module => site,
            key => hsts_include_subdomains,
            type => boolean,
            default => false,
            description => "If true, the Strict-Transport-Security header will include the 'includeSubDomains' directive. "
                           "This is only useful if hsts is also set to true. Defaults to false."
        },
        #{
            module => site,
            key => hsts_preload,
            type => boolean,
            default => false,
            description => "If true, the site will include the 'preload' directive in the Strict-Transport-Security header. "
                           "This is only useful if hsts is also set to true. Defaults to false."
        }
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% interface functions
-export([
    observe_content_types_dispatch/3,
    observe_edge_insert/2,
    observe_edge_delete/2,
    observe_media_stillimage/2,
    observe_dispatch/2,
    observe_hierarchy_updated/2,
    manage_schema/2
]).

%% @doc Add an extra content-type to the 'id' controller.
observe_content_types_dispatch(#content_types_dispatch{ id = _Id }, Acc, Context) ->
    {ContentTypes, _} = controller_api:content_types_provided(Context),
    Dispatch = lists:map(
        fun
            ({<<"application">>, <<"json">>, _} = CT) ->
                {CT, {api_rsc_export, []}};
            ({A, B, _} = CT) ->
                {CT, {api_rsc_export, [ {zotonic_http_accept, <<A/binary, $/, B/binary>>} ]}}
        end,
        ContentTypes),
    Dispatch ++ Acc.


%% @doc If an edge is inserted, then force a repivot of the subject
observe_edge_insert(#edge_insert{ subject_id=SubjectId }, Context) ->
    z_pivot_rsc:insert_queue(SubjectId, Context),
    ok.

%% @doc If an edge is deleted, then force a repivot of the subject
observe_edge_delete(#edge_delete{ subject_id=SubjectId }, Context) ->
    z_pivot_rsc:insert_queue(SubjectId, Context),
    ok.

%% @doc Return the filename of a still image to be used for image tags.
%% @spec observe_media_stillimage(Notification, _Context) -> undefined | {ok, Filename} | {ok, {filepath, Filename, Path}}
observe_media_stillimage(#media_stillimage{props=Props}, Context) ->
    case maps:get(<<"preview_filename">>, Props, undefined) of
        None when None =:= <<>>; None =:= undefined ->
            case maps:get(<<"mime">>, Props, undefined) of
                undefined -> undefined;
                [] -> undefined;
                <<>> -> undefined;
                Mime ->
                    case z_media_preview:can_generate_preview(Mime) of
                        true ->
                            %% Let media preview handle this.
                            undefined;
                        false ->
                            %% Serve an icon representing the mime type.
                            [ A, B ] = binary:split(Mime, <<"/">>),
                            Files = [
                                <<"images/mimeicons/", A/binary, $-, B/binary, ".png">>,
                                <<"images/mimeicons/", A/binary, ".png">>,
                                <<"images/mimeicons/application-octet-stream.png">>
                            ],
                            lists:foldl(
                                fun(F, undefined) ->
                                        case z_module_indexer:find(lib, F, Context) of
                                            {ok, #module_index{filepath=_File}} -> {ok, <<"lib/", F/binary>>};
                                            {error, enoent} -> undefined
                                        end;
                                   (_F, Result) ->
                                       Result
                                end,
                                undefined,
                                Files)
                    end
            end;
        PreviewFilename ->
            {ok, z_convert:to_list(PreviewFilename)}
    end.

%% @doc Check if there is a controller or template matching the path.
observe_dispatch(#dispatch{path=Path}, Context) ->
    EncodedPagePath = z_url:url_path_encode(Path),
    case m_rsc:page_path_to_id(EncodedPagePath, Context) of
        {ok, Id} ->
            maybe_page_path_language(Id, EncodedPagePath, Context);
        {redirect, Id} ->
            {ok, #dispatch_match{
                    mod=controller_redirect,
                    mod_opts=[{id, Id}, {is_permanent, true}]}};
        {error, _} ->
            SlashPath = case Path of
                <<>> -> <<"/">>;
                <<"/", _/binary>> -> Path;
                _ -> <<"/", Path/binary>>
            end,
            case filename:extension(SlashPath) of
                <<".tpl">> ->
                    undefined;
                _ ->
                    find_static_file(SlashPath, Context)
            end
    end.

maybe_page_path_language(Id, RequestedPagePath, Context) ->
    case m_req:get(raw_path, Context) of
        RequestedPagePath ->
            % This is the original request path - assume no language rewrite done
            case z_context:get_q(<<"z_language">>, Context) of
                undefined ->
                    % No language override in the URL
                    % Check the language matching the found page_path.
                    case m_rsc:p_no_acl(Id, <<"page_path">>, Context) of
                        #trans{ tr = Tr } ->
                            Langs = lists:filtermap(
                                fun
                                    ({Lang, P}) when P =:= RequestedPagePath -> {true, Lang};
                                    (_) -> false
                                end,
                                Tr),
                            if
                                Langs =:= [] ->
                                    {ok, Id};
                                true ->
                                    case lists:member(z_context:language(Context), Langs) of
                                        true -> {ok, Id};
                                        false ->
                                            % Select best language for the current context
                                            Lang = z_trans:lookup_fallback_language(Langs, Context),
                                            {ok, Id, [ {z_language, Lang} ]}
                                    end
                            end;
                        _ ->
                            {ok, Id}
                    end;
                _QLang ->
                    {ok, Id}
            end;
        _OtherPath ->
            % Assume language was in the URL and there was a rewrite for the language
            {ok, Id}
    end.


find_static_file(SlashPath, Context) ->
    Lang = z_convert:to_binary(z_context:language(Context)),
    Files = case last(SlashPath) of
        $/ ->
            [
                <<"static/", Lang/binary, SlashPath/binary, "index.tpl">>,
                <<"static", SlashPath/binary, "index.tpl">>
            ];
        _ ->
            [
                <<"static/", Lang/binary, SlashPath/binary, ".tpl">>,
                <<"static", SlashPath/binary, ".tpl">>,
                <<"static/", Lang/binary, SlashPath/binary>>,
                <<"static", SlashPath/binary>>,
                <<"static/", Lang/binary, SlashPath/binary, "/index.tpl">>,
                <<"static", SlashPath/binary, "/index.tpl">>
            ]
    end,
    find_static_files_first(Files, Context).

find_static_files_first([], _Context) ->
    undefined;
find_static_files_first([F|Fs], Context) ->
    case z_module_indexer:find(template, F, Context) of
        {ok, #module_index{ filepath = Filepath }} ->
            case filename:extension(F) of
                <<".tpl">> ->
                    {ok, #dispatch_match{
                        mod = controller_template,
                        mod_opts = [ {template, F} ],
                        bindings = []
                    }};
                _ ->
                    {ok, #dispatch_match{
                        mod = controller_static_pages,
                        mod_opts = [ {fullpath, Filepath} ],
                        bindings = []
                    }}
            end;
        {error, _} ->
            find_static_files_first(Fs, Context)
    end.

last(<<>>) -> $/;
last(Path) -> binary:last(Path).


observe_hierarchy_updated(#hierarchy_updated{ root_id = <<"$category">> }, Context) ->
    % Something changed to the category hierarchy - let m_category resync the pivot
    m_category:renumber(Context);
observe_hierarchy_updated(#hierarchy_updated{ root_id = _ }, _Context) ->
    ok.

manage_schema(_, _Context) ->
    #datamodel{
        categories = [
            {organization, undefined, #{
                <<"title">> => #trans{
                    tr =[
                        {en, <<"Organization">>},
                        {nl, <<"Organisatie">>}
                    ]}
            }}
        ]
    }.

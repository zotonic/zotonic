%% @copyright 2026 Marc Worrell
%% @doc Human-readable explanations of persisted WebSub errors.
-module(filter_websub_error).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "template_filter",
        "export_and_syndication", "localization_and_translation"]
}).
-moduledoc("Translate subscription error codes into explanations for editors.
Accepts atoms, plain binaries and legacy binaries formatted with ~p.
Unknown errors use a generic message; retain the original code separately for diagnostics.
The result is plain text and must be escaped in HTML templates.").
-export([websub_error/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

-spec websub_error(Error, Context) -> binary() when
    Error :: term(),
    Context :: z:context().
websub_error(undefined, _Context) ->
    <<>>;
websub_error(<<>>, _Context) ->
    <<>>;
websub_error(Error, Context) when is_atom(Error) ->
    websub_error(atom_to_binary(Error, utf8), Context);
%% Previously, binary denial reasons were persisted as <<"reason">>.
%% Strip that wrapper without parsing untrusted Erlang terms.
websub_error(<<"<<\"", Rest/binary>>, Context) when byte_size(Rest) >= 3 ->
    Size = byte_size(Rest) - 3,
    case Rest of
        <<Reason:Size/binary, "\">>">> -> websub_error(Reason, Context);
        _ -> message(Rest, Context)
    end;
websub_error(Error, Context) ->
    message(Error, Context).

message(<<"access-denied-websub">>, Context) ->
    ?__("The source website does not allow this subscriber to use WebSub. On the source website, allow the subscribing user to use WebSub, or allow anonymous visitors for public subscriptions.", Context);
message(<<"access-denied-rsc">>, Context) ->
    ?__("The source website refused access to this page. The subscriber must be allowed to view it, and the page must be an original on that website.", Context);
message(<<"invalid-topic">>, Context) ->
    ?__("The source website does not recognize this subscription topic. Check that the original page still exists.", Context);
message(<<"unsafe_destination">>, Context) ->
    ?__("The destination could not be resolved or its network address is blocked. Local and private addresses are only allowed for .test hostnames in development.", Context);
message(<<"verification_timeout">>, Context) ->
    ?__("The source website did not confirm the subscription in time. Check that it can reach this website's callback URL.", Context);
message(<<"self_subscription">>, Context) ->
    ?__("This page points back to this website. A website cannot subscribe to itself.", Context);
message(<<"no_websub">>, Context) ->
    ?__("The source page does not advertise automatic updates via WebSub.", Context);
message(<<"invalid_discovery">>, Context) ->
    ?__("The source page advertises invalid WebSub links. Check its hub and topic URLs.", Context);
message(<<"resource_identity_changed">>, Context) ->
    ?__("The original page address has changed. Import the page again to set up automatic updates.", Context);
message(<<"eacces">>, Context) ->
    ?__("Access was denied while updating this page. Check the subscriber's source website credentials and permission to update the local page.", Context);
message(<<"fetch_failed">>, Context) ->
    ?__("Could not contact the remote website. Check its availability and HTTPS certificate.", Context);
message(<<"invalid_url">>, Context) ->
    ?__("The remote website address is invalid. Check the source and WebSub URLs.", Context);
message(<<"callback_hostname_required">>, Context) ->
    ?__("The callback URL must use a hostname instead of an IP address. Check this website's public URL.", Context);
message(<<"insecure_redirect">>, Context) ->
    ?__("The remote website redirects from HTTPS to HTTP. Use a secure HTTPS destination.", Context);
message(<<"too_many_redirects">>, Context) ->
    ?__("The remote website redirects too many times. Check its redirect configuration.", Context);
message(<<"missing_location">>, Context) ->
    ?__("The remote website returned a redirect without a destination.", Context);
message(<<"invalid_redirect">>, Context) ->
    ?__("The remote website returned an invalid redirect destination.", Context);
message(<<"{http_status,401}">>, Context) ->
    ?__("The remote website requires authentication. Check the subscribing user's configured credentials.", Context);
message(<<"{http_status,403}">>, Context) ->
    ?__("The remote website refused access. Check the subscribing user's permissions on that website.", Context);
message(<<"{http_status,404}">>, Context) ->
    ?__("The requested page or WebSub endpoint was not found on the remote website.", Context);
message(_Error, Context) ->
    ?__("Automatic updates could not be completed. Check the technical details for the reported error.", Context).

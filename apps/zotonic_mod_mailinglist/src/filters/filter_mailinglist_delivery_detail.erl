%% @copyright 2026 Marc Worrell
%% @doc Display the diagnostic from legacy serialized delivery failures.
-module(filter_mailinglist_delivery_detail).
-moduledoc("""
Extract the SMTP diagnostic from legacy `{bounce, <<\"...\">>}` and related
mailing-run details, including nested errors and missing diagnostics. New details
are stored as plain text. Only known wrappers, quoted strings and restricted
identifiers are accepted; arbitrary Erlang terms are never evaluated. The returned text is not HTML-safe: use the escape filter.
""").
-export([mailinglist_delivery_detail/2]).

-spec mailinglist_delivery_detail(Detail, Context) -> binary() | undefined when
    Detail :: binary() | undefined,
    Context :: z:context().
mailinglist_delivery_detail(Detail, _Context) ->
    z_mailinglist_delivery_detail:format(Detail).

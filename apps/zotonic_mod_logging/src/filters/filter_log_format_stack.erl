-module(filter_log_format_stack).

-export([
    log_format_stack/2
]).

log_format_stack(undefined, _Context) ->
    undefined;
log_format_stack(Stack, Context) ->
    S1 = z_html:escape(Stack),
    S2 = re:replace(
        S1,
        "^((.*)@)?(.*)?$",
        "<b>\\2</b> <small class='text-muted'>\\3</small>",
        [ global, multiline ]),
    filter_linebreaksbr:linebreaksbr( iolist_to_binary(S2), Context ).

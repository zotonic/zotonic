%%% Stub for old format calls to z_dateformat in z_stdlib.

-module(erlydtl_dateformat).

-export([
    format/2,
    format/3,

    format_utc/2,
    format_utc/3
    ]).

-include_lib("zotonic.hrl").

format(Format, Context) ->
    format(calendar:universal_time(), Format, Context).

format(Date, Format, #context{} = Context) ->
    z_dateformat:format(z_datetime:to_local(Date, Context), Format, opts(Date, z_context:tz(Context), Context)).

format_utc(Format, Context) ->
    format_utc(calendar:universal_time(), Format, Context).

format_utc(Date, Format, #context{} = Context) ->
    z_dateformat:format(Date, Format, opts(Date, "GMT", Context)).

opts(Date, Tz, Context) ->
	[
        {utc, Date},
        {tz, z_convert:to_list(Tz)},
        {tr, {l10n_date, [Context]}}
    ].

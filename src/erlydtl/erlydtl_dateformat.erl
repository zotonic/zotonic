%%% Stub for old format calls to z_dateformat in z_stdlib.

-module(erlydtl_dateformat).

-export([
	format/2,
	format/3
	]).

-include("zotonic.hrl").

format(Format, #context{} = Context) ->
	z_dateformat:format(Format, opts(Context)).

format(Date, Format, #context{} = Context) ->
	z_dateformat:format(Date, Format, opts(Context)).

opts(Context) ->
	[{tr, {l10n_date, [Context]}}].

%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @doc Provides strings for localization of dates.
%% @copyright 2011 Arjan Scherpenisse

-module(l10n_date).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("zotonic.hrl").

-export([
         label/2,
         dayname/2,
         monthname/2
]).

%% @doc Provide some localized date strings
%% @spec label(midnight|noon, #context{}) -> string()
label(midnight, Context) ->
    ?__("midnight", Context);
label(noon, Context) ->
    ?__("noon", Context).

%% @doc Provide localized versions of the day of the week.
dayname(1, Context) -> ?__("Monday", Context);
dayname(2, Context) -> ?__("Tuesday", Context);
dayname(3, Context) -> ?__("Wednesday", Context);
dayname(4, Context) -> ?__("Thursday", Context);
dayname(5, Context) -> ?__("Friday", Context);
dayname(6, Context) -> ?__("Saturday", Context);
dayname(7, Context) -> ?__("Sunday", Context);
dayname(_, _Context) -> "???".

%% @doc Provide localized versions of month names.
monthname(1, Context) ->  ?__("January", Context);
monthname(2, Context) ->  ?__("February", Context);
monthname(3, Context) ->  ?__("March", Context);
monthname(4, Context) ->  ?__("April", Context);
monthname(5, Context) ->  ?__("May", Context);
monthname(6, Context) ->  ?__("June", Context);
monthname(7, Context) ->  ?__("July", Context);
monthname(8, Context) ->  ?__("August", Context);
monthname(9, Context) ->  ?__("September", Context);
monthname(10, Context) -> ?__("October", Context);
monthname(11, Context) -> ?__("November", Context);
monthname(12, Context) -> ?__("December", Context);
monthname(_, _Context) -> "???".


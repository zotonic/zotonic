%% @author Marc Worrell <marc@worrell.nl>
%% @doc Mapping English country name to iso code
%% @copyright 2012 Marc Worrell

-module(l10n_country2iso).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    country2iso/1
]).

country2iso(<<"Bosnia and Herzegovina">>) -> <<"ba">>;
country2iso(<<"British Virgin Islands">>) -> <<"vg">>;
country2iso(<<"Burma">>) -> <<"mm">>;
country2iso(<<"Cote d'Ivoire">>) -> <<"ci">>;
country2iso(<<"Democratic Republic of the Congo">>) -> <<"cd">>;
country2iso(<<"Falkland Islands (Malvinas)">>) -> <<"fk">>;
country2iso(<<"French Guiana">>) -> <<"gf">>;
country2iso(<<"French Polynesia">>) -> <<"pf">>;
country2iso(<<"French Southern and Antarctic Lands">>) -> undefined;
country2iso(<<"Guadeloupe">>) -> <<"gp">>;
country2iso(<<"Guam">>) -> <<"gu">>;
country2iso(<<"Guernsey">>) -> undefined;
country2iso(<<"Guinea-Bissau">>) -> <<"gw">>;
country2iso(<<"Heard Island and McDonald Islands">>) -> <<"hm">>;
country2iso(<<"Holy See (Vatican City)">>) -> <<"va">>;
country2iso(<<"Iran (Islamic Republic of)">>) -> <<"ir">>;
country2iso(<<"Isle of Man">>) -> undefined;
country2iso(<<"Jersey">>) -> undefined;
country2iso(<<"Korea, Democratic People's Republic of">>) -> <<"kp">>;
country2iso(<<"Korea, Republic of">>) -> <<"kr">>;
country2iso(<<"Kyrgyzstan">>) -> <<"kg">>;
country2iso(<<"Lao People's Democratic Republic">>) -> <<"la">>;
country2iso(<<"Libyan Arab Jamahiriya">>) -> <<"ly">>;
country2iso(<<"Martinique">>) -> <<"mq">>;
country2iso(<<"Micronesia, Federated States of">>) -> <<"fm">>;
country2iso(<<"Montenegro">>) -> <<"yu">>;
country2iso(<<"New Caledonia">>) -> <<"nc">>;
country2iso(<<"Palestine">>) -> <<"we">>;  %% TODO: this is also <<"gz">>
country2iso(<<"Pitcairn Islands">>) -> <<"pn">>;
country2iso(<<"Republic of Moldova">>) -> <<"md">>;
country2iso(<<"Reunion">>) -> <<"re">>;
country2iso(<<"Russia">>) -> <<"ru">>;
country2iso(<<"Saint Barthelemy">>) -> undefined;
country2iso(<<"Saint Kitts and Nevis">>) -> <<"kn">>;
country2iso(<<"Saint Martin">>) -> <<"mq">>;
country2iso(<<"Saint Vincent and the Grenadines">>) -> <<"vc">>;
country2iso(<<"Sao Tome and Principe">>) -> <<"st">>;
country2iso(<<"Serbia">>) -> <<"yu">>;
country2iso(<<"Slovakia">>) -> <<"sk">>;
country2iso(<<"South Georgia South Sandwich Islands">>) -> <<"gs">>;
country2iso(<<"Svalbard">>) -> <<"sj">>;
country2iso(<<"Syrian Arab Republic">>) -> <<"sy">>;
country2iso(<<"Tajikistan">>) -> <<"tj">>;
country2iso(<<"The former Yugoslav Republic of Macedonia">>) -> <<"mk">>;
country2iso(<<"Timor-Leste">>) -> <<"tp">>;
country2iso(<<"United Republic of Tanzania">>) -> <<"tz">>;
country2iso(<<"Untied Arab Emirates">>) -> <<"ae">>;
country2iso(<<"United States Virgin Islands">>) -> <<"vi">>;
country2iso(<<"Viet Nam">>) -> <<"vn">>;
country2iso(A) -> 
    case lists:keyfind(A, 2, l10n_iso2country:iso2country()) of
        {Iso, _} -> Iso;
        false -> undefined
    end.

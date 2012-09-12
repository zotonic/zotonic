%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Format a resource record for a browser diff function.

%% Copyright 2012 Marc Worrell
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


-module(backup_format).

-export([
	format/3
	]).

% These fields are shown in this order, any other fields are shown after these.
-define(FIELDS, [
	title,

	category_id, 
	creator_id,
	modifier_id,

	created,
	modified,

	publication_start,
	publication_end,

	is_published,
	is_featured,
	is_protected,

	chapeau,
	subtitle,
	short_title,
	summary,

	name_prefix,
	name_first,
	name_surname_prefix,
	name_surname,

	phone,
	phone_alt,
	phone_emergency,

	email,
	website,

	date_start,
	date_end,
	date_remarks,

	address_street_1,
	address_street_2,
	address_city,
	address_state,
	address_postcode,
	address_country,

	mail_street_1,
	mail_street_2,
	mail_city,
	mail_state,
	mail_postcode,
	mail_country,

	location_lng,
	location_lat,

	body,
	body_extra,
	blocks,

	page_path,
	name,

	seo_noindex,
	seo_title,
	slug,
	custom_slug,
	seo_keywords,
	seo_desc
]).

-include("zotonic.hrl").

format(undefined, List, Context) ->
	format([], List, Context);
format(List, undefined, Context) ->
	format(List, [], Context);
format(ListA, ListB, Context) ->
	AllKeys = lists:usort(proplists:get_keys(ListA) ++ proplists:get_keys(ListB)), 
	Extra = proplists:get_keys(AllKeys) -- ?FIELDS,
	Ks = lists:reverse(?FIELDS ++ Extra),
	fetch(Ks, ListA, ListB, [], Context).


fetch([], _ListA, _ListB, Acc, _Context) ->
	Acc;
fetch([K|Ks], ListA, ListB, Acc, Context) ->
	A = proplists:get_value(K, ListA),
	B = proplists:get_value(K, ListB),
	case {format_value(K, A, Context),
		  format_value(K, B, Context)}
	of 
		{X,X} -> 
			fetch(Ks, ListA, ListB, Acc, Context);
		{FA,FB} ->
			Acc1 = [ {K, FA, FB} | Acc ],
			fetch(Ks, ListA, ListB, Acc1, Context)
	end.

format_value(_K, undefined, _Context) -> 
	<<>>;
format_value(_K, <<>>, _Context) -> 
	<<>>;
format_value(_K, [], _Context) -> 
	<<>>;
format_value(_K, {trans, []}, _Context) -> 
	<<>>;
format_value(_K, {{_,_,_},{_,_,_}} = V, Context) -> 
	erlydtl_dateformat:format(V, "Y-m-d H:i:s", Context);
format_value(_K, V, _Context) when is_binary(V) -> 
	V;
format_value(category_id, Id, Context) ->
	by_id(Id, Context);
format_value(creator_id, Id, Context) ->
	by_id(Id, Context);
format_value(modifier_id, Id, Context) ->
	by_id(Id, Context);
format_value(rsc_id, Id, Context) ->
	by_id(Id, Context);
format_value(blocks, Blocks, Context) ->
	format_blocks(Blocks, Context);
format_value(_, {trans, Tr}, _Context) ->
	iolist_to_binary([ [z_convert:to_binary(Iso), $:, 32, V, <<"\n">>] || {Iso,V} <- Tr, V /= <<>> ]);
format_value(_K, V, _Context) when is_tuple(V) ->
	iolist:to_binary(io_lib:format("~p", [V]));
format_value(_K, V, Context) when is_list(V) ->
	iolist_to_binary(
		z_utils:combine(
			", ",
			[ format_value(none, A, Context) || A <- V ]));
format_value(_K, A, _Context) -> 
	z_convert:to_binary(A).


by_id(Id, Context) when is_integer(Id) ->
	iolist_to_binary([
		z_trans:lookup_fallback(m_rsc:p(Id, title, Context), Context),
		32,
		$(, integer_to_list(Id), $)
	]).


format_blocks(Blocks, Context) ->
	iolist_to_binary(z_utils:combine("\n\n--\n\n", [ format_block(B, Context) || B <- Blocks ])).

format_block(B, Context) ->
	iolist_to_binary(z_utils:combine(
			"\n", 
			[
				iolist_to_binary([z_convert:to_binary(K), ": ", format_value(K,V,Context)])
				|| {K,V} <- B 
			])).


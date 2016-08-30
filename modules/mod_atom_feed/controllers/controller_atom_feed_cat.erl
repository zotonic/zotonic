%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-08-04
%% @doc Serve an atom feed for a category.  Shows the newest created items of a certain category, always as the anonymous user.
%% The category being served must be a name.

%% Copyright 2009 Marc Worrell
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

-module(controller_atom_feed_cat).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    allowed_methods/1,
    content_encodings_provided/1,
	resource_exists/1,
	last_modified/1,
	expires/1,
	content_types_provided/1,
	charsets_provided/1,
	provide_content/1
]).

%% Let cached versions expire in an hour.
-define(MAX_AGE, 3600).

allowed_methods(Context) ->
    {[<<"HEAD">>, <<"GET">>], Context}.

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

content_encodings_provided(Context) ->
    {[<<"identity">>, <<"gzip">>], Context}.

content_types_provided(Context) ->
    {[{<<"application/atom+xml">>, provide_content}], Context}.

resource_exists(Context) ->
    {m_rsc:exists(z_context:get_q(cat, Context), Context), Context}.

last_modified(Context) ->
    MaxAge = integer_to_binary(?MAX_AGE),
    Context1 = z_context:set_resp_header(<<"cache-control">>, <<"public, max-age=", MaxAge/binary>>, Context),
    Modified = case m_category:last_modified(z_context:get_q(cat, Context1), Context1) of
        {ok, Date} -> Date;
        {error, _Reason} -> {{2008,12,10},{15,30,00}}
    end,
    Context2 = z_context:set(last_modified, Modified, Context1),
    {Modified, Context2}.

expires(State) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + ?MAX_AGE), State}.

provide_content(Context) ->
    CatName = m_category:id_to_name(z_context:get_q(cat, Context), Context),
    Vars = [
        {cat, CatName},
        {upcoming, z_context:get(upcoming, Context)},
        {updated, z_context:get(last_modified, Context)},
        {site_url, z_context:abs_url("", Context)}
    ],
    {Content, Context1} = z_template:render_to_iolist("atom_feed_cat.tpl", Vars, Context),
    Content1 = cowmachine_req:encode_content(Content, Context1),
    {Content1, Context1}.


%%%----------------------------------------------------------------------
%%% File    : xml.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XML utils
%%% Created : 20 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(xml).
-author('alexey@process-one.net').

-include("exmpp.hrl").

-export([element_to_string/1,
	 crypt/1, make_text_node/1,
	 remove_cdata/1,
	 get_cdata/1, get_tag_cdata/1,
	 get_attr/2, get_attr_s/2,
	 get_tag_attr/2, get_tag_attr_s/2,
	 get_subtag/2, get_subtag_cdata/2,
	 get_path_s/2,
	 replace_tag_attr/3]).

%% Select at compile time how to escape characters in binary text
%% nodes.
%% Can be choosen with ./configure --enable-full-xml
-ifdef(FULL_XML_SUPPORT).
-define(ESCAPE_BINARY(CData), make_text_node(CData)).
-else.
-define(ESCAPE_BINARY(CData), crypt(CData)).
-endif.

element_to_string(El) ->
    exmpp_xml:document_to_list(El).

crypt(S) ->
    exmpp_xml:escape_using_entities(S).

%% Make a cdata_binary depending on what characters it contains
make_text_node(CData) ->
    exmpp_xml:escape_using_cdata(CData).

remove_cdata(L) -> exmpp_xml:remove_cdata_from_list(L).

get_cdata(L) ->
    exmpp_xml:get_cdata_from_list_as_list(L).

get_tag_cdata({xmlelement, _Name, _Attrs, Els}) ->
    get_cdata(Els).

get_attr(AttrName, Attrs) ->
    case exmpp_xml:get_attribute_from_list_as_list(Attrs, AttrName,
						   undefined) of
	undefined ->
	    false;
	Val ->
	    {value, Val}
    end.

get_attr_s(AttrName, Attrs) ->
    exmpp_xml:get_attribute_from_list_as_list(Attrs, AttrName, "").

get_tag_attr(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr(AttrName, Attrs);
get_tag_attr(AttrName, #xmlelement{attrs = Attrs}) ->
    get_attr(AttrName, Attrs).

get_tag_attr_s(AttrName, El) ->
    exmpp_xml:get_attribute_as_list(El, AttrName, "").


get_subtag(El, Name) ->
    case exmpp_xml:get_element(El, Name) of
	undefined -> false;
	Sub_El    -> Sub_El
    end.

get_subtag_cdata(Tag, Name) ->
    exmpp_xml:get_cdata(Tag, Name).

get_path_s(El, []) ->
    El;
get_path_s(El, [{elem, Name} | Path]) ->
    case get_subtag(El, Name) of
	false ->
	    "";
	SubEl ->
	    get_path_s(SubEl, Path)
    end;
get_path_s(El, [{attr, Name}]) ->
    get_tag_attr_s(Name, El);
get_path_s(El, [cdata]) ->
    get_tag_cdata(El).


replace_tag_attr(Attr, Value, El) ->
    exmpp_xml:set_attribute(El, Attr, Value).



%%%-------------------------------------------------------------------
%%% File:      erlydtl_parser.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc Template language grammar
%%% @reference  See <a href="http://erlydtl.googlecode.com" target="_top">http://erlydtl.googlecode.com</a> for more information
%%% @changes Marc Worrell - added print/image/scomp, more args options etc.
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
%%% Adapted and expanded for Zotonic by Marc Worrell <marc@worrell.nl>
%%%-------------------------------------------------------------------

Nonterminals
    Elements
    Literal

    ValueBraced

    ExtendsTag
    IncludeTag
    NowTag

    BlockBlock
    BlockBraced
    EndBlockBraced

    CommentBlock
    CommentBraced
    EndCommentBraced

    CycleTag
    CycleNames
    CycleNamesCompat

    ForBlock
    ForBraced
	EmptyBraced
    EndForBraced
    ForExpression
    ForGroup

    IfBlock
    IfBraced
    IfExpression
    ElseBraced
    EndIfBraced

	IfSimpleExpression
	IfAndOrExpression
	IfOrExpression
	IfAndExpression
    
    IfEqualBlock
    IfEqualBraced
    IfEqualExpression
    EndIfEqualBraced  
    
    IfNotEqualBlock
    IfNotEqualBraced
    IfNotEqualExpression
    EndIfNotEqualBraced      

    AutoEscapeBlock
    AutoEscapeBraced
    EndAutoEscapeBraced

	WithBlock
	WithBraced
	EndWithBraced
	
    Value
	TermValue
    Variable
    Filter
	AutoId
	
	LibTag
	LibList
	
    LoadTag
    LoadNames
    
    CustomTag
    Args

    CallTag
    CallWithTag

	CacheBlock
	CacheBraced
	EndCacheBraced
	OptCacheTime

	UrlTag
	PrintTag
	ImageTag
	ImageUrlTag
	MediaTag
	TransTag
	TransExtTag
	ValueList

	OptionalAll.

Terminals
	all_keyword
	as_keyword
    autoescape_keyword
    block_keyword
	cache_keyword
    call_keyword
    close_tag
    close_var
    comment_keyword
    colon
    comma
    cycle_keyword
    dot
    else_keyword
	empty_keyword
    endautoescape_keyword
    endblock_keyword
	endcache_keyword
    endcomment_keyword
    endfor_keyword
    endif_keyword
    endifequal_keyword
    endifnotequal_keyword
	endwith_keyword
    equal
    extends_keyword
    for_keyword
    identifier
    if_keyword
    ifequal_keyword
    ifnotequal_keyword
	image_keyword
	image_url_keyword
    in_keyword
    include_keyword
	lib_keyword
    load_keyword
	media_keyword
    not_keyword
    now_keyword
    number_literal
    open_tag
    open_var
    pipe
    print_keyword
    string_literal
    text
	url_keyword
    with_keyword
	open_curly
	close_curly
	open_bracket
	close_bracket
	open_trans
	trans_text
	close_trans
	trans_literal
	or_keyword
	and_keyword
	'__keyword'
	model_index
 	hash.

Rootsymbol
    Elements.

Expect 1.

Elements -> '$empty' : [].
Elements -> Elements text : '$1' ++ ['$2'].
Elements -> Elements ValueBraced : '$1' ++ ['$2'].
Elements -> Elements TransTag : '$1' ++ ['$2'].
Elements -> Elements ExtendsTag : '$1' ++ ['$2'].
Elements -> Elements IncludeTag : '$1' ++ ['$2'].
Elements -> Elements NowTag : '$1' ++ ['$2'].
Elements -> Elements LibTag : '$1' ++ ['$2'].
Elements -> Elements LoadTag : '$1' ++ ['$2'].
Elements -> Elements CycleTag : '$1' ++ ['$2'].
Elements -> Elements BlockBlock : '$1' ++ ['$2'].
Elements -> Elements ForBlock : '$1' ++ ['$2'].
Elements -> Elements IfBlock : '$1' ++ ['$2'].
Elements -> Elements IfEqualBlock : '$1' ++ ['$2'].
Elements -> Elements IfNotEqualBlock : '$1' ++ ['$2'].
Elements -> Elements AutoEscapeBlock : '$1' ++ ['$2'].
Elements -> Elements CommentBlock : '$1' ++ ['$2'].
Elements -> Elements CustomTag : '$1' ++ ['$2'].
Elements -> Elements CallTag : '$1' ++ ['$2'].
Elements -> Elements CallWithTag : '$1' ++ ['$2'].
Elements -> Elements UrlTag : '$1' ++ ['$2'].
Elements -> Elements PrintTag : '$1' ++ ['$2'].
Elements -> Elements ImageTag : '$1' ++ ['$2'].
Elements -> Elements ImageUrlTag : '$1' ++ ['$2'].
Elements -> Elements MediaTag : '$1' ++ ['$2'].
Elements -> Elements TransExtTag : '$1' ++ ['$2'].
Elements -> Elements WithBlock : '$1' ++ ['$2'].
Elements -> Elements CacheBlock : '$1' ++ ['$2'].


ValueBraced -> open_var Value close_var : '$2'.

TransTag -> open_trans trans_text close_trans : {trans, '$2'}.
TransExtTag -> open_tag '__keyword' string_literal Args close_tag : {trans_ext, '$3', '$4'}.
ExtendsTag -> open_tag extends_keyword string_literal close_tag : {extends, '$3'}.
IncludeTag -> open_tag OptionalAll include_keyword string_literal Args close_tag : {include, '$4', '$5', '$2'}.
NowTag -> open_tag now_keyword string_literal close_tag : {date, now, '$3'}.

OptionalAll -> all_keyword : true.
OptionalAll -> '$empty' : false.

LibTag -> open_tag lib_keyword LibList close_tag : {lib, '$3'}.
LibList -> string_literal : ['$1'].
LibList -> LibList string_literal : '$1' ++ ['$2'].

LoadTag -> open_tag load_keyword LoadNames close_tag : {load, '$3'}.
LoadNames -> identifier : ['$1'].
LoadNames -> LoadNames identifier : '$1' ++ ['$2'].

BlockBlock -> BlockBraced Elements EndBlockBraced : {block, '$1', '$2'}.
BlockBraced -> open_tag block_keyword identifier close_tag : '$3'.
EndBlockBraced -> open_tag endblock_keyword close_tag.

CommentBlock -> CommentBraced Elements EndCommentBraced : {comment, '$2'}.
CommentBraced -> open_tag comment_keyword close_tag.
EndCommentBraced -> open_tag endcomment_keyword close_tag.

CycleTag -> open_tag cycle_keyword CycleNamesCompat close_tag : {cycle_compat, '$3'}.
CycleTag -> open_tag cycle_keyword CycleNames close_tag : {cycle, '$3'}.

CycleNames -> Value : ['$1'].
CycleNames -> CycleNames Value : '$1' ++ ['$2'].

CycleNamesCompat -> identifier comma : ['$1'].
CycleNamesCompat -> CycleNamesCompat identifier comma : '$1' ++ ['$2'].
CycleNamesCompat -> CycleNamesCompat identifier : '$1' ++ ['$2'].

ForBlock -> ForBraced Elements EndForBraced : {for, '$1', '$2'}.
ForBlock -> ForBraced Elements EmptyBraced Elements EndForBraced : {for, '$1', '$2', '$4'}.
EmptyBraced -> open_tag empty_keyword close_tag.
ForBraced -> open_tag for_keyword ForExpression close_tag : '$3'.
EndForBraced -> open_tag endfor_keyword close_tag.
ForExpression -> ForGroup in_keyword Value : {'in', '$1', '$3'}.
ForGroup -> identifier : ['$1'].
ForGroup -> ForGroup comma identifier : '$1' ++ ['$3'].

IfBlock -> IfBraced Elements ElseBraced Elements EndIfBraced : {ifelse, '$1', '$2', '$4'}.
IfBlock -> IfBraced Elements EndIfBraced : {'if', '$1', '$2'}.
IfBraced -> open_tag if_keyword IfExpression close_tag : '$3'.

IfExpression -> IfSimpleExpression IfAndOrExpression : {'expr', '$1', '$2'}.

IfAndOrExpression -> '$empty' : none.
IfAndOrExpression -> or_keyword IfSimpleExpression IfOrExpression : {'or', ['$2' | '$3']}.
IfAndOrExpression -> and_keyword IfSimpleExpression IfAndExpression : {'and', ['$2' | '$3']}.

IfOrExpression -> '$empty' : [].
IfOrExpression -> or_keyword IfSimpleExpression IfOrExpression : ['$2' | '$3'].

IfAndExpression -> '$empty' : [].
IfAndExpression -> and_keyword IfSimpleExpression IfAndExpression : ['$2' | '$3'].

IfSimpleExpression -> not_keyword IfSimpleExpression : {'not', '$2'}.
IfSimpleExpression -> Value : '$1'.

ElseBraced -> open_tag else_keyword close_tag.
EndIfBraced -> open_tag endif_keyword close_tag.

IfEqualBlock -> IfEqualBraced Elements ElseBraced Elements EndIfEqualBraced : {ifequalelse, '$1', '$2', '$4'}.
IfEqualBlock -> IfEqualBraced Elements EndIfEqualBraced : {ifequal, '$1', '$2'}.
IfEqualBraced -> open_tag ifequal_keyword IfEqualExpression Value close_tag : ['$3', '$4'].
IfEqualExpression -> Value : '$1'.
EndIfEqualBraced -> open_tag endifequal_keyword close_tag.

IfNotEqualBlock -> IfNotEqualBraced Elements ElseBraced Elements EndIfNotEqualBraced : {ifnotequalelse, '$1', '$2', '$4'}.
IfNotEqualBlock -> IfNotEqualBraced Elements EndIfNotEqualBraced : {ifnotequal, '$1', '$2'}.
IfNotEqualBraced -> open_tag ifnotequal_keyword IfNotEqualExpression Value close_tag : ['$3', '$4'].
IfNotEqualExpression -> Value : '$1'.
EndIfNotEqualBraced -> open_tag endifnotequal_keyword close_tag.

AutoEscapeBlock -> AutoEscapeBraced Elements EndAutoEscapeBraced : {autoescape, '$1', '$2'}.
AutoEscapeBraced -> open_tag autoescape_keyword identifier close_tag : '$3'.
EndAutoEscapeBraced -> open_tag endautoescape_keyword close_tag.

WithBlock -> WithBraced Elements EndWithBraced : {with, '$1', '$2'}.
WithBraced -> open_tag with_keyword Value as_keyword ForGroup close_tag : ['$3', '$5'].
EndWithBraced -> open_tag endwith_keyword close_tag.

CacheBlock -> CacheBraced Elements EndCacheBraced : {cache, '$1', '$2'}.
CacheBraced -> open_tag cache_keyword OptCacheTime Args close_tag : [ '$3', '$4' ].
EndCacheBraced -> open_tag endcache_keyword close_tag.

OptCacheTime -> '$empty' : undefined.
OptCacheTime -> number_literal : '$1'.

Filter -> identifier : ['$1'].
Filter -> identifier colon TermValue : ['$1', '$3'].

Literal -> string_literal : '$1'.
Literal -> trans_literal  : '$1'.
Literal -> number_literal : '$1'.

CustomTag -> open_tag OptionalAll identifier Args close_tag : {tag, '$3', '$4', '$2'}.

CallTag -> open_tag call_keyword identifier close_tag : {call, '$3'}.
CallWithTag -> open_tag call_keyword identifier with_keyword Value close_tag : {call, '$3', '$5'}.

ImageTag -> open_tag image_keyword Value Args close_tag : {image, '$3', '$4' }.
ImageUrlTag -> open_tag image_url_keyword Value Args close_tag : {image_url, '$3', '$4' }.

MediaTag -> open_tag media_keyword Value Args close_tag : {media, '$3', '$4' }.

UrlTag -> open_tag url_keyword identifier Args close_tag : {url, '$3', '$4'}.

PrintTag -> open_tag print_keyword Value close_tag : {print, '$3'}.

Args -> '$empty' : [].
Args -> Args identifier : '$1' ++ [{'$2', true}].
Args -> Args identifier equal Value : '$1' ++ [{'$2', '$4'}].

Value -> Value pipe Filter : {apply_filter, '$1', '$3'}.
Value -> TermValue : '$1'.

TermValue -> Variable : '$1'.
TermValue -> Literal : '$1'.
TermValue -> hash AutoId : {auto_id, '$2'}.
TermValue -> open_curly identifier Args close_curly : {tuple_value, '$2', '$3'}.
TermValue -> open_bracket ValueList close_bracket : {value_list, '$2'}.

AutoId -> identifier dot identifier : { '$1', '$3' }.
AutoId -> identifier : '$1'.

Variable -> model_index identifier : {model, '$2'}.
Variable -> identifier : {variable, '$1'}.
Variable -> Variable open_bracket Value close_bracket : {index_value, '$1', '$3'}.
Variable -> Variable dot identifier : {attribute, {'$3', '$1'}}.

ValueList -> Value : ['$1'].
ValueList -> ValueList comma Value : '$1' ++ ['$3'].

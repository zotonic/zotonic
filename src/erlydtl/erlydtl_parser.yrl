%%%-------------------------------------------------------------------
%%% File:      erlydtl_parser.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @copyright 2009-2011 Marc Worrell
%%% @doc Template language grammar
%%% @reference  See <a href="http://erlydtl.googlecode.com" target="_top">http://erlydtl.googlecode.com</a> for more information
%%% @changes Marc Worrell - added print/image/scomp, more args options etc.
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%% Copyright (c) 2009-2011 Marc Worrell
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
    OptWith

    ExtendsTag
    OverrulesTag
    InheritTag
    
    IncludeTag
    CatIncludeTag
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

    FilterBlock
    FilterBraced
    EndFilterBraced
    Filters

    ForBlock
    ForBraced
    EmptyBraced
    EndForBraced
    ForExpression
    ForGroup

    IfBlock
    IfBraced
    ElsePart
    ElseIfList
    ElseIfBraced
    ElseBraced
    EndIfBraced

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
    FilterArgs
	AutoId
	
	LibTag
	LibList
	
    LoadTag
    LoadNames
    
    CustomTag
    Args
    SpacelessBlock
	TransArgs

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
    OptArrayList
    ArrayList
    
	OptionalAll
	
	OptE
	E
	Uminus
	Unot.

Terminals
	all_keyword
	as_keyword
	atom_literal
    autoescape_keyword
    block_keyword
	cache_keyword
    call_keyword
	catinclude_keyword
    close_tag
    close_var
    comment_keyword
    colon
    comma
    cycle_keyword
    dot
    else_keyword
    elseif_keyword
    empty_keyword
    endautoescape_keyword
    endblock_keyword
	endcache_keyword
    endcomment_keyword
    endfilter_keyword
    endfor_keyword
    endif_keyword
    endifequal_keyword
    endifnotequal_keyword
    endspaceless_keyword
	endwith_keyword
    equal
    extends_keyword
    filter_keyword
    for_keyword
    identifier
    if_keyword
    ifequal_keyword
    ifnotequal_keyword
	image_keyword
	image_url_keyword
    in_keyword
    include_keyword
    inherit_keyword
	lib_keyword
    load_keyword
	media_keyword
    not_keyword
    now_keyword
    number_literal
    open_tag
    open_var
    overrules_keyword
    pipe
    print_keyword
    spaceless_keyword
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
	xor_keyword
	and_keyword
	__keyword
 	hash
	'==' '/=' '<' '>' '=<' '>='
	'+' '-'
	'*' '/' '%'
	'(' ')'.

Rootsymbol
    Elements.

%% Operator precedences for the E non terminal
Left 100 or_keyword.
Left 105 xor_keyword.
Left 110 and_keyword.
Nonassoc 300 '==' '/=' '<' '>' '=<' '>='.
Left 400 '+' '-'.
Left 500 '*' '/' '%'.
Unary 600 Uminus Unot.

%% Expected shift/reduce conflicts
Expect 1.

Elements -> '$empty' : [].
Elements -> Elements text : '$1' ++ ['$2'].
Elements -> Elements ValueBraced : '$1' ++ ['$2'].
Elements -> Elements TransTag : '$1' ++ ['$2'].
Elements -> Elements TransExtTag : '$1' ++ ['$2'].
Elements -> Elements ExtendsTag : '$1' ++ ['$2'].
Elements -> Elements OverrulesTag : '$1' ++ ['$2'].
Elements -> Elements InheritTag : '$1' ++ ['$2'].
Elements -> Elements IncludeTag : '$1' ++ ['$2'].
Elements -> Elements CatIncludeTag : '$1' ++ ['$2'].
Elements -> Elements NowTag : '$1' ++ ['$2'].
Elements -> Elements SpacelessBlock : '$1' ++ ['$2'].
Elements -> Elements LibTag : '$1' ++ ['$2'].
Elements -> Elements LoadTag : '$1' ++ ['$2'].
Elements -> Elements CycleTag : '$1' ++ ['$2'].
Elements -> Elements BlockBlock : '$1' ++ ['$2'].
Elements -> Elements FilterBlock : '$1' ++ ['$2'].
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
Elements -> Elements WithBlock : '$1' ++ ['$2'].
Elements -> Elements CacheBlock : '$1' ++ ['$2'].


ValueBraced -> open_var E OptWith close_var : {value, '$2', '$3'}.

OptWith -> '$empty' : [].
OptWith -> with_keyword Args : '$2'.

ExtendsTag -> open_tag extends_keyword string_literal close_tag : {extends, '$3'}.
OverrulesTag -> open_tag overrules_keyword close_tag : overrules.
InheritTag -> open_tag inherit_keyword close_tag : inherit.

TransTag -> open_trans trans_text close_trans : {trans, '$2'}.
TransExtTag -> open_tag __keyword string_literal TransArgs close_tag : {trans_ext, '$3', '$4'}.
IncludeTag -> open_tag OptionalAll include_keyword E Args close_tag : {include, '$4', '$5', '$2'}.
CatIncludeTag -> open_tag OptionalAll catinclude_keyword E E Args close_tag : {catinclude, '$4', '$5', '$6', '$2'}.
NowTag -> open_tag now_keyword string_literal close_tag : {date, now, '$3'}.

OptionalAll -> all_keyword : true.
OptionalAll -> '$empty' : false.

LibTag -> open_tag lib_keyword LibList Args close_tag : {lib, '$3', '$4'}.
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

FilterBlock -> FilterBraced Elements EndFilterBraced : {filter, '$1', '$2'}.
FilterBraced -> open_tag filter_keyword Filters close_tag : '$3'.
EndFilterBraced -> open_tag endfilter_keyword close_tag.

Filters -> Filter : ['$1'].
Filters -> Filters pipe Filter : '$1' ++ ['$3'].

ForBlock -> ForBraced Elements EndForBraced : {for, '$1', '$2'}.
ForBlock -> ForBraced Elements EmptyBraced Elements EndForBraced : {for, '$1', '$2', '$4'}.
EmptyBraced -> open_tag empty_keyword close_tag.
ForBraced -> open_tag for_keyword ForExpression close_tag : '$3'.
EndForBraced -> open_tag endfor_keyword close_tag.
ForExpression -> ForGroup in_keyword E : {'in', '$1', '$3'}.
ForGroup -> identifier : ['$1'].
ForGroup -> ForGroup comma identifier : '$1' ++ ['$3'].

IfBlock -> IfBraced Elements ElsePart : {'if', '$1', '$2', '$3'}.

ElsePart -> EndIfBraced : [].
ElsePart -> ElseBraced Elements EndIfBraced : [{'else', '$2'}].
ElsePart -> ElseIfList : '$1'.

ElseIfList -> ElseIfBraced Elements ElsePart : [{'elseif', '$1', '$2'}] ++ '$3'.

IfBraced -> open_tag if_keyword E close_tag : '$3'.
ElseIfBraced -> open_tag elseif_keyword E close_tag : '$3'.
ElseBraced -> open_tag else_keyword close_tag.
EndIfBraced -> open_tag endif_keyword close_tag.

IfEqualBlock -> IfEqualBraced Elements ElseBraced Elements EndIfEqualBraced : {ifequalelse, '$1', '$2', '$4'}.
IfEqualBlock -> IfEqualBraced Elements EndIfEqualBraced : {ifequal, '$1', '$2'}.
IfEqualBraced -> open_tag ifequal_keyword IfEqualExpression E close_tag : ['$3', '$4'].
IfEqualExpression -> E : '$1'.
EndIfEqualBraced -> open_tag endifequal_keyword close_tag.

IfNotEqualBlock -> IfNotEqualBraced Elements ElseBraced Elements EndIfNotEqualBraced : {ifnotequalelse, '$1', '$2', '$4'}.
IfNotEqualBlock -> IfNotEqualBraced Elements EndIfNotEqualBraced : {ifnotequal, '$1', '$2'}.
IfNotEqualBraced -> open_tag ifnotequal_keyword IfNotEqualExpression E close_tag : ['$3', '$4'].
IfNotEqualExpression -> E : '$1'.
EndIfNotEqualBraced -> open_tag endifnotequal_keyword close_tag.

SpacelessBlock -> open_tag spaceless_keyword close_tag Elements open_tag endspaceless_keyword close_tag : {spaceless, '$4'}.

AutoEscapeBlock -> AutoEscapeBraced Elements EndAutoEscapeBraced : {autoescape, '$1', '$2'}.
AutoEscapeBraced -> open_tag autoescape_keyword identifier close_tag : '$3'.
EndAutoEscapeBraced -> open_tag endautoescape_keyword close_tag.

WithBlock -> WithBraced Elements EndWithBraced : {with, '$1', '$2'}.
WithBraced -> open_tag with_keyword ValueList as_keyword ForGroup close_tag : ['$3', '$5'].
EndWithBraced -> open_tag endwith_keyword close_tag.

CacheBlock -> CacheBraced Elements EndCacheBraced : {cache, '$1', '$2'}.
CacheBraced -> open_tag cache_keyword OptCacheTime Args close_tag : [ '$3', '$4' ].
EndCacheBraced -> open_tag endcache_keyword close_tag.

OptCacheTime -> '$empty' : undefined.
OptCacheTime -> number_literal : '$1'.

Filter -> identifier FilterArgs: {filter, '$1', '$2'}.
FilterArgs -> '$empty' : [].
FilterArgs -> FilterArgs colon TermValue : '$1' ++ ['$3'].

Literal -> string_literal : '$1'.
Literal -> trans_literal  : '$1'.
Literal -> number_literal : '$1'.
Literal -> atom_literal : '$1'.

CustomTag -> open_tag OptionalAll identifier Args close_tag : {tag, '$3', '$4', '$2'}.

CallTag -> open_tag call_keyword identifier Args close_tag : {call_args, '$3', '$4'}.
CallWithTag -> open_tag call_keyword identifier with_keyword E close_tag : {call_with, '$3', '$5'}.

ImageTag -> open_tag image_keyword E Args close_tag : {image, '$3', '$4' }.
ImageUrlTag -> open_tag image_url_keyword Value Args close_tag : {image_url, '$3', '$4' }.

MediaTag -> open_tag media_keyword E Args close_tag : {media, '$3', '$4' }.

UrlTag -> open_tag url_keyword identifier Args close_tag : {url, '$3', '$4'}.

PrintTag -> open_tag print_keyword E close_tag : {print, '$3'}.

TransArgs -> '$empty' : [].
TransArgs -> TransArgs identifier equal string_literal : '$1' ++ [{'$2', '$4'}].

Args -> '$empty' : [].
Args -> Args identifier : '$1' ++ [{'$2', true}].
Args -> Args identifier equal E : '$1' ++ [{'$2', '$4'}].

Value -> Value pipe Filter : {apply_filter, '$1', '$3'}.
Value -> TermValue : '$1'.

TermValue -> '(' E ')' : '$2'.
TermValue -> Variable : '$1'.
TermValue -> Literal : '$1'.
TermValue -> hash AutoId : {auto_id, '$2'}.
TermValue -> open_curly identifier Args close_curly : {tuple_value, '$2', '$3'}.
TermValue -> open_bracket OptArrayList close_bracket : {value_list, '$2'}.

AutoId -> identifier dot identifier : { '$1', '$3' }.
AutoId -> identifier : '$1'.

Variable -> identifier : {variable, '$1'}.
Variable -> Variable open_bracket Value close_bracket : {index_value, '$1', '$3'}.
Variable -> Variable dot identifier : {attribute, {'$3', '$1'}}.

ValueList -> E : ['$1'].
ValueList -> ValueList comma E : '$1' ++ ['$3'].

OptArrayList -> '$empty' : [].
OptArrayList -> E : ['$1'].
OptArrayList -> E comma ArrayList : ['$1'|'$3'].
OptArrayList -> comma ArrayList : [undefined|'$2'].

ArrayList -> OptE : ['$1'].
ArrayList -> ArrayList comma OptE : '$1' ++ ['$3'].

OptE -> '$empty': undefined.
OptE -> E : '$1'.


%%% Expressions

E -> E or_keyword E  : {expr, "or", '$1', '$3'}.
E -> E xor_keyword E  : {expr, "xor", '$1', '$3'}.
E -> E and_keyword E  : {expr, "and", '$1', '$3'}.
E -> E '==' E  : {expr, "eq", '$1', '$3'}.
E -> E '/=' E  : {expr, "ne", '$1', '$3'}.
E -> E '<' E  : {expr, "lt", '$1', '$3'}.
E -> E '>' E  : {expr, "gt", '$1', '$3'}.
E -> E '=<' E  : {expr, "le", '$1', '$3'}.
E -> E '>=' E  : {expr, "ge", '$1', '$3'}.
E -> E '+' E  : {expr, "add", '$1', '$3'}.
E -> E '-' E  : {expr, "sub", '$1', '$3'}.
E -> E '*' E  : {expr, "multiply", '$1', '$3'}.
E -> E '/' E  : {expr, "divide", '$1', '$3'}.
E -> E '%' E  : {expr, "modulo", '$1', '$3'}.
E -> Uminus : '$1'.
E -> Unot : '$1'.
E -> Value : '$1'.

Uminus -> '-' E : {expr, "negate", '$2'}.
Unot -> not_keyword E : {expr, "not", '$2'}.

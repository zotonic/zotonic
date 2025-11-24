%% @copyright 2025 Marc Worrell
%% @doc SPARQL parser, based on the W3C 1.1 EBNF grammar.
%% Lexer emits atoms like 'select', 'where', 'var1', 'iri_ref', 'string_literal1', etc.
%% With values as `{Token, Line, Value}`.
%% @end

Nonterminals
    query prologue decls decl
    select_query construct_query describe_query ask_query
    dataset_clauses dataset_clause
    where_clause solution_modifier
    group_clause having_clause having_conditions having_condition
    order_clause order_conditions order_condition
    limit_offset_clause limit_clause offset_clause
    var_list var_or_star select_items select_item
    group_graph_pattern group_graph_pattern_sub triples_block
    graph_pattern_not_triples optional_graph_pattern group_graph_pattern_inner
    union_graph_pattern filter_pattern bind_pattern inline_data
    triples_same_subject blank_node_property_list
    property_list_not_empty property_list object_list object
    verb var_or_term var iri_term prefixed_name
    expression conditional_or_expression conditional_and_expression
    relational_expression additive_expression multiplicative_expression
    unary_expression primary_expression
    bracketted_expression built_in_call built_in_function arg_list
    aggregate aggregate_distinct aggregate_separator
    rdf_literal numeric_literal boolean_literal string.

Terminals
    select construct describe ask where distinct reduced from named graph optional
    union filter bind as values
    group by having order asc desc limit offset base prefix a
    var1 var2 iri_ref pname_ns pname_ln blank_node_label anon nil
    integer decimal double integer_positive decimal_positive double_positive
    integer_negative decimal_negative double_negative
    string_literal1 string_literal2 string_literal_long1 string_literal_long2
    langtag true false
    lbrace rbrace lparen rparen lbracket rbracket dot comma semicolon
    plus minus star slash bang eq ne lt gt le ge andand oror
    hat hat2
    str lang langmatches datatype bound iri uri bnode rand abs ceil floor round
    concat substr strlen replace ucase lcase encode_for_uri contains strstarts
    strends strbefore strafter year month day hours minutes seconds timezone tz
    now uuid struuid md5 sha1 sha256 sha384 sha512 coalesce 'if' strlang strdt
    sameterm isiri isuri isblank isliteral isnumeric regex
    count sum min max avg sample group_concat separator.

Rootsymbol
    query.

%% Expected shift/reduce conflicts
Expect 15.

%% ---------- Queries ----------

query -> prologue select_query :
    {query, '$1', '$2'}.
query -> prologue construct_query :
    {query, '$1', '$2'}.
query -> prologue describe_query :
    {query, '$1', '$2'}.
query -> prologue ask_query :
    {query, '$1', '$2'}.

prologue -> decls : '$1'.
decls -> '$empty' : [].
decls -> decls decl : '$1' ++ ['$2'].

decl -> base iri_ref :
    {base, unwrap('$2')}.
decl -> prefix pname_ns iri_ref :
    {prefix, unwrap('$2'), unwrap('$3')}.

select_query -> select var_or_star dataset_clauses where_clause solution_modifier :
    {select, default, '$2', '$3', '$4', '$5'}.
select_query -> select distinct var_or_star dataset_clauses where_clause solution_modifier :
    {select, distinct, '$3', '$4', '$5', '$6'}.
select_query -> select reduced var_or_star dataset_clauses where_clause solution_modifier :
    {select, reduced, '$3', '$4', '$5', '$6'}.

construct_query -> construct group_graph_pattern dataset_clauses where_clause solution_modifier :
    {construct, '$2', '$3', '$4', '$5'}.

describe_query -> describe var_list dataset_clauses where_clause solution_modifier :
    {describe, '$2', '$3', '$4', '$5'}.

ask_query -> ask dataset_clauses where_clause solution_modifier :
    {ask, '$2', '$3', '$4'}.

var_or_star -> star : all.
var_or_star -> select_items : '$1'.

select_items -> select_item : ['$1'].
select_items -> select_items select_item : '$1' ++ ['$2'].

select_item -> var : '$1'.
select_item -> lparen expression as var rparen :
    {as, '$2', '$4'}.

var_list -> var : ['$1'].
var_list -> var_list var : '$1' ++ ['$2'].

dataset_clauses -> '$empty' : [].
dataset_clauses -> dataset_clauses dataset_clause : '$1' ++ ['$2'].

dataset_clause -> from iri_term :
    {from, '$2'}.
dataset_clause -> from named iri_term :
    {from_named, '$3'}.

where_clause -> group_graph_pattern : '$1'.
where_clause -> where group_graph_pattern : '$2'.

solution_modifier -> group_clause having_clause order_clause limit_offset_clause :
    {solution_modifier, '$1', '$2', '$3', '$4'}.

group_clause -> '$empty' : [].
group_clause -> group by var_list : '$3'.

having_clause -> '$empty' : [].
having_clause -> having having_conditions : '$2'.

having_conditions -> having_condition : ['$1'].
having_conditions -> having_conditions having_condition : '$1' ++ ['$2'].

having_condition -> bracketted_expression : '$1'.
having_condition -> built_in_call : '$1'.

order_clause -> '$empty' : [].
order_clause -> order by order_conditions : '$3'.

order_conditions -> order_condition : ['$1'].
order_conditions -> order_conditions order_condition : '$1' ++ ['$2'].

order_condition -> expression : {order, default, '$1'}.
order_condition -> asc lparen expression rparen : {order, asc, '$3'}.
order_condition -> desc lparen expression rparen : {order, desc, '$3'}.

limit_offset_clause -> '$empty' : [].
limit_offset_clause -> limit_clause : ['$1'].
limit_offset_clause -> offset_clause : ['$1'].
limit_offset_clause -> limit_clause offset_clause : ['$1', '$2'].
limit_offset_clause -> offset_clause limit_clause : ['$1', '$2'].

limit_clause -> limit integer : {limit, unwrap('$2')}.
offset_clause -> offset integer : {offset, unwrap('$2')}.

%% ---------- Graph patterns ----------

group_graph_pattern -> lbrace group_graph_pattern_sub rbrace :
    {group, '$2'}.

group_graph_pattern_sub -> '$empty' : [].
group_graph_pattern_sub -> triples_block : '$1'.
group_graph_pattern_sub -> group_graph_pattern_sub graph_pattern_not_triples dot :
    '$1' ++ ['$2'].
group_graph_pattern_sub -> group_graph_pattern_sub graph_pattern_not_triples :
    '$1' ++ ['$2'].

triples_block -> triples_same_subject :
    [{triple_pattern, '$1'}].
triples_block -> triples_same_subject dot :
    [{triple_pattern, '$1'}].
triples_block -> triples_same_subject dot triples_block :
    [{triple_pattern, '$1'} | '$3'].

graph_pattern_not_triples -> optional_graph_pattern : '$1'.
graph_pattern_not_triples -> group_graph_pattern_inner : '$1'.
graph_pattern_not_triples -> union_graph_pattern : '$1'.
graph_pattern_not_triples -> filter_pattern : '$1'.
graph_pattern_not_triples -> bind_pattern : '$1'.
graph_pattern_not_triples -> inline_data : '$1'.

optional_graph_pattern -> optional group_graph_pattern :
    {optional, '$2'}.

group_graph_pattern_inner -> group_graph_pattern :
    '$1'.
group_graph_pattern_inner -> graph var_or_term group_graph_pattern :
    {graph, '$2', '$3'}.

union_graph_pattern -> group_graph_pattern union group_graph_pattern :
    {union, ['$1', '$3']}.
union_graph_pattern -> union_graph_pattern union group_graph_pattern :
    append_union('$1', '$3').

filter_pattern -> filter expression :
    {filter, '$2'}.

bind_pattern -> bind lparen expression as var rparen :
    {bind, '$3', '$5'}.

inline_data -> values var lbrace object_list rbrace :
    {values, '$2', '$4'}.

%% ---------- Triples ----------

triples_same_subject -> var_or_term property_list_not_empty :
    {subject, '$1', '$2'}.
triples_same_subject -> blank_node_property_list property_list :
    {subject, '$1', '$2'}.

blank_node_property_list -> lbracket property_list_not_empty rbracket :
    {blank_node_property_list, '$2'}.

property_list_not_empty -> verb object_list :
    [{predicate, '$1', '$2'}].
property_list_not_empty -> verb object_list semicolon property_list :
    [{predicate, '$1', '$2'} | '$4'].

property_list -> '$empty' : [].
property_list -> property_list_not_empty : '$1'.

object_list -> object : ['$1'].
object_list -> object_list comma object : '$1' ++ ['$3'].

object -> var_or_term : '$1'.
object -> blank_node_property_list : '$1'.

verb -> var_or_term : '$1'.
verb -> a : rdf_type.

var_or_term -> var : '$1'.
var_or_term -> iri_term : '$1'.
var_or_term -> rdf_literal : '$1'.
var_or_term -> numeric_literal : '$1'.
var_or_term -> boolean_literal : '$1'.
var_or_term -> blank_node_label : {bnode, unwrap('$1')}.
var_or_term -> anon : anon.
var_or_term -> nil : nil.

var -> var1 : {var, unwrap('$1')}.
var -> var2 : {var, unwrap('$1')}.

iri_term -> iri_ref : {iri, unwrap('$1')}.
iri_term -> prefixed_name : '$1'.

prefixed_name -> pname_ns : {pname, unwrap('$1')}.
prefixed_name -> pname_ln : {pname, unwrap('$1')}.

%% ---------- Expressions ----------

expression -> conditional_or_expression : '$1'.

conditional_or_expression -> conditional_and_expression : '$1'.
conditional_or_expression -> conditional_or_expression oror conditional_and_expression :
    {'or', '$1', '$3'}.

conditional_and_expression -> relational_expression : '$1'.
conditional_and_expression -> conditional_and_expression andand relational_expression :
    {'and', '$1', '$3'}.

relational_expression -> additive_expression : '$1'.
relational_expression -> additive_expression eq additive_expression : {'=', '$1', '$3'}.
relational_expression -> additive_expression ne additive_expression : {'!=', '$1', '$3'}.
relational_expression -> additive_expression lt additive_expression : {'<', '$1', '$3'}.
relational_expression -> additive_expression gt additive_expression : {'>', '$1', '$3'}.
relational_expression -> additive_expression le additive_expression : {'=<', '$1', '$3'}.
relational_expression -> additive_expression ge additive_expression : {'>=', '$1', '$3'}.

additive_expression -> multiplicative_expression : '$1'.
additive_expression -> additive_expression plus multiplicative_expression :
    {'+', '$1', '$3'}.
additive_expression -> additive_expression minus multiplicative_expression :
    {'-', '$1', '$3'}.

multiplicative_expression -> unary_expression : '$1'.
multiplicative_expression -> multiplicative_expression star unary_expression :
    {'*', '$1', '$3'}.
multiplicative_expression -> multiplicative_expression slash unary_expression :
    {'/', '$1', '$3'}.

unary_expression -> primary_expression : '$1'.
unary_expression -> bang primary_expression : {'not', '$2'}.
unary_expression -> plus primary_expression : {'u+', '$2'}.
unary_expression -> minus primary_expression : {'u-', '$2'}.

primary_expression -> bracketted_expression : '$1'.
primary_expression -> built_in_call : '$1'.
primary_expression -> iri_term : '$1'.
primary_expression -> var : '$1'.
primary_expression -> rdf_literal : '$1'.
primary_expression -> numeric_literal : '$1'.
primary_expression -> boolean_literal : '$1'.

bracketted_expression -> lparen expression rparen : '$2'.

built_in_call -> iri_term lparen arg_list rparen :
    {call, '$1', '$3'}.
built_in_call -> iri_term nil :
    {call, '$1', []}.
built_in_call -> built_in_function lparen arg_list rparen :
    {call, '$1', '$3'}.
built_in_call -> aggregate : '$1'.
built_in_call -> built_in_function nil :
    {call, '$1', []}.

built_in_function -> str : str.
built_in_function -> lang : lang.
built_in_function -> langmatches : langmatches.
built_in_function -> datatype : datatype.
built_in_function -> bound : bound.
built_in_function -> iri : iri.
built_in_function -> uri : uri.
built_in_function -> bnode : bnode.
built_in_function -> rand : rand.
built_in_function -> abs : abs.
built_in_function -> ceil : ceil.
built_in_function -> floor : floor.
built_in_function -> round : round.
built_in_function -> concat : concat.
built_in_function -> substr : substr.
built_in_function -> strlen : strlen.
built_in_function -> replace : replace.
built_in_function -> ucase : ucase.
built_in_function -> lcase : lcase.
built_in_function -> encode_for_uri : encode_for_uri.
built_in_function -> contains : contains.
built_in_function -> strstarts : strstarts.
built_in_function -> strends : strends.
built_in_function -> strbefore : strbefore.
built_in_function -> strafter : strafter.
built_in_function -> year : year.
built_in_function -> month : month.
built_in_function -> day : day.
built_in_function -> hours : hours.
built_in_function -> minutes : minutes.
built_in_function -> seconds : seconds.
built_in_function -> timezone : timezone.
built_in_function -> tz : tz.
built_in_function -> now : now.
built_in_function -> uuid : uuid.
built_in_function -> struuid : struuid.
built_in_function -> md5 : md5.
built_in_function -> sha1 : sha1.
built_in_function -> sha256 : sha256.
built_in_function -> sha384 : sha384.
built_in_function -> sha512 : sha512.
built_in_function -> coalesce : coalesce.
built_in_function -> 'if' : 'if'.
built_in_function -> strlang : strlang.
built_in_function -> strdt : strdt.
built_in_function -> sameterm : sameterm.
built_in_function -> isiri : isiri.
built_in_function -> isuri : isuri.
built_in_function -> isblank : isblank.
built_in_function -> isliteral : isliteral.
built_in_function -> isnumeric : isnumeric.
built_in_function -> regex : regex.

aggregate -> count lparen aggregate_distinct star rparen :
    {aggregate, count, '$3', all, undefined}.
aggregate -> count lparen aggregate_distinct expression rparen :
    {aggregate, count, '$3', '$4', undefined}.
aggregate -> sum lparen aggregate_distinct expression rparen :
    {aggregate, sum, '$3', '$4', undefined}.
aggregate -> min lparen aggregate_distinct expression rparen :
    {aggregate, min, '$3', '$4', undefined}.
aggregate -> max lparen aggregate_distinct expression rparen :
    {aggregate, max, '$3', '$4', undefined}.
aggregate -> avg lparen aggregate_distinct expression rparen :
    {aggregate, avg, '$3', '$4', undefined}.
aggregate -> sample lparen aggregate_distinct expression rparen :
    {aggregate, sample, '$3', '$4', undefined}.
aggregate -> group_concat lparen aggregate_distinct expression aggregate_separator rparen :
    {aggregate, group_concat, '$3', '$4', '$5'}.

aggregate_distinct -> '$empty' : default.
aggregate_distinct -> distinct : distinct.

aggregate_separator -> '$empty' : undefined.
aggregate_separator -> semicolon separator eq string : '$4'.

arg_list -> '$empty' : [].
arg_list -> expression : ['$1'].
arg_list -> arg_list comma expression : '$1' ++ ['$3'].

%% ---------- Literals ----------

rdf_literal -> string :
    {literal, '$1'}.
rdf_literal -> string langtag :
    {literal_lang, '$1', unwrap('$2')}.
rdf_literal -> string hat2 iri_term :
    {literal_dt, '$1', '$3'}.

numeric_literal -> integer : {integer, unwrap('$1')}.
numeric_literal -> decimal : {decimal, unwrap('$1')}.
numeric_literal -> double : {double, unwrap('$1')}.
numeric_literal -> integer_positive : {integer, unwrap('$1')}.
numeric_literal -> decimal_positive : {decimal, unwrap('$1')}.
numeric_literal -> double_positive : {double, unwrap('$1')}.
numeric_literal -> integer_negative : {integer, unwrap('$1')}.
numeric_literal -> decimal_negative : {decimal, unwrap('$1')}.
numeric_literal -> double_negative : {double, unwrap('$1')}.

boolean_literal -> true : true.
boolean_literal -> false : false.

string -> string_literal1 : unwrap('$1').
string -> string_literal2 : unwrap('$1').
string -> string_literal_long1 : unwrap('$1').
string -> string_literal_long2 : unwrap('$1').

%% ---------- Erlang code helpers ----------

Erlang code.

unwrap({_Token, _Line, Value}) -> Value;
unwrap({_Token, Value}) -> Value;
unwrap(Value) -> Value.

append_union({union, Xs}, G) -> {union, Xs ++ [G]};
append_union(G1, G2) -> {union, [G1, G2]}.

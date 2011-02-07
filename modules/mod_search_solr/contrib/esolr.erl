%% esolr -  Erlang client library for the apache Solr search server
%%---------------------------------------------------------------------------
%% Copyright (c) 2002 Pablo Polvorin <ppolv@yahoo.com.ar>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
% @author Pablo Polvorin <ppolv@yahoo.com.ar>
% @version alpha, really
% @doc <b>esolr</b> , a http/xml client library to the apache Solr search server.
%%
%% modified by Arjan Scherpenisse <arjan@scherpenisse.net> for use in Zotonic's module system.
%%
-module(esolr).

%%API
-export([start/0,start_link/0,start/1,start_link/1,
         add/2,search/3,delete/2,commit/1,optimize/1,stop/1,set_auto_optimize/2,set_auto_commit/2]).

%%gen_server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).


-define(ESOLR_DEFAULT_SELECTURL,"http://localhost:8983/solr/select").
-define(ESOLR_DEFAULT_UPDATEURL,"http://localhost:8983/solr/update").
-define(ESOLR_STATUS_OK,0).

-define(ESOLR_DEFAULT_TIMEOUT,10*1000).  %10sec
-define(ESOLR_MAX_ADD_TIMEOUT,60*1000).   %1 min
-define(ESOLR_MAX_DELETE_TIMEOUT,60*1000).
-define(ESOLR_MAX_SEARCH_TIMEOUT,60*1000).
-define(ESOLR_MAX_COMMIT_TIMEOUT,120*1000). %2min
-define(ESOLR_MAX_OPTIMIZE_TIMEOUT,120*1000).

-include_lib("xmerl/include/xmerl.hrl").

-record(esolr,
	{update_url,   
	 search_url,
	 add_timeout,	
	 delete_timeout,
	 commit_timeout,
	 search_timeout,
	 optimize_timeout,
	 pending,		%% gb_tree mapping with pending request, keyed by RequestId (as return by async http:request)
	 auto_commit = false,   %% false | always | {time,TRef}
	 auto_optimize = false, %% false | {time,TRef}
	 dirty = false		   %%true if there are uncommited updates
	 }).



%%-----------------API---------------------------------------
% @doc same as start([])
start() ->
	gen_server:start(?MODULE,[],[]).

% @doc same as start_link([]) 
start_link() ->
	gen_server:start_link(?MODULE,[],[]).
	
	
% @doc start the esolr process
% 
%      If not specified, the default url used for search is "http://localhost:8983/solr/select", 
%      and for updates http://localhost:8983/solr/update 
% @spec start(Options::[Option]) -> Result
%       Option = {select_url,URL}|{update_url,URL}| {add_timeout,integer()} | {search_timeout,integer()} 
%                |{delete_timeout,integer()} | {commit_timeout,integer()} | {optimize_timeout,integer()}
%       URL = string()
start(Options) ->
	gen_server:start(?MODULE,Options,[]).

% @see start/1
start_link(Options) ->
	gen_server:start_link(?MODULE,Options,[]).
	
	


% @doc add the given documents to the index
%	
% @spec add(Docs::Documents) -> Result
% Documents = [Doc]
% Doc = {doc,Fields}
% Fields = [Field]	
% Field = {Name,Value}
% Name = atom()
% Value = IOString
add(Docs, Pid) ->
	gen_server:call(Pid,{add,Docs},?ESOLR_MAX_ADD_TIMEOUT).
	
% @doc search the index
%
%      AdditionalInfo contains additional information present in the response 
%      not directly parsed by esolr, like highlight info
%
% @spec search(Query::Query,Options::SearchOptions) -> SearchResult 	
% Query = string()
% SearchOptions = [SearchOption]
% SearchOption = {fields,SearchFields} | {start,StartRow} | {count,Count} | {sort,[SortSpecification]} | {highlight,HihhligthFields}
% SearchFields = string()
% HihhligthFields = string()
% StartRow = integer()
% Count = integer()
% SortSpecification = {Name,Sort}
% Sort = asc | desc
% SearchResult = {ok,RespAttrs,Docs,AdditionalInfo} | {error,Reason}
% RespAttrs = [{Name,Value}]
% Docs = [{doc,Fields}]
% Fields = [{Name,Value}]
% Name = atom()
% Value = IOString
% AdditionalInfo = term()  
search(Query,Options,Pid) ->
	gen_server:call(Pid,{search,Query,Options},?ESOLR_MAX_SEARCH_TIMEOUT).
	
	
% @doc  delete one or more documents. 
% @spec delete(Del::Delete) -> Response
%       Delete = {id,Id} | {q,Query}
% 		Id = string()
% 		Query = string()
%		Response = ok | {error,Reason}
delete(Del,Pid) ->	
	gen_server:call(Pid,{delete,Del},?ESOLR_MAX_DELETE_TIMEOUT).


% @doc  send a "commit" command to the server
% @see set_auto_commit/1
commit(Pid) ->
	gen_server:call(Pid,commit,?ESOLR_MAX_COMMIT_TIMEOUT).

% @doc  send a "optimize" command to the server
%       
% @see set_auto_optimize/1
optimize(Pid) ->
	gen_server:call(Pid,optimize,infinity).

% @doc  stop the esolr process
stop(Pid) ->
	gen_server:call(Pid,stop,infinity).

% @doc  sets the autocommit behavior of the library. 
%
%       {time,N} to do an automatic commit every N miliseconds, if there are uncommited updates <br/>
% 		always :   esolr will automatically commit after each update.     <br/>
% 		false :    esolr won't commit automatically. 
% @spec set_auto_commit(AutoCommitMode::Mode) ->ok
%       Mode = false | always | {time,integer()}
set_auto_commit(AutoCommitMode, Pid)  ->
	if 
		AutoCommitMode == false;
		AutoCommitMode == always; 
		is_tuple(AutoCommitMode) ->
			gen_server:call(Pid,{set_auto_commit,AutoCommitMode});
		true -> throw(bad_option)
	end.
	
	
	
% @doc  sets the auto optimize behavior of the library.
%
%		Similar to set_auto_commit/1, esolr can periodically send "optimize" commands to the server.
%
%       {time,N}: to do an automatic optimizet every N miliseconds <br/>
% 		false :    esolr won't send optimize commands automatically. <br/>
% @spec set_auto_optimize(AutoOptimizeMode::Mode) ->ok
%       Mode = false | {time,integer()}
set_auto_optimize(AutoOptimizetMode, Pid)->
	if 
		AutoOptimizetMode == false;
	    is_tuple(AutoOptimizetMode) ->
	    	gen_server:call(Pid,{set_auto_optimize,AutoOptimizetMode});
	    true -> throw(bad_option)
	end.
	    	




%%----------------------gen_server callbacks ------------------------

% @hidden
timeout_value(Key,Max,Default,Options) ->
	case lists:keysearch(Key,1,Options) of
		{value,{Key,N}} when N =< Max -> 
			N;
		{value,{Key,N}} when N >  Max -> 
			throw({invalid_timeout,{Key,too_big,N}});
		false -> 
			Default
	end.

% @hidden
init(Options) ->
	SelectUrl = case lists:keysearch(select_url,1,Options) of
					{value,{select_url,S}} -> S;
					false -> ?ESOLR_DEFAULT_SELECTURL
				end,
	UpdateUrl = case lists:keysearch(update_url,1,Options) of
					{value,{update_url,U}} -> U;
					false -> ?ESOLR_DEFAULT_UPDATEURL
				end,
	AddTimeout = timeout_value(add_timeout,?ESOLR_MAX_ADD_TIMEOUT,?ESOLR_DEFAULT_TIMEOUT,Options),
	CommitTimeout = timeout_value(commit_timeout,?ESOLR_MAX_COMMIT_TIMEOUT,?ESOLR_DEFAULT_TIMEOUT,Options),
	OptimizeTimeout = timeout_value(optimize_timeout,?ESOLR_MAX_OPTIMIZE_TIMEOUT,?ESOLR_DEFAULT_TIMEOUT,Options),
	SearchTimeout = timeout_value(search_timeout,?ESOLR_MAX_SEARCH_TIMEOUT,?ESOLR_DEFAULT_TIMEOUT,Options),
				
	inets:start(),
	{ok,#esolr{update_url=UpdateUrl,search_url = SelectUrl, 
		add_timeout=AddTimeout,
		commit_timeout=CommitTimeout,
		optimize_timeout=OptimizeTimeout,
		search_timeout=SearchTimeout,
		pending=gb_trees:empty()}}.


% @hidden
%%Cancel previous timer if exists
handle_call(R={set_auto_commit,_Mode},From,State=#esolr{auto_commit={time,TRef}}) ->
	timer:cancel(TRef),
	handle_call(R,From,State#esolr{auto_commit=false});
%%create new timer if neccesary
handle_call({set_auto_commit,{time,N}},_From,State) ->
	{ok,TRef} = timer:send_interval(N,auto_commit),
	{reply,ok,State#esolr{auto_commit={time,TRef}}};
%%no need to setup timer	
handle_call({set_auto_commit,Mode},_From,State) ->
	{reply,ok,State#esolr{auto_commit=Mode}};
	
handle_call(R={set_auto_optimize,_Mode},From,State=#esolr{auto_optimize={time,TRef}}) ->
	timer:cancel(TRef),
	handle_call(R,From,State#esolr{auto_optimize=false});
%%create new timer if neccesary
handle_call({set_auto_optimize,{time,N}},_From,State) ->
	{ok,TRef} = timer:send_interval(N,auto_optimize),
	{reply,ok,State#esolr{auto_optimize={time,TRef}}};
%%no need to setup timer	
handle_call({set_auto_optimize,Mode},_From,State) ->
	{reply,ok,State#esolr{auto_optimize=Mode}};
		


handle_call({add,Docs},From,State=#esolr{add_timeout=T}) ->
	Request = encode_add(Docs),
	make_post_request(Request,{From,add},State#esolr{dirty=true},T);
	
handle_call(commit,From,State=#esolr{commit_timeout=T}) ->	
	Request = encode_commit(),
	make_post_request(Request,{From,commit},State#esolr{dirty=false},T);
	

handle_call({delete,Del},From,State=#esolr{delete_timeout=T}) ->	
	Request = encode_delete(Del),
	make_post_request(Request,{From,delete},State#esolr{dirty=true},T);

handle_call(optimize,From,State=#esolr{optimize_timeout=T}) ->	
	Request = encode_optimize(),
	make_post_request(Request,{From,optimize},State,T);
	
handle_call({search,Query,Options},From,State=#esolr{search_url=URL,pending=P,search_timeout=Timeout}) ->	
	RequestParams = encode_search(Query,Options),
	SearchURL = lists:flatten([URL,"?wt=json&"|RequestParams]),
	{ok,RequestId} = http:request(get,{SearchURL,[{"connection", "close"}]},[{timeout,Timeout}],[{sync,false}]),
	Pendings = gb_trees:insert(RequestId,{From,search},P),
	{noreply,State#esolr{pending=Pendings}};

handle_call(stop,_From,State) ->	
	{stop,normal,ok,State}.
	

make_post_request(Request,PendingInfo,State=#esolr{update_url=URL,pending=P,auto_commit=AC,dirty=Dirty},Timeout) ->
	{ok,RequestId} = http:request(post,{URL,[{"connection", "close"}],"text/xml",Request},[{timeout,Timeout}],[{sync,false}]),
	Pendings = gb_trees:insert(RequestId,PendingInfo,P),
	if 
		(AC == always) and Dirty ->  
				  CommitRequest = encode_commit(),
				  {ok,C_RequestId} = http:request(post,{URL,[{"connection", "close"}],"text/xml",CommitRequest},
				  					     [{timeout,State#esolr.commit_timeout}],[{sync,false}]),
				  Pendings2 = gb_trees:insert(C_RequestId,{auto,auto_commit},Pendings),
				  error_logger:info_report([{auto_commit,send}]),
			  	  {noreply,State#esolr{pending=Pendings2,dirty=false}};
		
		true -> {noreply,State#esolr{pending=Pendings}}
	end.



% @hidden
handle_cast(_Request,State) ->
	{norepyl,State}.


 
 
% @hidden
handle_info({http,{RequestId,HttpResponse}},State = #esolr{pending=P}) ->
	case gb_trees:lookup(RequestId,P) of
		{value,{Client,RequestOp}} -> handle_http_response(HttpResponse,RequestOp,Client),
						 {noreply,State#esolr{pending=gb_trees:delete(RequestId,P)}};
		none -> {noreply,State}
				%% the requestid isn't here, probably the request was deleted after a timeout
	end;


handle_info(auto_commit,State = #esolr{dirty=true,commit_timeout=T}) ->
	Request = encode_commit(),
	R = make_post_request(Request,{auto,auto_commit},State#esolr{dirty=false},T),
	error_logger:info_report([{auto_commit,send}]),
    z_utils:flush_message(auto_commit), 
	R;
	
	
	
handle_info(auto_commit,State = #esolr{dirty=false}) ->
    z_utils:flush_message(auto_commit),	
	{noreply,State};
	
handle_info(auto_optimize,State) ->
	Request = encode_optimize(),
	R = make_post_request(Request,{auto,auto_optimize},State,State#esolr.optimize_timeout),
	error_logger:info_report([{auto_optimize,send}]),
    z_utils:flush_message(auto_optimize),
	R.

 
% @hidden
terminate(_Reason,_State) ->
	ok.
% @hidden	
code_change(_OldVsn,State,_Extra)	->
	{ok,State}.
	
	
	
	
%%----------------------internal functions ------------------------	

handle_http_response({error,HttpError},RequestOp,Client) ->
	response_error(RequestOp,Client,HttpError);
	
%%search response are in json format
handle_http_response({{_HttpV,200,_Reason},_Headers,Data},search,Client) ->
	{ok,{obj,Response},[]} = rfc4627:decode(Data),
	{value,{"responseHeader",{obj,Headers}},RestResponse} = lists:keytake("responseHeader",1,Response),
	{value,{"status",Status}} = lists:keysearch("status",1,Headers),
	case Status of
		0 -> parse_search_response(RestResponse,Client); 
		N -> response_error(search,Client,N)
	end;
 	
handle_http_response({{_HttpV,200,_Reason},_Headers,Data},Op,Client) ->
	{Response,[]} = xmerl_scan:string(binary_to_list(Data)),
	[Header] = xmerl_xpath:string("/response/lst[@name='responseHeader']",Response),
	case parse_xml_response_header(Header) of
		{ok,QTime} ->  parse_xml_response(Op,Response,QTime,Client);
		{error,Error} ->  response_error(Op,Client,Error)
	end;
	
 	
	
handle_http_response({{_HttpV,StatusCode,Reason},_Headers,_Data},_Op,Client) ->	
	error_logger:error_report({"unrecognized response status",StatusCode,Reason}),
	gen_server:reply(Client,{error,{status_code,StatusCode,Reason}}).	
	
	
response_error(auto_commit,auto,Error) ->
	error_logger:error_report([{auto_commit_error,Error}]);
	
 
response_error(auto_optimize,auto,Error) ->
	error_logger:error_report([{auto_optimize_error,Error}]);
	

response_error(_Op,Client,Error) ->
 	gen_server:reply(Client,{error,Error}).
	
	
	
parse_search_response(Response,Client) ->
	{value,{"response",{obj,SearchRespFields}},RestResponse} = lists:keytake("response",1, Response),
	{value,{"docs",Docs},RespFields} =  lists:keytake("docs",1,SearchRespFields),
	gen_server:reply(Client,{ok,RespFields,[{doc,DocFields} || {obj,DocFields}<-Docs],RestResponse}).
	
	
	
parse_xml_response(Op,_Response,_QTime,Client) when Op == add ;
											   Op == commit;
											   Op == optimize;
											   Op == delete	->
	gen_server:reply(Client,ok);
	
	
parse_xml_response(auto_commit,_Response,QTime,auto) ->
	error_logger:info_report([{auto_commit,QTime}]);
	
parse_xml_response(auto_optimize,_Response,QTime,auto) ->
	error_logger:info_report([{auto_optimize,QTime}]).
	
parse_xml_response_header(Header) ->
	[#xmlText{value=V}] = xmerl_xpath:string("/lst/int[@name='status']/text()",Header),
	case list_to_integer(V) of
		?ESOLR_STATUS_OK ->  [#xmlText{value=V1}] = xmerl_xpath:string("/lst/int[@name='QTime']/text()",Header),
					         {ok,list_to_integer(V1)};
		Other -> {error,Other}
		
	end.
		
		
	
	
encode_search(Query,Options) ->
	S = [["q=",url_encode(Query)] | lists:map(fun encode_search_option/1,Options)],
	string:join(S,"&").
	
	
encode_search_option({fields,Fields}) ->
	["fl=",url_encode(Fields)];
	
encode_search_option({start,Start}) ->
	["start=",integer_to_list(Start)];

encode_search_option({rows,Count}) ->	
	["rows=",integer_to_list(Count)];
	
encode_search_option({sort,SortFields}) ->		
	S = [ [Name, "+", atom_to_list(Order)] || {Name,Order} <- SortFields],
	["sort=",string:join(S,",")];


encode_search_option({highlight,Highlight}) ->
	["hl=on&hl.fl=",url_encode(Highlight)];

encode_search_option({facet,Field}) ->
	["facet=on&facet.mincount=1&facet.field=",url_encode(Field)];

encode_search_option({morelikethis,Fields,Count}) ->
	["mlt=on&mlt.count=",integer_to_list(Count),"&mlt.fl=",Fields,
     "&mlt.mindf=1&mlt.mintf=1"].


encode_delete({id,Id})->
	iolist_to_binary(xmerl:export_simple([{delete,[],[{id,[],[Id]}]}],xmerl_xml));

encode_delete({q,Query})->
	iolist_to_binary(xmerl:export_simple([{delete,[],[{'query',[],[Query]}]}],xmerl_xml)).
	
encode_commit() ->
	iolist_to_binary(xmerl:export_simple([{commit,[]}],xmerl_xml)).
	
encode_optimize() ->
	iolist_to_binary(xmerl:export_simple([{optimize,[]}],xmerl_xml)).
	
	
encode_add(Docs) ->
	Doc = {add,[],lists:map(fun encode_doc/1,Docs)},
	iolist_to_binary(xmerl:export_simple([Doc],xmerl_xml)).
	
encode_doc({doc,Fields}) ->
	{doc,[],lists:map(fun encode_field/1,Fields)};

encode_doc({doc,Boost,Fields}) ->
	{doc,[{boost,Boost}],lists:map(fun encode_field/1,Fields)}.
	
	
encode_field({Name,Value}) when is_binary(Value)->
	{field,[{name,Name}],[[Value]]};

encode_field({Name,Value}) ->
	{field,[{name,Name}],[Value]};
	
encode_field({Name,Value,Boost}) when is_binary(Value)->
	{field,[{name,Name},{boost,Boost}],[[Value]]};

encode_field({Name,Value,Boost}) ->
	{field,[{name,Name},{boost,Boost}],[Value]}.



  	
%%%
% URL encode - borrowed from CouchDB
% borrowed again from http://weblog.plexobject.com/?p=1594
%%%
url_encode([H|T]) ->
    if
         H >= $a, $z >= H ->
             [H|url_encode(T)];
         H >= $A, $Z >= H ->
             [H|url_encode(T)];
         H >= $0, $9 >= H ->
             [H|url_encode(T)];
         H == $_; H == $.; H == $-; H == $: ->
             [H|url_encode(T)];
         true ->
             case lists:flatten(io_lib:format("~.16.0B", [H])) of
                 [X, Y] ->
                     [$%, X, Y | url_encode(T)];
                 [X] ->
                     [$%, $0, X | url_encode(T)]
             end
     end;
url_encode([]) ->
     [].
 

%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Bryan Fink <bryan@basho.com>
%% @copyright 2007-2009 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Decision core for webmachine

-module(webmachine_decision_core).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-author('Bryan Fink <bryan@basho.com>').
-export([handle_request/2]).
-export([do_log/1]).
-include("webmachine_logger.hrl").
-include("wm_reqdata.hrl").

handle_request(Resource, ReqData) ->
    try
        d(v3b13, Resource, ReqData)
    catch
        error:X ->
            ?WM_DBG(X),            
            error_response(erlang:get_stacktrace(), Resource, ReqData)
    end.

%% @doc Call the resource or a default.
%% @spec resource_call(atom(), Resource, ReqData) -> {term(), NewResource, NewReqData}
resource_call(Fun, Rs, Rd) ->
	case cacheable(Fun) of
		true ->
			case proplists:lookup(Fun, Rd#wm_reqdata.cache) of
				none -> 
					{T, Rs1, Rd1} = Rs:do(Fun, Rd),
					{T, Rs1, Rd1#wm_reqdata{cache=[{Fun,T}|Rd1#wm_reqdata.cache]}};
				{Fun, Cached} -> 
					{Cached, Rs, Rd}
			end;
		false ->
    		Rs:do(Fun, Rd)
	end.

cacheable(charsets_provided) -> true;
cacheable(content_types_provided) -> true;
cacheable(encodings_provided) -> true;
cacheable(last_modified) -> true;
cacheable(generate_etag) -> true;
cacheable(_) -> false.


get_header_val(H, Rd) -> 
    wrq:get_req_header_lc(H, Rd).

method(Rd) ->
    wrq:method(Rd).

d(DecisionID, Rs, Rd) ->
    Rs:log_d(DecisionID),
    decision(DecisionID, Rs, Rd).

respond(Code, Rs, Rd) ->
    {RsCode, RdCode} = case Code of
        Code when Code == 403; Code == 404 ->
            {ok, ErrorHandler} = application:get_env(webmachine, error_handler),
            Reason = {none, none, []},
            {ErrorHTML, RdError} = ErrorHandler:render_error(Code, Rd, Reason),
            {Rs, wrq:set_resp_body(ErrorHTML, RdError)};
        304 ->
            RdNoCT = wrq:remove_resp_header("Content-Type", Rd),
            {Etag, RsEt, RdEt0} = resource_call(generate_etag, Rs, RdNoCT),
            RdEt = case Etag of
                undefined -> RdEt0;
                ETag -> wrq:set_resp_header("ETag", ETag, RdEt0)
            end,
            {Expires, RsExp, RdExp0} = resource_call(expires, RsEt, RdEt),
            RdExp = case Expires of
                undefined -> RdExp0;
                Exp -> wrq:set_resp_header("Expires", httpd_util:rfc1123_date(calendar:universal_time_to_local_time(Exp)), RdExp0)
            end,
            {RsExp, RdExp};
        _ -> 
            {Rs, Rd}
    end,
    RdRespCode = wrq:set_response_code(Code, RdCode),
    resource_call(finish_request, RsCode, RdRespCode).

respond(Code, Headers, Rs, Rd) ->
    RdHs = wrq:set_resp_headers(Headers, Rd),
    respond(Code, Rs, RdHs).

error_response(Code, Reason, Rs, Rd) ->
    {ok, ErrorHandler} = application:get_env(webmachine, error_handler),
    {ErrorHTML, Rd1} = ErrorHandler:render_error(Code, Rd, Reason),
    Rd2 = wrq:set_resp_body(ErrorHTML, Rd1),
    respond(Code, Rs, Rd2).
error_response(Reason, Rs, Rd) ->
    error_response(500, Reason, Rs, Rd).

decision_test({Test, Rs, Rd}, TestVal, TrueFlow, FalseFlow) ->
    decision_test(Test, TestVal, TrueFlow, FalseFlow, Rs, Rd).

decision_test(Test,TestVal,TrueFlow,FalseFlow, Rs, Rd) ->
    case Test of
	{error, Reason} -> error_response(Reason, Rs, Rd);
	{error, Reason0, Reason1} -> error_response({Reason0, Reason1}, Rs, Rd);
	{halt, Code} -> respond(Code, Rs, Rd);
	TestVal -> decision_flow(TrueFlow, Test, Rs, Rd);
	_ -> decision_flow(FalseFlow, Test, Rs, Rd)
    end.
	    
decision_flow(X, TestResult, Rs, Rd) when is_integer(X) ->
    if X >= 500 -> error_response(X, TestResult, Rs, Rd);
       true -> respond(X, Rs, Rd)
    end;
decision_flow(X, _TestResult, Rs, Rd) when is_atom(X) -> 
    d(X, Rs, Rd);
decision_flow({ErrCode, Reason}, _TestResult, Rs, Rd) when is_integer(ErrCode) ->
    error_response(ErrCode, Reason, Rs, Rd).

do_log(LogData) ->
    case application:get_env(webmachine, webmachine_logger_module) of
        {ok, LoggerModule} -> LoggerModule:log_access(LogData);
        _ -> nop
    end,
    case application:get_env(webmachine, enable_perf_logger) of
	{ok, true} ->
	    webmachine_perf_logger:log(LogData);
	_ ->
	    ignore
    end.


%% "Service Available"
decision(v3b13, Rs, Rd) ->
    decision_test(resource_call(ping, Rs, Rd), pong, v3b13b, 503);
decision(v3b13b, Rs, Rd) ->	
    decision_test(resource_call(service_available, Rs, Rd), true, v3b12, 503);
%% "Known method?"
decision(v3b12, Rs, Rd) ->
    {Methods, Rs1, Rd1} = resource_call(known_methods, Rs, Rd),
    decision_test(lists:member(method(Rd1), Methods), true, v3b11, 501, Rs1, Rd1);
%% "URI too long?"
decision(v3b11, Rs, Rd) ->
    decision_test(resource_call(uri_too_long, Rs, Rd), true, 414, v3b10);
%% "Method allowed?"
decision(v3b10, Rs, Rd) ->
    {Methods, Rs1, Rd1} = resource_call(allowed_methods, Rs, Rd),
    case lists:member(method(Rd1), Methods) of
	true ->
	    d(v3b9, Rs1, Rd1);
	false ->
	    RdAllow = wrq:set_resp_header("Allow", string:join([atom_to_list(M) || M <- Methods], ", "), Rd1),
	    respond(405, Rs1, RdAllow)
    end;
%% "Malformed?"
decision(v3b9, Rs, Rd) ->
    decision_test(resource_call(malformed_request, Rs, Rd), true, 400, v3b8);
%% "Authorized?"
decision(v3b8, Rs, Rd) ->
    {IsAuthorized, Rs1, Rd1} = resource_call(is_authorized, Rs, Rd),
    case IsAuthorized of
	true -> 
	    d(v3b7, Rs1, Rd1);
	{error, Reason} ->
	    error_response(Reason, Rs1, Rd1);
	{halt, Code}  ->
	    respond(Code, Rs1, Rd1);
    AuthHead ->
        RdAuth = wrq:set_resp_header("WWW-Authenticate", AuthHead, Rd1),
        respond(401, Rs1, RdAuth)
    end;
%% "Forbidden?"
decision(v3b7, Rs, Rd) ->
    decision_test(resource_call(forbidden, Rs, Rd), true, 403, v3b6_upgrade);
%% "Upgrade?"
decision(v3b6_upgrade, Rs, Rd) ->
    case get_header_val("upgrade", Rd) of
		undefined ->
			decision(v3b6, Rs, Rd);
		UpgradeHdr ->
		    case get_header_val("connection", Rd) of
				undefined ->
					decision(v3b6, Rs, Rd);
				Connection ->
					case string:strip(string:to_lower(Connection)) of
						"upgrade" ->
							{Choosen, Rs1, Rd1} = choose_upgrade(UpgradeHdr, Rs, Rd),
							case Choosen of
								none ->
									decision(v3b6, Rs1, Rd1);
								{_Protocol, UpgradeFunc} ->
									%% TODO: log the upgrade action
									{upgrade, UpgradeFunc, Rs1, Rd1}
							end;
						_ ->
							decision(v3b6, Rs, Rd)
					end
			end
	end;
%% "Okay Content-* Headers?"
decision(v3b6, Rs, Rd) ->
    decision_test(resource_call(valid_content_headers, Rs, Rd), true, v3b5, 501);
%% "Known Content-Type?"
decision(v3b5, Rs, Rd) ->
    decision_test(resource_call(known_content_type, Rs, Rd), true, v3b4, 415);
%% "Req Entity Too Large?"
decision(v3b4, Rs, Rd) ->
    decision_test(resource_call(valid_entity_length, Rs, Rd), true, v3b3, 413);
%% "OPTIONS?"
decision(v3b3, Rs, Rd) ->
    case wrq:method(Rd) of 
	'OPTIONS' ->
	    {Hdrs, Rs1, Rd1} = resource_call(options, Rs, Rd),
	    respond(200, Hdrs, Rs1, Rd1);
	_ ->
	    d(v3c3, Rs, Rd)
    end;
%% Accept exists?
decision(v3c3, Rs, Rd) ->
    {ContentTypes, Rs1, Rd1} = resource_call(content_types_provided, Rs, Rd),
    PTypes = [Type || {Type,_Fun} <- ContentTypes],
    case get_header_val("accept", Rd1) of
	undefined ->
	    {ok, RdCT} = webmachine_request:set_metadata('content-type', hd(PTypes), Rd1),
	    d(v3d4, Rs1, RdCT);
	_ ->
	    d(v3c4, Rs1, Rd1)
    end;
%% Acceptable media type available?
decision(v3c4, Rs, Rd) ->
    {ContentTypesProvided, Rs1, Rd1} = resource_call(content_types_provided, Rs, Rd),
    PTypes = [Type || {Type,_Fun} <- ContentTypesProvided],
    AcceptHdr = get_header_val("accept", Rd1),
    case webmachine_util:choose_media_type(PTypes, AcceptHdr) of
	none ->
	    respond(406, Rs1, Rd1);
	MType ->
	    {ok, RdCT} = webmachine_request:set_metadata('content-type', MType, Rd1),
	    d(v3d4, Rs, RdCT)
    end;
%% Accept-Language exists?
decision(v3d4, Rs, Rd) ->
    decision_test(get_header_val("accept-language", Rd), undefined, v3e5, v3d5, Rs, Rd);
%% Acceptable Language available? %% WMACH-46 (do this as proper conneg)
decision(v3d5, Rs, Rd) ->
    decision_test(resource_call(language_available, Rs, Rd), true, v3e5, 406);
%% Accept-Charset exists?
decision(v3e5, Rs, Rd) ->
    case get_header_val("accept-charset", Rd) of
        undefined -> decision_test(choose_charset("*", Rs, Rd), none, 406, v3f6);
        _ -> d(v3e6, Rs, Rd)
    end;
%% Acceptable Charset available?
decision(v3e6, Rs, Rd) ->
    decision_test(choose_charset(get_header_val("accept-charset", Rd), Rs, Rd), none, 406, v3f6);
%% Accept-Encoding exists?
% (also, set content-type header here, now that charset is chosen)
decision(v3f6, Rs, Rd) ->
    CType = webmachine_request:get_metadata('content-type', Rd),
    CSet = case webmachine_request:get_metadata('chosen-charset', Rd) of
               undefined -> "";
               CS -> "; charset=" ++ CS
           end,
    Rd1 = wrq:set_resp_header("Content-Type", CType ++ CSet, Rd),
    case get_header_val("accept-encoding", Rd1) of
        undefined -> decision_test(choose_encoding("identity;q=1.0,*;q=0.5", Rs, Rd1), none, 406, v3g7);
        _ -> d(v3f7, Rs, Rd1)
    end;
%% Acceptable encoding available?
decision(v3f7, Rs, Rd) ->
    decision_test(choose_encoding(get_header_val("accept-encoding", Rd), Rs, Rd), none, 406, v3g7);
%% "Resource exists?"
decision(v3g7, Rs, Rd) ->
    % this is the first place after all conneg, so set Vary here
    {Variances, Rs1, Rd1} = variances(Rs, Rd),
    RdVar = case Variances of
        [] -> Rd1;
        _ -> wrq:set_resp_header("Vary", string:join(Variances, ", "), Rd1)
    end,
    decision_test(resource_call(resource_exists, Rs1, RdVar), true, v3g8, v3h7);
%% "If-Match exists?"
decision(v3g8, Rs, Rd) ->
    decision_test(get_header_val("if-match", Rd), undefined, v3h10, v3g9, Rs, Rd);
%% "If-Match: * exists"
decision(v3g9, Rs, Rd) ->
    decision_test(get_header_val("if-match", Rd), "*", v3h10, v3g11, Rs, Rd);
%% "ETag in If-Match"
decision(v3g11, Rs, Rd) ->
    ReqETag = webmachine_util:unquote_header(get_header_val("if-match", Rd)),
    decision_test(resource_call(generate_etag, Rs, Rd), ReqETag, v3h10, 412);
%% "If-Match: * exists"
decision(v3h7, Rs, Rd) ->
    decision_test(get_header_val("if-match", Rd), "*", 412, v3i7, Rs, Rd);
%% "If-unmodified-since exists?"
decision(v3h10, Rs, Rd) ->
    decision_test(get_header_val("if-unmodified-since", Rd), undefined, v3i12, v3h11, Rs, Rd);
%% "I-UM-S is valid date?"
decision(v3h11, Rs, Rd) ->
    IUMSDate = get_header_val("if-unmodified-since", Rd),
    decision_test(webmachine_util:convert_request_date(IUMSDate), bad_date, v3i12, v3h12, Rs, Rd);
%% "Last-Modified > I-UM-S?"
decision(v3h12, Rs, Rd) ->
    ReqDate = get_header_val("if-unmodified-since", Rd),
    ReqErlDate = webmachine_util:convert_request_date(ReqDate),
    {ResErlDate, Rs1, Rd1} = resource_call(last_modified, Rs, Rd),
    decision_test(ResErlDate > ReqErlDate, true, 412, v3i12, Rs1, Rd1);
%% "Moved permanently? (apply PUT to different URI)"
decision(v3i4, Rs, Rd) ->
    {MovedPermanently, Rs1, Rd1} = resource_call(moved_permanently, Rs, Rd),
    case MovedPermanently of
	{true, MovedURI} ->
	    RdLoc = wrq:set_resp_header("Location", MovedURI, Rd1),
	    respond(301, Rs1, RdLoc);
	false ->
	    d(v3p3, Rs1, Rd1);
	{error, Reason} ->
	    error_response(Reason, Rs1, Rd1);
	{halt, Code} ->
	    respond(Code, Rs1, Rd1)
    end;
%% PUT?
decision(v3i7, Rs, Rd) ->
    decision_test(method(Rd), 'PUT', v3i4, v3k7, Rs, Rd);
%% "If-none-match exists?"
decision(v3i12, Rs, Rd) ->
    decision_test(get_header_val("if-none-match", Rd), undefined, v3l13, v3i13, Rs, Rd);
%% "If-None-Match: * exists?"
decision(v3i13, Rs, Rd) ->
    decision_test(get_header_val("if-none-match", Rd), "*", v3j18, v3k13, Rs, Rd);
%% GET or HEAD?
decision(v3j18, Rs, Rd) ->
    decision_test(lists:member(method(Rd),['GET','HEAD']), true, 304, 412, Rs, Rd);
%% "Moved permanently?"
decision(v3k5, Rs, Rd) ->
    {MovedPermanently, Rs1, Rd1} = resource_call(moved_permanently, Rs, Rd),
    case MovedPermanently of
	{true, MovedURI} ->
	    RdLoc = wrq:set_resp_header("Location", MovedURI, Rd1),
	    respond(301, Rs1, RdLoc);
	false ->
	    d(v3l5, Rs1, Rd1);
	{error, Reason} ->
	    error_response(Reason, Rs1, Rd1);
	{halt, Code} ->
	    respond(Code, Rs1, Rd1)
    end;
%% "Previously existed?"
decision(v3k7, Rs, Rd) ->
    decision_test(resource_call(previously_existed, Rs, Rd), true, v3k5, v3l7);
%% "Etag in if-none-match?"
decision(v3k13, Rs, Rd) ->
    ReqETag = webmachine_util:unquote_header(get_header_val("if-none-match", Rd)),
    decision_test(resource_call(generate_etag, Rs, Rd), ReqETag, v3j18, v3l13);
%% "Moved temporarily?"
decision(v3l5, Rs, Rd) ->
    {MovedTemporarily, Rs1, Rd1} = resource_call(moved_temporarily, Rs, Rd),
    case MovedTemporarily of
	{true, MovedURI} ->
	    RdLoc = wrq:set_resp_header("Location", MovedURI, Rd1),
	    respond(307, Rs1, RdLoc);
	false ->
	    d(v3m5, Rs1, Rd1);
	{error, Reason} ->
	    error_response(Reason, Rs1, Rd1);
	{halt, Code} ->
	    respond(Code, Rs1, Rd1)
    end;
%% "POST?"
decision(v3l7, Rs, Rd) ->
    decision_test(method(Rd), 'POST', v3m7, 404, Rs, Rd);
%% "IMS exists?"
decision(v3l13, Rs, Rd) ->
    decision_test(get_header_val("if-modified-since", Rd), undefined, v3m16, v3l14, Rs, Rd);
%% "IMS is valid date?"
decision(v3l14, Rs, Rd) -> 
    IMSDate = get_header_val("if-modified-since", Rd),
    decision_test(webmachine_util:convert_request_date(IMSDate), bad_date, v3m16, v3l15, Rs, Rd);
%% "IMS > Now?"
decision(v3l15, Rs, Rd) ->
    NowDateTime = calendar:universal_time(),
    ReqDate = get_header_val("if-modified-since", Rd),
    ReqErlDate = webmachine_util:convert_request_date(ReqDate),
    decision_test(ReqErlDate > NowDateTime, true, v3m16, v3l17, Rs, Rd);
%% "Last-Modified > IMS?"
decision(v3l17, Rs, Rd) ->
    ReqDate = get_header_val("if-modified-since", Rd),    
    ReqErlDate = webmachine_util:convert_request_date(ReqDate),
    {ResErlDate, Rs1, Rd1} = resource_call(last_modified, Rs, Rd),
    decision_test(ResErlDate =:= undefined orelse ResErlDate > ReqErlDate,
                  true, v3m16, 304, Rs1, Rd1);
%% "POST?"
decision(v3m5, Rs, Rd) ->
    decision_test(method(Rd), 'POST', v3n5, 410, Rs, Rd);
%% "Server allows POST to missing resource?"
decision(v3m7, Rs, Rd) ->
    decision_test(resource_call(allow_missing_post, Rs, Rd), true, v3n11, 404);
%% "DELETE?"
decision(v3m16, Rs, Rd) ->
    decision_test(method(Rd), 'DELETE', v3m20, v3n16, Rs, Rd);
%% DELETE enacted immediately?
%% Also where DELETE is forced.
decision(v3m20, Rs, Rd) ->
    decision_test(resource_call(delete_resource, Rs, Rd), true, v3m20b, 500);
decision(v3m20b, Rs, Rd) ->
    decision_test(resource_call(delete_completed, Rs, Rd), true, v3o20, 202);
%% "Server allows POST to missing resource?"
decision(v3n5, Rs, Rd) ->
    decision_test(resource_call(allow_missing_post, Rs, Rd), true, v3n11, 410);
%% "Redirect?"
decision(v3n11, Rs, Rd) ->
    {PostIsCreate, Rs1, Rd1} = resource_call(post_is_create, Rs, Rd),
    {Stage1, RsStage1, RdStage1} = case PostIsCreate of
        true ->
            {CreatePath, Rs2, Rd2} = resource_call(create_path, Rs1, Rd1),
            case CreatePath of
                undefined -> 
                    error_response("post_is_create w/o create_path", Rs2, Rd2);
                NewPath ->
                    case is_list(NewPath) of
                        false -> 
                            error_response("create_path not a string", Rs2, Rd2);
                        true ->
                            RdPath = wrq:set_disp_path(NewPath, Rd2),
                            {Res, Rs3, Rd3} = accept_helper(Rs2, RdPath),
                            case Res of
                                {respond, Code} -> respond(Code, Rs3, Rd3);
                                {halt, Code} -> respond(Code, Rs3, Rd3);
                                {error, _,_} -> error_response(Res, Rs3, Rd3);
                                {error, _} -> error_response(Res, Rs3, Rd3);
                                _ -> {stage1_ok, Rs3, Rd3}
                            end
                    end
            end;
        _ ->
            {ProcessPost, Rs2, Rd2} = resource_call(process_post, Rs1, Rd1),
            case ProcessPost of
                true -> 
                    {_, Rs3, Rd3} = encode_body_if_set(Rs2, Rd2),
                    {stage1_ok, Rs3, Rd3};
                {halt, Code} -> respond(Code, Rs2, Rd2);
                Err -> error_response(Err, Rs2, Rd2)
            end
    end,
    case Stage1 of
        stage1_ok ->
            case wrq:resp_redirect(RdStage1) of
                true ->
                    case wrq:get_resp_header("Location", RdStage1) of
                        undefined ->
                            respond(500, "Response had do_redirect but no Location", RsStage1, RdStage1);
                        _ ->
                            respond(303, RsStage1, RdStage1)
                    end;
                _ ->
                    d(v3p11, RsStage1, RdStage1)
            end;
        _ -> 
            {nop, RsStage1, RdStage1}
    end;
%% "POST?"
decision(v3n16, Rs, Rd) ->
    decision_test(method(Rd), 'POST', v3n11, v3o16, Rs, Rd);
%% Conflict?
decision(v3o14, Rs, Rd) ->
    {IsConflict, Rs1, Rd1} = resource_call(is_conflict, Rs, Rd),
    case IsConflict of
        true -> respond(409, Rs1, Rd1);
        _ -> 
            {Res, RsHelp, RdHelp} = accept_helper(Rs1, Rd1),
            case Res of
                {respond, Code} -> respond(Code, RsHelp, RdHelp);
                {halt, Code} -> respond(Code, RsHelp, RdHelp);
                {error, _,_} -> error_response(Res, RsHelp, RdHelp);
                {error, _} -> error_response(Res, RsHelp, RdHelp);
                _ -> d(v3p11, RsHelp, RdHelp)
            end
    end;
%% "PUT?"
decision(v3o16, Rs, Rd) ->
    decision_test(method(Rd), 'PUT', v3o14, v3o18, Rs, Rd);
%% Multiple representations?
% (also where body generation for GET and HEAD is done)
decision(v3o18, Rs, Rd) ->    
    BuildBody = case method(Rd) of
        'GET' -> true;
        'HEAD' -> true;
        _ -> false
    end,
    {FinalBody, RsBody, RdBody} = case BuildBody of
        true ->
            {Etag, RsEtag, RdEtag0} = resource_call(generate_etag, Rs, Rd),
            RdEtag = case Etag of
                undefined -> RdEtag0;
                ETag -> wrq:set_resp_header("ETag", ETag, RdEtag0)
            end,

            {LastModified, RsLM, RdLM0} = resource_call(last_modified, RsEtag, RdEtag),
            RdLM = case LastModified of
                undefined -> RdLM0;
                LM -> wrq:set_resp_header("Last-Modified", httpd_util:rfc1123_date(calendar:universal_time_to_local_time(LM)), RdLM0)
            end,

            {Expires, RsExp, RdExp0} = resource_call(expires, RsLM, RdLM),
            RdExp = case Expires of
                undefined -> RdExp0;
                Exp -> wrq:set_resp_header("Expires", httpd_util:rfc1123_date(calendar:universal_time_to_local_time(Exp)), RdExp0)
            end,

            CT = webmachine_request:get_metadata('content-type', RdExp),
            {ContentTypesProvided, RsCT, RdCT} = resource_call(content_types_provided, RsExp, RdExp),
            F = hd([Fun || {Type,Fun} <- ContentTypesProvided, CT =:= Type]),
            resource_call(F, RsCT, RdCT);
        false -> 
            {nop, Rs, Rd}
    end,
    case FinalBody of
        {error, _} -> error_response(FinalBody, RsBody, RdBody);
        {error, _,_} -> error_response(FinalBody, RsBody, RdBody);
        {halt, Code} -> respond(Code, RsBody, RdBody);
        nop -> d(v3o18b, RsBody, RdBody);
        _ ->
            {EncodedBody, RsEB, RdEB} = encode_body(FinalBody, RsBody, RdBody), 
            d(v3o18b, RsEB, wrq:set_resp_body(EncodedBody, RdEB))
    end;

decision(v3o18b, Rs, Rd) ->
    decision_test(resource_call(multiple_choices, Rs, Rd), true, 300, 200);
%% Response includes an entity?
decision(v3o20, Rs, Rd) ->
    decision_test(webmachine_request:has_resp_body(Rd), true, v3o18, 204, Rs, Rd);
%% Conflict?
decision(v3p3, Rs, Rd) ->
    {IsConflict, Rs1, Rd1} = resource_call(is_conflict, Rs, Rd),
    case IsConflict of
        true -> respond(409, Rs1, Rd1);
        _ -> 
            {Res, RsHelp, RdHelp} = accept_helper(Rs1, Rd1),
            case Res of
                {respond, Code} -> respond(Code, RsHelp, RdHelp);
                {halt, Code} -> respond(Code, RsHelp, RdHelp);
                {error, _,_} -> error_response(Res, RsHelp, RdHelp);
                {error, _} -> error_response(Res, RsHelp, RdHelp);
                _ -> d(v3p11, RsHelp, RdHelp)
            end
    end;

%% New resource?  (at this point boils down to "has location header")
decision(v3p11, Rs, Rd) ->
    case wrq:get_resp_header("Location", Rd) of
        undefined -> d(v3o20, Rs, Rd);
        _ -> respond(201, Rs, Rd)
    end.

accept_helper(Rs, Rd) ->
    CT = case get_header_val("Content-Type", Rd) of
             undefined -> "application/octet-stream";
             Other -> Other
         end,
    {MT, MParams} = webmachine_util:media_type_to_detail(CT),
    {ok, RdMParams} = webmachine_request:set_metadata('mediaparams', MParams, Rd),
    {ContentTypesAccepted, Rs1, Rd1} = resource_call(content_types_accepted, Rs, RdMParams),
    case [Fun || {Type,Fun} <- ContentTypesAccepted, MT =:= Type] of
        [] -> 
            {{respond, 415}, Rs1, Rd1};
        AcceptedContentList ->
            F = hd(AcceptedContentList),
            {Result, Rs2, Rd2} = resource_call(F, Rs1, Rd1),
            case Result of
                true ->
                    {_, RsEncoded, RdEncoded} = encode_body_if_set(Rs2, Rd2),
                    {true, RsEncoded, RdEncoded};
                _ -> 
                    {Result, Rs2, Rd2}
            end
    end.

encode_body_if_set(Rs, Rd) ->
    case webmachine_request:has_resp_body(Rd) of
        true ->
            Body = wrq:resp_body(Rd),
            {Encoded, Rs1, Rd1} = encode_body(Body, Rs, Rd),
            {true, Rs1, wrq:set_resp_body(Encoded, Rd1)};
        _ -> 
            {false, Rs, Rd}
    end.

encode_body(Body, Rs, Rd) ->
    ChosenCSet = webmachine_request:get_metadata('chosen-charset', Rd),
    {CharSetsProvided, Rs1, Rd1} = resource_call(charsets_provided, Rs, Rd),
    Charsetter = 
        case CharSetsProvided of
            no_charset -> fun(X) -> X end;
            CP -> hd([Fun || {CSet,Fun} <- CP, ChosenCSet =:= CSet])
        end,
    ChosenEnc = webmachine_request:get_metadata('content-encoding', Rd1),
    {EncodingsProvided, Rs2, Rd2} = resource_call(encodings_provided, Rs1, Rd1),
    Encoder = hd([Fun || {Enc,Fun} <- EncodingsProvided, ChosenEnc =:= Enc]),
    case Body of
        {stream, StreamBody} ->
            {{stream, make_encoder_stream(Encoder, Charsetter, StreamBody)}, Rs2, Rd2};
        {stream, Size, Fun} ->
            {stream, Size, make_size_encoder_stream(Encoder, Charsetter, Fun)};
        {writer, BodyFun} ->
            {{writer, {Encoder, Charsetter, BodyFun}}, Rs2, Rd2};
        {writer, Size, BodyFun} ->
            {{writer, Size, {Encoder, Charsetter, BodyFun}}, Rs2, Rd2};
        _ ->
            {Encoder(Charsetter(to_binary(Body))), Rs2, Rd2}
    end.
    
    to_binary(Body) when is_tuple(Body) -> Body;
    to_binary(Body) -> iolist_to_binary(Body).

make_size_encoder_stream(Encoder, Charsetter, Fun) ->
    fun(Start, End) ->
        make_encoder_stream(Encoder, Charsetter, Fun(Start, End))
    end.

make_encoder_stream(Encoder, Charsetter, {Body, done}) ->
    {Encoder(Charsetter(Body)), done};
make_encoder_stream(Encoder, Charsetter, {Body, Next}) ->
    {Encoder(Charsetter(Body)), fun() -> make_encoder_stream(Encoder, Charsetter, Next()) end}.

choose_encoding(AccEncHdr, Rs, Rd) ->
    {EncodingsProvided, Rs1, Rd1} = resource_call(encodings_provided, Rs, Rd),
    Encs = [Enc || {Enc,_Fun} <- EncodingsProvided],
    case webmachine_util:choose_encoding(Encs, AccEncHdr) of
	none -> 
	    {none, Rs1, Rd1};
	ChosenEnc ->
        RdEnc = case ChosenEnc of
            "identity" -> Rd1;
            _ -> wrq:set_resp_header("Content-Encoding",ChosenEnc, Rd1)
        end,
        {ok, RdEnc1} = webmachine_request:set_metadata('content-encoding',ChosenEnc,RdEnc),
	    {ChosenEnc, Rs1, RdEnc1}
    end.

choose_charset(AccCharHdr, Rs, Rd) ->
    {CharsetsProvided, Rs1, Rd1} = resource_call(charsets_provided, Rs, Rd),
    case CharsetsProvided of
        no_charset ->
            {no_charset, Rs1, Rd1};
        CL ->
            CSets = [CSet || {CSet,_Fun} <- CL],
            case webmachine_util:choose_charset(CSets, AccCharHdr) of
                none -> 
                    {none, Rs1, Rd1};
                Charset ->
                    {ok, RdCSet} = webmachine_request:set_metadata('chosen-charset', Charset, Rd1),
                    {Charset, Rs1, RdCSet}
            end
    end.

choose_upgrade(UpgradeHdr, Rs, Rd) ->
    {UpgradesProvided, Rs1, Rd1} = resource_call(upgrades_provided, Rs, Rd),
	Provided1 = [ {string:to_lower(Prot), Prot, PFun} || {Prot, PFun} <- UpgradesProvided],
	Requested = [ string:to_lower(string:strip(Up)) || Up <- string:tokens(UpgradeHdr, ",") ],
	{choose_upgrade1(Requested, Provided1), Rs1, Rd1}.

	choose_upgrade1([], _) ->
		none;
	choose_upgrade1([Req|Requested], Provided) ->
		case lists:keysearch(Req, 1, Provided) of
			false ->
				choose_upgrade1(Requested, Provided);
			{value, {_, Protocol, UpgradeFun}} ->
				{Protocol, UpgradeFun}
		end.


variances(Rs, Rd) ->
    {ContentTypesProvided, Rs1, Rd1} = resource_call(content_types_provided, Rs, Rd),
    Accept = case length(ContentTypesProvided) of
        1 -> [];
        0 -> [];
        _ -> ["Accept"]
    end,
    {EncodingsProvided, Rs2, Rd2} = resource_call(encodings_provided, Rs1, Rd1),
    AcceptEncoding = case length(EncodingsProvided) of
	1 -> [];
	0 -> [];
	_ -> ["Accept-Encoding"]
    end,
    {CharsetsProvided, Rs3, Rd3} = resource_call(charsets_provided, Rs2, Rd2),
    AcceptCharset = case CharsetsProvided of
        no_charset -> 
            [];
        CP ->
            case length(CP) of
                1 -> [];
                0 -> [];
                _ -> ["Accept-Charset"]
            end
    end,
    {Variances, Rs4, Rd4} = resource_call(variances, Rs3, Rd3),
    {Accept ++ AcceptEncoding ++ AcceptCharset ++ Variances, Rs4, Rd4}.
    
    

-record(wm_reqdata, {
    
    % Reqstate
    socket=undefined,
    metadata=dict:new(),
    range=undefined,
    peer=undefined,
    bodyfetch=undefined,
    log_data=undefined,

    % Reqdata
    method, version,
    disp_path, path, raw_path, path_info, path_tokens,
    app_root,response_code,max_recv_body,
    req_cookie, req_qs, req_headers, req_body,
    resp_redirect, resp_headers, resp_body,
    host_tokens, port,

	%% Cache of resource calls
	cache=[]
}).

-define(WM_DBG(Msg), error_logger:info_msg("DEBUG: ~p:~p  ~p~n", [?MODULE, ?LINE, Msg])).

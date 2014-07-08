

admin_controller_is_authorized(DefaultMod, ReqData, Context) ->
	Context1 = ?WM_REQ(ReqData, Context),
	Context2 = z_context:ensure_all(Context1),
	z_context:lager_md(Context2),
    z_acl:wm_is_authorized([{use, z_context:get(acl_module, Context, DefaultMod)}], admin_logon, Context2).

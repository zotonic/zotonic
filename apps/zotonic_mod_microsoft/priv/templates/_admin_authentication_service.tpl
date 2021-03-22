{% wire id="admin_microsoft" type="submit" postback="admin_microsoft" delegate=`mod_microsoft` %}
<form name="admin_microsoft" id="admin_microsoft" class="form" method="POST" action="postback">
    <div class="row">
        <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header"><span class="fa fa-windows"></span> Microsoft</h3>
                <div class="widget-content">
                    <p class="help-block">
                        {_ Application keys can be found in _} <a href="https://go.microsoft.com/fwlink/?linkid=2083908" title="Azure Portal" target="_blank" rel="noopener noreferrer">{_ Azure Portal App registrations _}</a>
                    </p>

                    <div class="form-group label-floating">
                        <input type="text" id="microsoft_appid" name="appid" value="{{ m.config.mod_microsoft.appid.value|escape }}" class="form-control"
                        placeholder="{_ Application ID _}">
                        <label class="control-label" for="microsoft_app_id">{_ Application ID _}</label>
                    </div>

                    <div class="form-group label-floating">
                        <input type="text" id="microsoft_appsecret" name="appsecret" value="{{ m.config.mod_microsoft.appsecret.value|escape }}" class="form-control" placeholder="{_ Client Secret _}">
                        <label class="control-label" for="microsoft_appsecret">{_ Client Secret _}</label>
                    </div>

                    <div class="form-group label-floating">
                        <input type="text" id="microsoft_scope" name="scope" value="{{ m.config.mod_microsoft.scope.value|default:'email profile'|escape }}" class="form-control" placeholder="{_ Scope _}">
                        <label class="control-label" for="microsoft_scope">{_ Scope _}</label>
                        <p class="help-block">
                            {_ Space separated list of scopes that you want the user to consent to. Examples are: <tt>email</tt>, <tt>offline_access</tt> and <tt>profile</tt>. The <tt>openid</tt> scope is always added automatically. _}
                            <a href="https://docs.microsoft.com/en-us/azure/active-directory/develop/v2-permissions-and-consent" target="_blank" rel="noopener noreferrer">{_ More information about scopes _}</a>
                        </p>
                    </div>

                    <div class="form-group label-floating">
                        <input type="text" id="microsoft_scope" name="tenant" value="{{ m.config.mod_microsoft.tenant.value|default:'common'|escape }}" class="form-control" placeholder="{_ Tenant _}">
                        <label class="control-label" for="microsoft_tenant">{_ Tenant _}</label>
                        <p class="help-block">
                            {_ Control who can sign in. Allowed values are: <tt>common</tt>, <tt>organizations</tt>, <tt>consumers</tt>, and tenant identifiers. _}
                            <a href="https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-v2-protocols#endpoints" target="_blank" rel="noopener noreferrer">{_ More information about tenant identifiers _}</a>
                        </p>
                    </div>

                    <div class="form-group">
                        <label class="checkbox">
                            <input type="checkbox" id="microsoft_useauth" name="useauth" {% if m.config.mod_microsoft.useauth.value %}checked="checked"{% endif %} value="1" />
                            {_ Use Microsoft authentication _}
                        </label>
                    </div>

                    <div class="form-group">
                        <button class="btn btn-primary" type="submit">{_ Save Microsoft Settings _}</button>
                    </div>

                </div>
            </div>
        </div>
    </div>
</form>

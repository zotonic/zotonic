{% wire id="admin_linkedin" type="submit" postback="admin_linkedin" delegate=`mod_linkedin` %}
<form name="admin_linkedin" id="admin_linkedin" class="form-horizontal" method="POST" action="postback">
    <div class="row-fluid">
        <div class="span9">
            <div class="widget">
                <h3 class="widget-header"><span class="icon-linkedin-sign"></span> LinkedIn</h3>
                <div class="widget-content">

                    <div class="clearfix">
                        <h1 class="icon-linkedin-sign pull-left">&nbsp;</h1>
                        <p>LinkedIn<br/>
                            <small>{_ You can find the application keys in _} <a href="https://www.linkedin.com/secure/developer" title="Developer Network" target="_blank">{_ Your LinkedIn Developer Network _}</small></a>
                        </p>
                    </div>

                    <div class="control-group">
                        <label class="control-label" for="linkedin_consumer_key">{_ API Key _}</label>
                        <div class="controls">
                            <input type="text" id="linkedin_appid" name="appid" value="{{ m.config.mod_linkedin.appid.value|escape }}" class="input-block-level" />
                        </div>
                    </div>
                    
                    <div class="control-group">
                        <label class="control-label" for="appsecret">{_ Secret Key _}</label>
                        <div class="controls">
                            <input type="text" id="linkedin_consumer_secret" name="appsecret" value="{{ m.config.mod_linkedin.appsecret.value|escape }}" class="input-block-level" />
                        </div>
                    </div>

                    <div class="control-group">
                        <label class="control-label" for="linkedin_scope">{_ Scope _}</label>
                        <div class="controls">
                            <input type="text" id="linkedin_scope" name="scope" value="{{ m.config.mod_linkedin.scope.value|default:'r_basicprofile r_emailaddress r_contactinfo'|escape }}" class="input-block-level" />
                        </div>
                    </div>

                    <div class="control-group">
                        <div class="controls">
                            <label class="checkbox" for="linkedin_useauth">
                                <input type="checkbox" id="linkedin_useauth" name="useauth" {% if m.config.mod_linkedin.useauth.value %}checked="checked"{% endif %} value="1" />
                                {_ Use LinkedIn authentication _}
                            </label>
                        </div>
                    </div>

                    <div class="form-actions">
                        <div class="controls">
                            <button class="btn btn-primary" type="submit">{_ Save LinkedIn Settings _}</button>
                        </div>
                    </div>
                    
                </div>
            </div>
        </div>
    </div>
</form>

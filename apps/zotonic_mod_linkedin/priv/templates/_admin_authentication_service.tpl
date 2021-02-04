{% wire id="admin_linkedin" type="submit" postback="admin_linkedin" delegate=`mod_linkedin` %}
<form name="admin_linkedin" id="admin_linkedin" class="form" method="POST" action="postback">
    <div class="row">
        <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header"><span class="fa fa-linkedin"></span> LinkedIn</h3>
                <div class="widget-content">

                    <p class="help-block">
                        {_ You can find the application keys in _} <a href="https://www.linkedin.com/secure/developer" title="Developer Network" target="_blank">{_ Your LinkedIn Developer Network _}</a>
                    </p>

                    <div class="form-group label-floating">
                        <input type="text" id="linkedin_appid" name="appid" value="{{ m.config.mod_linkedin.appid.value|escape }}" class="form-control" placeholder="{_ API Key _}">
                        <label class="control-label" for="linkedin_consumer_key">{_ API Key _}</label>
                    </div>

                    <div class="form-group label-floating">
                        <input type="text" id="linkedin_consumer_secret" name="appsecret" value="{{ m.config.mod_linkedin.appsecret.value|escape }}" class="form-control" placeholder="{_ Secret Key _}">
                        <label class="control-label" for="appsecret">{_ Secret Key _}</label>
                    </div>

                    <div class="form-group">
                        <label class="checkbox">
                            <input type="checkbox" id="linkedin_useauth" name="useauth" {% if m.config.mod_linkedin.useauth.value %}checked="checked"{% endif %} value="1" />
                            {_ Use LinkedIn authentication _}
                        </label>
                    </div>

                    <div class="form-group">
                        <button class="btn btn-primary" type="submit">{_ Save LinkedIn Settings _}</button>
                    </div>

                </div>
            </div>
        </div>
    </div>
</form>

{% wire id="admin_facebook" type="submit" postback="admin_facebook" %}
<form name="admin_facebook" id="admin_facebook" class="form-horizontal" method="POST" action="postback">
    <div class="row">
        <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header"><span class="icon-facebook-sign"></span> Facebook</h3>
                <div class="widget-content">

                    <div class="clearfix">
                        <h1 class="icon-facebook-sign pull-left">&nbsp;</h1>

                        <p>
                            Facebook<br/>
                            <small>{_ You can find the application keys in _}  <a href="https://www.facebook.com/developers/apps.php" title="Developer Dashboard" target="_blank">{_ Your Facebook Developer Dashboard _}</a></small>
                        </p>
                    </div>

                    <div class="form-group row">
                        <label class="control-label col-md-3" for="facebook_app_id">{_ App ID. _}</label>
                        <div class="col-md-9">
                            <input type="text" id="facebook_appid" name="appid" value="{{ m.config.mod_facebook.appid.value|escape }}" class="col-lg-6 col-md-6 form-control" />
                        </div>
                    </div>
                    
                    <div class="form-group row">
                        <label class="control-label col-md-3" for="facebook_appsecret">{_ App Secret _}</label>
                        <div class="col-md-9">
                            <input type="text" id="facebook_appsecret" name="appsecret" value="{{ m.config.mod_facebook.appsecret.value|escape }}" class="col-lg-6 col-md-6 form-control" />
                        </div>
                    </div>
                    
                    <div class="form-group row">
                        <label class="control-label col-md-3" for="facebook_scope">{_ Scope _}</label>
                        <div class="col-md-9">
                            <input type="text" id="facebook_scope" name="scope" value="{{ m.config.mod_facebook.scope.value|default:'email'|escape }}" class="col-lg-6 col-md-6 form-control" />
                        </div>
                    </div>

                    <div class="form-group row">
                        <div class="col-md-9 col-md-offset-3">
                            <div class="checkbox">
                                <label>
                                    <input type="checkbox" id="facebook_useauth" name="useauth" {% if m.config.mod_facebook.useauth.value %}checked="checked"{% endif %} value="1" />
                                    {_ Use Facebook authentication _}
                                </label>
                            </div>
                        </div>
                    </div>

                    <div class="form-group">
                        <div class="col-md-offset-3">
                            <button class="btn btn-primary" type="submit">{_ Save Facebook Settings _}</button>
                        </div>
                    </div>
                    
                </div>
            </div>
        </div>
    </div>
</form>

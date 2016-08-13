{% wire id="admin_instagram" type="submit" postback="admin_instagram" delegate=`mod_instagram` %}
<form name="admin_instagram" id="admin_instagram" class="form-horizontal" method="POST" action="postback">
    <div class="row">
        <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header"><span class="fa fa-instagram"></span> Instagram</h3>
                <div class="widget-content">

                    <p class="help-block">
                        {_ Application keys can be found in _} <a href="https://apps.instagram.com" title="Manage Clients" target="_blank">{_ Your Instagram Manage Clients page _}</a>
                    </p>

                    <fieldset>
                        <div class="form-group row">
                            <label class="control-label col-md-3" for="instagram_consumer_key">{_ Client ID _}</label>
                            <div class="col-md-9">
                                <input type="text" id="instagram_consumer_key" name="consumer_key" value="{{ m.config.mod_instagram.consumer_key.value|escape }}" class="form-control" />
                            </div>
                        </div>

                        <div class="form-group row">
                            <label class="control-label col-md-3" for="instagram_consumer_secret">{_ Client Secret _}</label>
                            <div class="col-md-9">
                                <input type="text" id="instagram_consumer_secret" name="consumer_secret" value="{{ m.config.mod_instagram.consumer_secret.value|escape }}" class="form-control" />
                            </div>
                        </div>

                        <div class="form-group row">
                            <div class="col-md-9 col-md-offset-3">
                                <div class="checkbox">
                                    <label for="instagram_useauth">
                                        <input type="checkbox" id="instagram_useauth" name="useauth" {% if m.config.mod_instagram.useauth.value %}checked="checked"{% endif %} value="1" />
                                        {_ Use Instagram authentication _}
                                    </label>
                                </div>
                            </div>
                        </div>
                    </fieldset>

                    <fieldset>

                        <p>{_ Zotonic can import photos and videos with specific tags. _}</p>

                        <div class="form-group row">
                            <label class="control-label col-md-3" for="instagram_access_token">{_ Access Token _}</label>
                            <div class="col-md-9">
                                {% live template="_instagram_access_token.tpl" topic=m.acl.user %}
                            </div>
                       </div>


                        <div class="form-group row">
                            <label class="control-label col-md-3" for="instagram_follow">{_ Follow tags _}</label>
                            <div class="col-md-9">
                                <textarea id="instagram_follow" name="follow" value="" class="form-control" />{{ m.config.mod_instagram.follow.value|escape }}</textarea>

                                <p class="help-block">
                                    {_ Separate tags with commas or newlines. _}
                                </p>
                            </div>
                        </div>

                        <div class="form-group row">
                            <div class="col-md-9 col-md-offset-3">
                                <div class="checkbox">
                                    <label for="instagram_import_photos">
                                        <input type="checkbox" id="instagram_import_photos" name="import_photos" {% if m.config.mod_instagram.import_photos.value %}checked="checked"{% endif %} value="1" />
                                        {_ Photo import _}
                                    </label>
                                </div>
                                <div class="checkbox">
                                    <label for="instagram_import_videos">
                                        <input type="checkbox" id="instagram_import_videos" name="import_videos" {% if m.config.mod_instagram.import_videos.value %}checked="checked"{% endif %} value="1" />
                                        {_ Video import _}
                                    </label>
                                </div>
                            </div>
                        </div>

                    </fieldset>

                    <div class="form-group row">
                        <div class="col-md-9 col-md-offset-3">
                            <button class="btn btn-primary" type="submit">{_ Save Instagram Settings _}</button>
                        </div>
                    </div>

                </div>
            </div>
        </div>
    </div>
</form>

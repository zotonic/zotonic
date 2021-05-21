{% wire id="admin_twitter" type="submit" postback="admin_twitter" delegate=`mod_twitter` %}
<form name="admin_twitter" id="admin_twitter" class="form" method="POST" action="postback">
    <div class="row">
        <div class="col-md-6">
            <div class="widget">
                <h3 class="widget-header"><span class="fa fa-twitter"></span> Twitter</h3>
                <div class="widget-content">
                    <p class="help-block">
                        {_ Application keys can be found in _} <a href="https://apps.twitter.com" title="Application Management" target="_blank">{_ Your Twitter Application Management page _}</a>
                    </p>

                    <fieldset>

                        <div class="form-group label-floating">
                            <input type="text" id="twitter_consumer_key" name="consumer_key" value="{{ m.config.mod_twitter.consumer_key.value|escape }}" class="form-control" placeholder="{_ Consumer Key _}">
                            <label class="control-label" for="twitter_consumer_key">{_ Consumer Key _}</label>
                        </div>

                        <div class="form-group label-floating">
                            <input type="text" id="twitter_consumer_secret" name="consumer_secret" value="{{ m.config.mod_twitter.consumer_secret.value|escape }}" class="form-control" placeholder="{_ Consumer Secret _}">
                            <label class="control-label" for="consumer_secret">{_ Consumer Secret _}</label>
                        </div>

                        <div class="form-group">
                            <label class="checkbox">
                                <input type="checkbox" id="twitter_useauth" name="useauth" {% if m.config.mod_twitter.useauth.value %}checked="checked"{% endif %} value="1" />
                                {_ Use Twitter authentication _}
                            </label>
                        </div>
                    </fieldset>

                    <fieldset>

                        <p>{_ To import tweets, Zotonic needs an access token and secret. You can find these on the <b>Keys and Access Tokens</b> tab on your Twitter application page. _}</p>

                        <div class="form-group label-floating">
                            <input type="text" id="twitter_access_token" name="access_token" value="{{ m.config.mod_twitter.access_token.value|escape }}" class="form-control" placeholder="{_ Access Token _}">
                            <label class="control-label" for="twitter_access_token">{_ Access Token _}</label>
                        </div>
                        <div class="form-group label-floating">
                            <input type="text" id="twitter_access_token_secret" name="access_token_secret" value="{{ m.config.mod_twitter.access_token_secret.value|escape }}" class="form-control" placeholder="{_ Access Token Secret _}">
                            <label class="control-label" for="twitter_access_token_secret">{_ Access Token Secret _}</label>
                        </div>
                        <div class="form-group label-floating">
                            <textarea id="twitter_follow" rows="4" name="follow" value="" class="form-control" placeholder="{_ Follow tags/accounts _}">{{ m.config.mod_twitter.follow.value|escape }}</textarea>
                            <label class="control-label" for="twitter_follow">{_ Follow tags/accounts _}</label>

                            <p class="help-block">
                                {_ Separate #tags, @usernames and phrases with commas or newlines. _}
                            </p>
                        </div>
                        <div class="form-group">
                            <label class="control-label" for="twitter_max_feed_backoff">{_ Maximum poll delay _}</label>
                            {% with m.config.mod_twitter.max_feed_backoff.value|default:"3600" as backoff %}
                                <select class="form-control" name="max_feed_backoff" id="twitter_max_feed_backoff">
                                    <option value="60" {% if backoff == 60 %}selected{% endif %}>{_ 1 minute _}</option>
                                    <option value="120" {% if backoff == 120 %}selected{% endif %}>{_ 2 minutes _}</option>
                                    <option value="300" {% if backoff == 300 %}selected{% endif %}>{_ 5 minutes _}</option>
                                    <option value="600" {% if backoff == 600 %}selected{% endif %}>{_ 10 minutes _}</option>
                                    <option value="900" {% if backoff == 900 %}selected{% endif %}>{_ 15 minutes _}</option>
                                    <option value="1800" {% if backoff == 1800 %}selected{% endif %}>{_ 30 minutes _}</option>
                                    <option value="3600" {% if backoff == 3600 %}selected{% endif %}>{_ 1 hour _}</option>
                                    <option value="7200" {% if backoff == 7200 %}selected{% endif %}>{_ 2 hours _}</option>
                                </select>
                            {% endwith %}
                            <p class="help-block">
                                {_ Twitter is periodically polled for new content. If there is no content then the waiting period will be extended up to this maximum. _}
                            </p>
                        </div>
                    </fieldset>

                    <div class="form-group">
                        <button class="btn btn-primary" type="submit">{_ Save Twitter Settings _}</button>
                    </div>

                </div>
            </div>
        </div>
    </div>
</form>

{% wire id="admin_twitter" type="submit" postback="admin_twitter" delegate=`mod_twitter` %}
<form name="admin_twitter" id="admin_twitter" class="form-horizontal" method="POST" action="postback">
    <div class="row-fluid">
        <div class="span9">
            <div class="widget">
                <h3 class="widget-header"><span class="icon-twitter-sign"></span> Twitter</h3>
                <div class="widget-content">

                    <div class="clearfix">
                        <h1 class="icon-twitter-sign pull-left">&nbsp;</h1>
                        <p>Twitter<br/>
                            <small>{_ You can find the application keys in _} <a href="https://apps.twitter.com" title="Application Management" target="_blank">{_ Your Twitter Application Management _}</small></a>
                        </p>
                    </div>

                    <fieldset>
                        <div class="control-group">
                            <label class="control-label" for="twitter_consumer_key">{_ Consumer Key _}</label>
                            <div class="controls">
                                <input type="text" id="twitter_consumer_key" name="consumer_key" value="{{ m.config.mod_twitter.consumer_key.value|escape }}" class="input-block-level" />
                            </div>
                        </div>
                        
                        <div class="control-group">
                            <label class="control-label" for="consumer_secret">{_ Consumer Secret _}</label>
                            <div class="controls">
                                <input type="text" id="twitter_consumer_secret" name="consumer_secret" value="{{ m.config.mod_twitter.consumer_secret.value|escape }}" class="input-block-level" />
                            </div>
                        </div>

                        <div class="control-group">
                            <div class="controls">
                                <label class="checkbox" for="twitter_useauth">
                                    <input type="checkbox" id="twitter_useauth" name="useauth" {% if m.config.mod_twitter.useauth.value %}checked="checked"{% endif %} value="1" />
                                    {_ Use Twitter authentication _}
                                </label>
                            </div>
                        </div>
                    </fieldset>

                    <fieldset>

                        <p>{_ To import tweets, Zotonic needs an access token and secret. You can find these on the <b>Keys and Access Tokens</b> tab on your Twitter application page. _}</p>

                        <div class="control-group">
                            <label class="control-label" for="twitter_access_token">{_ Access Token _}</label>
                            <div class="controls">
                                <input type="text" id="twitter_access_token" name="access_token" value="{{ m.config.mod_twitter.access_token.value|escape }}" class="input-block-level" />
                            </div>
                        </div>


                        <div class="control-group">
                            <label class="control-label" for="twitter_access_token_secret">{_ Access Token Secret _}</label>
                            <div class="controls">
                                <input type="text" id="twitter_access_token_secret" name="access_token_secret" value="{{ m.config.mod_twitter.access_token_secret.value|escape }}" class="input-block-level" />
                            </div>
                        </div>

                        <div class="control-group">
                            <label class="control-label" for="twitter_follow">{_ Follow tags/accounts _}</label>
                            <div class="controls">
                                <textarea id="twitter_follow" name="follow" class="input-block-level" />{{ m.config.mod_twitter.follow.value|escape }}</textarea>

                                <p class="help-block">
                                    {_ Separate #tags, @usernames and phrases with commas or newlines. _}
                                </p>

                            </div>
                        </div>

                    </fieldset>

                    <div class="form-actions">
                        <div class="controls">
                            <button class="btn btn-primary" type="submit">{_ Save Twitter Settings _}</button>
                        </div>
                    </div>
                    
                </div>
            </div>
        </div>
    </div>
</form>

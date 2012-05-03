<div class="widget">
    <h3 class="widget-header">Google</h3>
    <div class="widget-content">

        <div class="control-group">
            <label class="control-label" for="seo_google-analytics">{_ Google Analytics tracking id _}</label>
            <div class="controls">
                <input type="text" id="seo_google-analytics" name="seo_google-analytics" value="{{ m.config.seo_google.analytics.value|escape }}" class="span4" />
                <p class="help-block">
                    You find this id in the tracking script, it has the format <strong>UA-123456-1</strong>. 
                    <a href="http://www.google.com/support/googleanalytics/bin/answer.py?hl=en&amp;answer=55603" title="Google Analytics Help">Where can I find my tracking script?</a>
                </p>
            </div>
        </div>

        <div class="control-group">
            <label class="control-label" for="seo_google-webmaster">{_ Google Webmaster Tools _}</label>
            <div class="controls">
                <input type="text" id="seo_google-webmaster_verify" name="seo_google-webmaster_verify" value="{{ m.config.seo_google.webmaster_verify.value|escape }}" class="span8" />
                <p class="help-block">
                    Enter here the verification code for <a href="https://www.google.com/webmasters/tools">Google Webmaster Tools</a>. Copy the value of the content attribute in the meta tag provided by Google.
                </p>
            </div>
        </div>
    </fieldset>
</div>

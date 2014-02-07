{% extends "admin_base.tpl" %}

{% block title %}{_ Search Engine Optimization _}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ Search Engine Optimization _}</h2>

    <p>{_ Here you find settings to optimize the indexing of your site by search engines. _}</p>
</div>

{% wire id="admin-seo" type="submit" postback="admin_seo" %}
<form name="admin-seo" id="admin-seo" method="POST" action="postback">
    <div class="widget">
        <h3 class="widget-header">{_ General SEO Optimization _}</h3>
        <div class="widget-content">
            <div class="control-group">
                <label class="control-label" for="seo-keywords">{_ Keywords to include on every page. Separate keywords with a "," _}</label>
                <div class="controls">
                    <input type="text" id="seo-keywords" name="seo-keywords" value="{{ m.config.seo.keywords.value|escape }}" class="span8" />
                </div>
            </div>

            <div class="control-group">
                <label class="control-label" for="seo-description">{_ Description to include on every page _}</label>
                <div class="controls">
                    <input type="text" id="seo-description" name="seo-description" value="{{ m.config.seo.description.value|escape }}" class="span8" />
                </div>
            </div>

            <div class="control-group">
                <div class="controls">
                    <label class="inline checkbox" for="seo-noindex" title="{_ Add a noindex, nofollow element to all pages. _}">
                        <input type="checkbox" id="seo-noindex" name="seo-noindex" value="1" {% if m.config.seo.noindex.value %}checked="checked"{% endif %} />
                        {_ Exclude this site from search engines _}
                    </label>
                </div>
            </div>
        </div>
    </div>

    <div class="widget">
        <h3 class="widget-header">Google</h3>
        <div class="widget-content">

            <div class="control-group">
                <label class="control-label" for="seo_google-analytics">{_ Google Analytics tracking id _}</label>
                <div class="controls">
                    <input type="text" id="seo_google-analytics" name="seo_google-analytics" value="{{ m.config.seo_google.analytics.value|escape }}" class="span4" />
                    <p class="help-block">
                        {_ You find this id in the tracking script, it has the format _} <strong>UA-123456-1</strong>. 
                        <a href="https://support.google.com/analytics/bin/answer.py?hl=en&amp;answer=1008080" title="Google Analytics Help">{_ Where can I find my tracking script? _}</a>
                    </p>
                </div>
            </div>

            <div class="control-group">
                <label class="control-label" for="seo_google-webmaster">{_ Google Webmaster Tools _}</label>
                <div class="controls">
                    <input type="text" id="seo_google-webmaster_verify" name="seo_google-webmaster_verify" value="{{ m.config.seo_google.webmaster_verify.value|escape }}" class="span8" />
                    <p class="help-block">
                        {_ Enter here the verification code for _} <a href="https://www.google.com/webmasters/tools">{_ Google Webmaster Tools _}</a>. {_ Copy the value of the content attribute in the meta tag provided by Google. _}
                    </p>
                </div>
            </div>
        </div>
    </div>

    <div class="widget">
        <h3 class="widget-header">Bing &amp; Yahoo!</h3>
        <div class="widget-content">
            <div class="control-group">
                <label class="control-label" for="seo_bing-webmaster_verify">{_ Bing Webmaster validation id _}</label>
                <div class="controls">
                    <input type="text" id="seo_bing-webmaster_verify" name="seo_bing-webmaster_verify" value="{{ m.config.seo_bing.webmaster_verify.value|escape }}" class="span4" />
                    <p class="help-block">
                        {_ Enter here the verification code for _} <a href="https://ssl.bing.com/webmaster/home/mysites">{_ Bing Webmaster _}</a>.<br/>
                        {_ You find this id in the content attribute of the meta tag, it has the format _} <strong>8103A84C247E45185F39A97C50D40731</strong>. 
                    </p>
                </div>
            </div>
        </div>
    </div>
    
    {% all include "_admin_seo_config.tpl" %}

    <div class="form-actions">
        <button class="btn btn-primary" type="submit">{_ Save SEO settings _}</button>
    </div>            
</form>


{% endblock %}

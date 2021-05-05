{% extends "admin_base.tpl" %}

{% block title %}{_ Search Engine Optimization _}{% endblock %}

{% block content %}

    <div class="admin-header">
        <h2>{_ Search Engine Optimization _}</h2>
        <p>{_ Here you find settings to optimize the indexing of your site by search engines. _}</p>
    </div>

    {% wire id="admin-seo" type="submit" postback="admin_seo" %}
    <form name="admin-seo" id="admin-seo" method="POST" action="postback">

        <div class="row">
            <div class="col-md-7">

                <div class="widget">
                    <h3 class="widget-header">{_ General SEO Optimization _}</h3>
                    <div class="widget-content">
                        <div class="form-group label-floating">
                            <input type="text" id="seo-keywords" name="seo-keywords" value="{{ m.config.seo.keywords.value|escape }}" class="form-control" placeholder="{_ Keywords _}">
                            <label class="control-label" for="seo-keywords">{_ Keywords _}</label>
                            <p class="help-block">
                                {_ Keywords to include on every page. Separate keywords with a "," _}
                            </p>
                        </div>

                        <div class="form-group label-floating">
                            <input type="text" id="seo-description" name="seo-description" value="{{ m.config.seo.description.value|escape }}" class="form-control" placeholder="{_ Description _}">
                            <p class="help-block">
                            <label class="control-label" for="seo-description">{_ Description _}</label>
                                {_ Description to include on every page _}
                            </p>
                        </div>

                        {% if m.site.seo.noindex|is_defined %}
                            <p class="alert alert-info">
                                {% if m.site.seo.noindex %}
                                    {_ The site configuration file excludes this site from search engines. _}
                                {% else %}
                                    {_ The site configuration file allows this site to be indexed by search engines. _}
                                {% endif %}
                            </p>
                        {% else %}
                            <div class="form-group">
                                <label class="checkbox">
                                    <input type="checkbox" id="seo-noindex" name="seo-noindex" value="1" {% if m.config.seo.noindex.value %}checked="checked"{% endif %} />
                                    {_ Exclude this site from search engines _}
                                </label>
                                <p class="help-block">
                                    {_ Add a noindex, nofollow element to all pages. _}
                                </p>
                            </div>
                        {% endif %}
                    </div>
                </div>

                <div class="widget">
                    <h3 class="widget-header">Google</h3>
                    <div class="widget-content">
                        <div class="form-group label-floating">
                            <input type="text" id="seo_google-analytics" name="seo_google-analytics" value="{{ m.config.seo_google.analytics.value|escape }}" class="form-control" placeholder="{_ Google Analytics tracking id _}">
                            <label class="control-label" for="seo_google-analytics">{_ Google Analytics tracking id _}</label>
                            <p class="help-block">
                                {_ You find this id in the tracking script, it has the format _} <strong>G-..........</strong> {_ or _} <strong>UA-123456-1</strong>.
                                <a href="https://support.google.com/analytics/bin/answer.py?hl=en&amp;answer=1008080" title="Google Analytics Help" rel="noopener noreferrer" target="_blank">{_ Where can I find my tracking script? _}</a>
                            </p>
                        </div>

                        <div class="form-group label-floating">
                            <input type="text" id="seo_google-gtm" name="seo_google-gtm" value="{{ m.config.seo_google.gtm.value|escape }}" class="form-control" placeholder="{_ Google Tag Manager id _}">
                            <label class="control-label" for="seo_google-gtm">{_ Google Tag Manager id _}</label>
                            <p class="help-block">
                                {_ You find this on your _} <a href="https://tagmanager.google.com/" title="Google Tag Manager" rel="noopener noreferrer" target="_blank">{_ Google Tag Manager account page _}</a>, {_ it has the format _} <strong>GTM-......</strong>.
                            </p>
                        </div>

                        <div class="form-group label-floating">
                            <input type="text" id="seo_google-webmaster_verify" name="seo_google-webmaster_verify" value="{{ m.config.seo_google.webmaster_verify.value|escape }}" class="form-control" placeholder="{_ Google Search Console _}">
                            <label class="control-label col-md-4" for="seo_google-webmaster">{_ Google Search Console _}</label>
                            <p class="help-block">
                                {_ Enter here the verification code for _} <a href="https://search.google.com/search-console" rel="noopener noreferrer" target="_blank">{_ Google Search Console _}</a>. {_ Copy the value of the content attribute in the meta tag provided by Google. _}
                            </p>
                        </div>
                    </div>
                </div>

                <div class="widget">
                    <h3 class="widget-header">Bing &amp; Yahoo!</h3>
                    <div class="widget-content">
                        <div class="form-group label-floating">
                            <input type="text" id="seo_bing-webmaster_verify" name="seo_bing-webmaster_verify" value="{{ m.config.seo_bing.webmaster_verify.value|escape }}" class="form-control" placeholder="{_ Bing Webmaster validation id _}">
                            <label class="control-label" for="seo_bing-webmaster_verify">{_ Bing Webmaster validation id _}</label>
                            <p class="help-block">
                                {_ Enter here the verification code for _} <a href="https://www.bing.com/webmasters" rel="noopener noreferrer" target="_blank">{_ Bing Webmaster _}</a>.<br/>
                                {_ You find this id in the content attribute of the meta tag, it has the format _} <strong>8103A84C247E45185F39A97C50D40731</strong>.
                            </p>
                        </div>
                    </div>
                </div>

                <div class="widget">
                    <h3 class="widget-header">Yandex</h3>
                    <div class="widget-content">
                        <div class="form-group label-floating">
                            <input type="text" id="seo_yandex-webmaster_verify" name="seo_yandex-webmaster_verify" value="{{ m.config.seo_yandex.webmaster_verify.value|escape }}" class="form-control" placeholder="{_ Yandex Webmaster validation id _}">
                            <label class="control-label" for="seo_yandex-webmaster_verify">{_ Yandex Webmaster validation id _}</label>
                            <p class="help-block">
                                {_ Enter here the verification code for _} <a href="https://webmaster.yandex.com/sites/?noRedirect=yes&amp;hostnameFilter=" rel="noopener noreferrer" target="_blank">{_ Yandex Webmaster _}</a>.<br/>
                                {_ You find this id in the content attribute of the meta tag, it has the format _} <strong>937800ae8c5a6cbf</strong>.
                            </p>
                        </div>
                    </div>
                </div>

            </div>

            <div class="col-md-5">
                {% all include "_admin_seo_config.tpl" %}

                <div class="form-actions">
                    <button class="btn btn-primary" type="submit">{_ Save SEO settings _}</button>
                </div>
            </div>

        </div>


    </form>


{% endblock %}

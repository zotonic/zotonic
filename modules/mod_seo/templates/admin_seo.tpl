{% extends "admin_base.tpl" %}

{% block title %}{_ Search Engine Optimization _}{% endblock %}

{% block content %}

<div class="admin-header">

    <h2>{_ Search Engine Optimization _}</h2>

    <p>{_ Here you find settings to optimize the indexing of your site by search engines. _}</p>

    <div class="well">
        <button class="btn btn-primary" type="submit">{_ Save SEO settings _}</button>
    </div>
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
                        <input type="checkbox" id="seo-noindex" name="seo-noindex" value="1" {% if m.config.seo.noindex.value %}check="checked"{% endif %} />
                        {_ Exclude this site from search engines _}
                    </label>
                </div>
            </div>

        </div>
    </div>
    
    {% all include "_admin_seo_config.tpl" %}
            
</form>


{% endblock %}

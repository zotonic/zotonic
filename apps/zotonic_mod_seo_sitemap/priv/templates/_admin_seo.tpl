<div class="widget">
    <h3 class="widget-header">{_ Sitemap _}</h3>
    <div class="widget-content">
        <p>
            {_ Zotonic maintains a sitemap of all pages. It is good to rebuild this sitemap after language or other structural content changes. _}<br>
            {_ The rebuild is done in the background and can take a while. _}
            <a href="{% url admin_log type="info" message="sitemap" %}">{_ Check the logs for progress. _}</a>
        </p>

        <p>
            {% button class="btn btn-default" text=_"Rebuild sitemap"
                      action={postback
                            postback=`sitemap_rebuild`
                            delegate=`mod_seo_sitemap`
                        }
            %}
        </p>

        <p class="help-block">
            <span class="glyphicon glyphicon-info-sign"></span>
            {_ The location of the sitemap is: _}
            {% with `x-default` as z_language %}
                <tt>{% url sitemap_xml absolute_url %}</tt>
            {% endwith %}
    </div>
</div>

{% with id.is_a.category as is_category %}
<fieldset>
    {% if is_category %}
        <legend>{_ Sitemap settings for all pages of this category _}</legend>
        <p class="help-block">{_ These settings apply to all pages of this category. They define hints for search engines indexing the site. _}</p>
    {% else %}
        <legend>{_ Sitemap settings _}</legend>
        <p class="help-block">{_ These settings define hints for search engines indexing the site. _}</p>
    {% endif %}

    <div class="form-group row">
        <div class="col-md-6">
            <label class="control-label" for="seo_sitemap_priority">
                {_ Priority in sitemap _}
            </label>
            <div>
                <select class="form-control" id="seo_sitemap_priority" name="seo_sitemap_priority">
                    {% if not is_category %}
                        <option value="" {% if id.seo_sitemap_priority|is_undefined %}selected{% endif %}>{_ Priority defined for category _} ({{ id.category_id.seo_sitemap_priority|default:"0.5" }})</option>
                    {% endif %}
                    <option value="0.0" {% if id.seo_sitemap_priority == '0.0' %}selected{% endif %}>0.0 &ndash; {_ not in sitemap _}</option>
                    <option value="0.1" {% if id.seo_sitemap_priority == '0.1' %}selected{% endif %}>0.1</option>
                    <option value="0.2" {% if id.seo_sitemap_priority == '0.2' %}selected{% endif %}>0.2</option>
                    <option value="0.3" {% if id.seo_sitemap_priority == '0.3' %}selected{% endif %}>0.3</option>
                    <option value="0.4" {% if id.seo_sitemap_priority == '0.4' %}selected{% endif %}>0.4</option>
                    {% if is_category %}
                        <option value="" {% if not id.seo_sitemap_priority %}selected{% endif %}>0.5 &ndash; {_ default_}</option>
                    {% else %}
                        <option value="0.5" {% if id.seo_sitemap_priority == "0.5" %}selected{% endif %}>0.5</option>
                    {% endif %}
                    <option value="0.6" {% if id.seo_sitemap_priority == '0.6' %}selected{% endif %}>0.6</option>
                    <option value="0.7" {% if id.seo_sitemap_priority == '0.7' %}selected{% endif %}>0.7</option>
                    <option value="0.8" {% if id.seo_sitemap_priority == '0.8' %}selected{% endif %}>0.8</option>
                    <option value="0.9" {% if id.seo_sitemap_priority == '0.9' %}selected{% endif %}>0.9</option>
                    <option value="1.0" {% if id.seo_sitemap_priority == '1.0' %}selected{% endif %}>1.0</option>
                </select>
            </div>
            <p class="help-block">{_ The home page defaults to 1.0, pages with a page path default to 0.8. _}</p>
        </div>

        <div class="col-md-6">
            <label class="control-label" for="seo_sitemap_changefreq">
                {_ Change frequency for sitemap _}
            </label>
            <div>
                <select class="form-control" id="seo_sitemap_changefreq" name="seo_sitemap_changefreq">
                    {% if not is_category %}
                        <option value="" {% if id.seo_sitemap_changefreq|is_undefined %}selected{% endif %}>{_ Change frequency defined for category _}
                            ({{ id.category_id.seo_sitemap_changefreq|default:"weekly" }})
                        </option>
                    {% endif %}
                    <option value="always" {% if id.seo_sitemap_changefreq == 'always' %}selected{% endif %}>{_ Always _}</option>
                    <option value="hourly" {% if id.seo_sitemap_changefreq == 'hourly' %}selected{% endif %}>{_ Hourly _}</option>
                    <option value="daily" {% if id.seo_sitemap_changefreq == 'daily' %}selected{% endif %}>{_ Daily _}</option>
                    {% if is_category %}
                        <option value="" {% if not id.seo_sitemap_changefreq %}selected{% endif %}>{_ Weekly (default) _}</option>
                    {% else %}
                        <option value="weekly" {% if id.seo_sitemap_changefreq == 'weekly' %}selected{% endif %}>{_ Weekly _}</option>
                    {% endif %}
                    <option value="monthly" {% if id.seo_sitemap_changefreq == 'monthly' %}selected{% endif %}>{_ Monthly _}</option>
                    <option value="yearly" {% if id.seo_sitemap_changefreq == 'yearly' %}selected{% endif %}>{_ Yearly _}</option>
                    <option value="never" {% if id.seo_sitemap_changefreq == 'never' %}selected{% endif %}>{_ Never _}</option>
                </select>
            </div>
        </div>
    </div>
</fieldset>
{% endwith %}

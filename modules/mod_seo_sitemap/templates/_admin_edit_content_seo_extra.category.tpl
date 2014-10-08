<fieldset>
<legend>{_ Sitemap settings _}</legend>

<p class="help-block">{_ These settings apply to all pages of this category. They define hints for search engines indexing the site. _}</p>

<div class="row">
    <div class="form-group col-lg-6 col-md-6">
        <label class="control-label" for="seo_sitemap_priority">
            {_ Priority in sitemap _}
        </label>
        <div>
            <select class="form-control" id="seo_sitemap_priority" name="seo_sitemap_priority">
                <option value="0.0" {% if id.seo_sitemap_priority == '0.0' %}selected{% endif %}>0.0</option>
                <option value="0.1" {% if id.seo_sitemap_priority == '0.1' %}selected{% endif %}>0.1</option>
                <option value="0.2" {% if id.seo_sitemap_priority == '0.2' %}selected{% endif %}>0.2</option>
                <option value="0.3" {% if id.seo_sitemap_priority == '0.3' %}selected{% endif %}>0.3</option>
                <option value="0.4" {% if id.seo_sitemap_priority == '0.4' %}selected{% endif %}>0.4</option>
                <option value="" {% if not id.seo_sitemap_priority %}selected{% endif %}>0.5 ({_ default_})</option>
                <option value="0.6" {% if id.seo_sitemap_priority == '0.6' %}selected{% endif %}>0.6</option>
                <option value="0.7" {% if id.seo_sitemap_priority == '0.7' %}selected{% endif %}>0.7</option>
                <option value="0.8" {% if id.seo_sitemap_priority == '0.8' %}selected{% endif %}>0.8</option>
                <option value="0.9" {% if id.seo_sitemap_priority == '0.9' %}selected{% endif %}>0.9</option>
                <option value="1.0" {% if id.seo_sitemap_priority == '1.0' %}selected{% endif %}>1.0</option>
            </select>
        </div>
    </div>

    <div class="form-group col-lg-6 col-md-6">
        <label class="control-label" for="seo_sitemap_changefreq">
            {_ Change frequency for sitemap _}
        </label>
        <div>
            <select class="form-control" id="seo_sitemap_changefreq" name="seo_sitemap_changefreq">
                <option value="always" {% if id.seo_sitemap_changefreq == 'always' %}selected{% endif %}>{_ Always _}</option>
                <option value="hourly" {% if id.seo_sitemap_changefreq == 'hourly' %}selected{% endif %}>{_ Hourly _}</option>
                <option value="daily" {% if id.seo_sitemap_changefreq == 'daily' %}selected{% endif %}>{_ Daily _}</option>
                <option value="" {% if not id.seo_sitemap_changefreq %}selected{% endif %}>{_ Weekly (default) _}</option>
                <option value="monthly" {% if id.seo_sitemap_changefreq == 'monthly' %}selected{% endif %}>{_ Monthly _}</option>
                <option value="yearly" {% if id.seo_sitemap_changefreq == 'yearly' %}selected{% endif %}>{_ Yearly _}</option>
                <option value="never" {% if id.seo_sitemap_changefreq == 'never' %}selected{% endif %}>{_ Never _}</option>
            </select>
        </div>
    </div>
</div>
</fieldset>

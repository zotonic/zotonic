{% extends "admin_base.tpl" %}

{% block title %}{_ Search Engine Optimization _}{% endblock %}

{% block content %}

<div id="content" class="zp-85">
	<div class="block clearfix">

	{% wire id="admin-seo" type="submit" postback="admin_seo" %}
	<form name="admin-seo" id="admin-seo" method="POST" action="postback">
		<h2>{_ Search Engine Optimization _}</h2>
		<p>{_ Here you find settings to optimize the indexing of your site by search engines. _}</p>

		<div class="item-wrapper">
			<h3 class="above-item">{_ General SEO Optimization _}</h3>
			<div class="item">
				<fieldset class="admin-form">
					<div class="form-item clearfix">
						<label for="seo-keywords">{_ Keywords to include on every page. Separate keywords with a "," _}</label>
						<input type="text" id="seo-keywords" name="seo-keywords" value="{{ m.config.seo.keywords.value|escape }}" />
					</div>

					<div class="form-item clearfix">
						<label for="seo-description">{_ Description to include on every page _}</label>
						<input type="text" id="seo-description" name="seo-description" value="{{ m.config.seo.description.value|escape }}" />
					</div>

					<div class="form-item clearfix">
						<label for="seo-noindex" title="{_ Add a noindex, nofollow element to all pages. _}">
							<input type="checkbox" id="seo-noindex" name="seo-noindex" value="1" {% if m.config.seo.noindex.value %}check="checked"{% endif %} />
							{_ Exclude this site from search engines _}
						</label>
					</div>

				</fieldset>
			</div>
		</div>
		
		{% all include "_admin_seo_config.tpl" %}
		
		<div class="form-item clearfix">
			<button type="submit">{_ Save SEO settings _}</button>
		</div>
	</form>

	</div>
</div>

{% endblock %}
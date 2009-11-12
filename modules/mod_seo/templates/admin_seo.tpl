{% extends "admin_base.tpl" %}

{% block title %} Admin SEO {% endblock %}

{% block content %}

<div id="content" class="zp-85">
	<div class="block clearfix">

	{% wire id="admin-seo" type="submit" postback="admin_seo" %}
	<form name="admin-seo" id="admin-seo" method="POST" action="postback">
		<h2>Search Engine Optimization</h2>
		<p>Here you find settings to optimize the indexing of your site by search engines.</p>

		<div class="item-wrapper">
			<h3 class="above-item">General SEO Optimization</h3>
			<div class="item">
				<fieldset class="admin-form">
					<div class="form-item clearfix">
						<label for="seo-keywords">Keywords to include on every page. Separate keywords with a ","</label>
						<input type="text" id="seo-keywords" name="seo-keywords" value="{{ m.config.seo.keywords.value|escape }}" />
					</div>

					<div class="form-item clearfix">
						<label for="seo-description">Description to include on every page</label>
						<input type="text" id="seo-description" name="seo-description" value="{{ m.config.seo.description.value|escape }}" />
					</div>

				</fieldset>
			</div>
		</div>
		
		{% all include "_admin_seo_config.tpl" %}
		
		<div class="form-item clearfix">
			<button type="submit">Save SEO settings</button>
		</div>
	</form>

	</div>
</div>

{% endblock %}
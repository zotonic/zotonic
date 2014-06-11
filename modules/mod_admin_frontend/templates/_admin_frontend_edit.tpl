{% javascript %}
	$('.tree-list .active').removeClass('active');
	$('.tree-list div[data-page-id="{{ id }}"]').addClass('active');
	$('#save-buttons').hide().fadeIn();
{% endjavascript %}

{% block tinymce_init %}
	{% catinclude "_admin_frontend_tinymce_init.tpl" id tree_id=tree_id %}
{% endblock %}

{% block rscform %}

{% with id.is_editable as is_editable %}
{% with m.config.i18n.language_list.list as languages %}
{% wire id="rscform" type="submit" postback={rscform view_location=view_location} delegate=`controller_admin_edit` %}
<form id="rscform" method="post" action="postback" class="form-horizontal">
	<input type="hidden" name="id" value="{{ id }}" />

	<div class="meta-data row-fluid">
		<div class="span10">
			{% block meta_data_first %}{% endblock %}
			<p>
				{{ id.category_id.title }}

				<span class="publication-dates">
					<label for="is_published" class="checkbox inline">
			    		<input type="checkbox" id="is_published" name="is_published" value="1" {% if id.is_published %}checked="checked"{% endif %}/>
			    	    {_ Published _}
				    </label>

					{% include "_edit_date.tpl" date=id.publication_start name="publication_start" is_end=0 %}
					{_ till _} 
					{% include "_edit_date.tpl" date=id.publication_end name="publication_end" is_end=1 %}

					{{ m.req.timezone }}
				</span>
			</p>
		</div>

		<div class="span2">
			<a id="meta-toggle" href="#meta" role="button" class="btn btn-small pull-right"><i class="icon-cog"></i></a>
			{% javascript %}
			$('#meta-toggle').click(function(e) {
				if ($('#meta-extra').is(":visible")) {
					$('.meta-extra').slideUp();
				} else {
					$('.meta-extra').slideUp();
					$('#meta-extra').slideDown();
				}
				e.preventDefault();
			});
			{% endjavascript %}
		</div>
	</div>

	{% block meta_data_after %}
	{% endblock %}

	<div class="row-fluid meta-extra" id="meta-extra" style="display:none">
		<ul class="nav nav-tabs">
			{% block meta_tabs %}{% endblock %}
			{% if m.modules.info.mod_translation.enabled %}
				<li><a href="#meta-language" data-toggle="tab">{_ Language _}</a></li>
			{% endif %}
			<li><a href="#meta-acl" data-toggle="tab">{_ Access control _}</a></li>
		</ul>
		<div class="tab-content">
			{% block meta_panels %}{% endblock %}
			<div class="tab-pane" id="meta-language">
				{% optional include "_translation_edit_languages.tpl" %}
			</div>
			<div class="tab-pane" id="meta-acl">
				{% include "_admin_edit_visible_for.tpl" id=id is_admin_frontend %}
			</div>
		</div>

		{% javascript %}
			$('#meta-extra .nav-tabs a:first').tab('show');
		{% endjavascript %}
	</div>

	<div id="poststuff">
	{% optional include "_translation_init_languages.tpl" %}
	{% block edit_blocks %}
		{% catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}

		{% if id.category_id.feature_show_address %}
			{% catinclude "_admin_edit_content_address.tpl" id is_editable=is_editable languages=languages %}
		{% endif %}
		
		{% all catinclude "_admin_edit_content.tpl" id is_editable=is_editable languages=languages %}

		{% if id.is_a.media or id.medium %}
			{% include "_admin_edit_content_media.tpl" %}
		{% endif %}

		{% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %}
		{% catinclude "_admin_edit_blocks.tpl" id is_editable=is_editable languages=languages %}
		{% catinclude "_admin_edit_depiction.tpl" id is_editable=is_editable languages=languages %}
	{% endblock %}
	</div>

	{# Hidden safe buttons and publish state - controlled via the nabvar #}
	<div style="display: none">
		<span id="button-prompt">{% block nav_prompt %}{{ id.category_id.title }}{% endblock %}</span>

		{% block buttons %}
			{% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}
		
			{% if id.is_editable %}
				{% button type="submit" id="save_view" class="btn" text=_"Save &amp; view" title=_"Save and view the page." %}
			{% else %}
				{% button id="save_view" class="btn btn-primary" text=_"View" title=_"View this page." action={redirect id=id} %}
			{% endif %}
		{% endblock %}
	</div>
</form>
{% endwith %}
{% endwith %}

{% javascript %}
	$("#save-buttons .brand").html($('#button-prompt').html());

	setTimeout(function() {
		$('.language-tabs').on('shown', '> li > a[data-toggle="tab"]', function (e) {
			if (e.target != e.relatedTarget) {
				var lang = $(e.target).parent().attr('lang');
				$("li[lang='"+lang+"']:visible > a").tab('show');
			}
		});
	}, 10);
{% endjavascript %}

{% endblock %}


{% javascript %}
	$('.tree-list .active').removeClass('active');
	$('.tree-list div[data-page-id="{{ id }}"]').addClass('active');
	$('#save-buttons').hide().fadeIn();
{% endjavascript %}

{% block tinymce_init %}
	{% catinclude "_admin_frontend_tinymce_init.tpl" id tree_id=tree_id %}
{% endblock %}

{% with id.is_editable as is_editable %}
{% with m.config.i18n.language_list.list as languages %}
{% wire id="rscform" type="submit" postback="rscform" delegate=`controller_admin_edit` %}
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
		{% if m.modules.info.mod_translation.enabled %}
		<fieldset>
			<legend>{_ Language _}</legend>
			{% optional include "_translation_edit_languages.tpl" %}
		</fieldset>
		{% endif %}
		<fieldset>
			<legend>{_ Access control _}</legend>
			{% include "_admin_edit_visible_for.tpl" id=id is_admin_frontend %}
		</fieldset>
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
		{% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}
	
		{% if id.is_editable %}
			{% button type="submit" id="save_view" class="btn" text=_"Save &amp; view" title=_"Save and view the page." %}
		{% else %}
			{% button id="save_view" class="btn btn-primary" text=_"View" title=_"View this page." action={redirect id=id} %}
		{% endif %}
	</div>
</form>
{% endwith %}
{% endwith %}

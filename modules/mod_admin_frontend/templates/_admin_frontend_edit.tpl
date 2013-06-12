{% javascript %}
	$('.tree-list .active').removeClass('active');
	$('.tree-list div[data-page-id="{{ id }}"]').addClass('active');
	$('#save-buttons').hide().fadeIn();
{% endjavascript %}

{% with id.is_editable as is_editable %}
{% with m.config.i18n.language_list.list as languages %}
{% wire id="rscform" type="submit" postback="rscform" delegate=`controller_admin_edit` %}
<form id="rscform" method="post" action="postback" class="form-horizontal">
	<input type="hidden" name="id" value="{{ id }}" />

	<div id="poststuff">
		{% all catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}

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
	</div>

	{# Hidden language checkboxes. TODO: make this user selectable (popup?) #}
	{% if m.modules.info.mod_translation.enabled %}
		<div style="display: none">
			{% include "_translation_edit_languages.tpl" %}
		</div>
	{% endif %}

	{# Hidden safe buttons and publish state - controlled via the nabvar #}
	<div style="display: none">
		{% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}
	
		{% if id.is_editable %}
			{% button type="submit" id="save_view" class="btn" text=_"Save &amp; view" title=_"Save and view the page." %}
		{% else %}
			{% button id="save_view" class="btn btn-primary" text=_"View" title=_"View this page." action={redirect id=id} %}
		{% endif %}

		<label for="is_published" class="checkbox inline">
    		<input type="checkbox" id="is_published" name="is_published" value="1" {% if id.is_published %}checked="checked"{% endif %}/>
    	    {_ Published _}
	    </label>
   	    {% javascript %}
    		$('#is_published_navbar').attr('checked', $('#is_published').is(':checked'));
   	    {% endjavascript %}
	</div>
</form>
{% endwith %}
{% endwith %}

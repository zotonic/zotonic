{% with id.is_editable as is_editable %}
{% wire id="rscform" type="submit" postback="rscform" delegate=`controller_admin_edit` %}
<form id="rscform" method="post" action="postback" class="form-horizontal">
	<input type="hidden" name="id" value="{{ id }}" />

	{% include "_admin_frontend_footer.tpl" %}

	<div id="poststuff">
		{% all catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}

		{% if id.category_id.feature_show_address|if_undefined:`true` %}
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
</form>
{% endwith %}

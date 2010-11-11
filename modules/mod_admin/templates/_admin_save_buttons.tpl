<div class="form-item clearfix">
	<div class="right">
	{% if not r.is_editable %}
		{% button class="save-resource" text=_"save this page" disabled=not is_editable %}
	{% else %}
		{% button class="save-resource" text=_"save this page" %}
	{% endif %}
	{% button class="discard-resource" text=_"cancel" action={redirect back} %}
	</div>
	<div style="clear:both"></div>
</div>

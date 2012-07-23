<div class="tab-pane {% if is_active %}active{% endif %}" id="{{ tab }}-new">
	{% include "_action_dialog_new_rsc.tpl" 
			delegate="action_admin_dialog_new_rsc" 
			predicate=predicate
			callback=callback
	%}
</div>

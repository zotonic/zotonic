{% if scheduled %}
	<h3>Scheduled</h3>

	<p>After publication this page will be send to the following mailing lists:</p>

	<ul>
		{% for mailinglist_id in scheduled %}
			<li class="clear" id="mailinglist-{{ mailinglist_id }}">
				<a href="{% url admin_edit_rsc id=mailinglist_id %}">
					{{ m.rsc[mailinglist_id].title|default:"<em>withheld</em>" }}
				</a>
				{% button text="Cancel" class="do_tooltip" title="Cancel this mailing." 
						postback={dialog_mailing_cancel_confirm mailinglist_id=mailinglist_id page_id=id} 
						delegate="mod_mailinglist" 
						disabled=m.rsc[mailinglist_id].is_editable|not 
				%}
			</li>
		{% endfor %}
	</ul>
{% endif %}

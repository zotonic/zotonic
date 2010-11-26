{% if scheduled %}
	<h3>{_ Scheduled _}</h3>

	<p>{_ After publication this page will be send to the following mailing lists: _}</p>

	<ul>
		{% for mailinglist_id in scheduled %}
			<li class="clear" id="mailinglist-{{ mailinglist_id }}">
				<a href="{% url admin_edit_rsc id=mailinglist_id %}">
					{{ m.rsc[mailinglist_id].title|default:_"<em>withheld</em>" }}
				</a>
				{% button text=_"Cancel" class="do_tooltip" title=_"Cancel this mailing." 
						postback={dialog_mailing_cancel_confirm mailinglist_id=mailinglist_id page_id=id} 
						delegate="mod_mailinglist" 
						disabled=not m.rsc[mailinglist_id].is_editable 
				%}
			</li>
		{% endfor %}
	</ul>
{% endif %}

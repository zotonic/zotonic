<div class="item-wrapper" id="sort-user-credentials">
	<h3 class="above-item clearfix do_blockminifier">
		<span class="title">{_ Events for this artist _}</span>
		<span class="arrow">{_ make smaller _}</span>
	</h3>
	<div class="item clearfix">
		<div class="admin-form">
			<p>{_ Add event for this artist. _} <a href="javascript:void(0)" class="do_dialog" data-dialog="title: '{_ Help about artist\'s events._}', text: '{_ You can quickly add events for this artist.  When you want to delete an event, then click on the name of the event and delete it on the event\'s edit page. _}', width: '450px'">{_ Need more help? _}</a></p>
			{% button action={dialog_artist_event_add id=id} text=_"Make a new event" %}
	
			<h3 class="clear">{_ Existing events _}</h3>

			{% for event_id in m.search[{event_for_performer id=id pagelen=100}] %}
				<div class="rsc-edge">
					<span class="clearfix">
						<span class="unlink-item">
							<a href="{% url admin_edit_rsc id=event_id %}">
								{{ m.rsc[event_id].title|default:_"untitled event" }}
								{% if m.rsc[event_id].date_start %}
									/ {{ m.rsc[event_id].date_start|date:"b j, H:i" }}
								{% endif %}
							</a>
						</span>
					</span>
				</div>
			{% empty %}
				<p>{_ No performances yet. _}</p>
			{% endfor %}

			<p>&nbsp;</p>
		</div>
	</div>
</div>

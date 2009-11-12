{% if m.rsc[id].is_a.artist %}

<div class="item-wrapper" id="sort-user-credentials">
	<h3 class="above-item clearfix do_blockminifier">
		<span class="title">Events for this artist</span>
		<span class="arrow">make smaller</span>
	</h3>
	<div class="item clearfix">
		<div class="admin-form">
			<p>Add event for this artist. <a href="javascript:void(0)" class="do_dialog {title: 'Help about artist\'s events.', text: 'You can quickly add events for this artist.  When you want to delete an event, then click on the name of the event and delete it on the event\'s edit page.', width: '450px'}">Need more help?</a></p>
			{% button action={dialog_artist_event_add id=id} text="make a new event" %}
	
			<h3 class="clear">Existing events</h3>

			{% for event_id in m.search[{event_for_performer id=id pagelen=100}] %}
				<div class="rsc-edge">
					<span class="clearfix">
						<span class="unlink-item">
							<a href="{% url admin_edit_rsc id=event_id %}">
								{{ m.rsc[event_id].title|default:"untitled event" }}
								{% if m.rsc[event_id].date_start %}
									/ {{ m.rsc[event_id].date_start|date:"b j, H:i" }}
								{% endif %}
							</a>
						</span>
					</span>
				</div>
			{% empty %}
				<p>No performances yet.</p>
			{% endfor %}

			<p>&nbsp;</p>
		</div>
	</div>
</div>

{% endif %}

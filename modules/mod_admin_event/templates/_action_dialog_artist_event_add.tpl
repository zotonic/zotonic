
{% wire id=#form type="submit" postback={event_add id=id on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">
	<div class="new-user-wrapper">

		<p>Add a new event where “{{ m.rsc[id].title }}” will be performing.</p>
		
		<div class="form-item">
			<label for="{{ #title }}" style="color:white">Event title</label>
			<input type="text" id="{{ #title }}" name="title" value="" />
		</div>
		{% validate id=#title name="title" type={presence} %}
		

		<div class="form-item">
			<label for="{{ #venue_id }}">Venue</label>
			<select id="{{ #venue_id }}" name="venue">
			{% for title, id in m.search[{all_bytitle cat="venue"}] %}
				<option value="{{ id }}">{{ title }}</option>
			{% endfor %}
			</select>
		</div>

		<h3>Genre</h3>
		
		<div class="form-item">
		{% for title, id in m.search[{all_bytitle cat="genre"}] %}
			<input id="{{ #genre.id }}" type="checkbox" value="{{ id }}" name="genre" />
			<label for="{{ #genre.id}}">{{ title }}</label><br/>
		{% endfor %}
		</div>

		<button type="submit">Add event</button>
		{% button action={dialog_close} text="Cancel" %}
	</div>
</form>


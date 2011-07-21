{% extends "admin_edit_widget_std.tpl" %}

{# Show the edit fields to edit the name of a person #}


{% block widget_title %}{{ _"Person name"|escapejs }}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}


{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset class="admin-form">
	<div class="notification notice">
		{_ Here you can edit the person's name.  Use the title of the base content for the display name of this person. _}
		<a href="javascript:void(0)" class="do_dialog" data-dialog="title: '{{ _"Help about person name."|escapejs }}', text: '{{ _"<strong>First</strong> also known as given name, forename or Christen name.<br/><strong>Middle</strong> often shortened to an initial like in <em>John D. Rockefeller</em>.<br/><strong>Surname prefix</strong> like the Dutch <em>van, van de, der</em>.<br/><strong>Surname</strong> also known as family name or last name."|escapejs }}', width: '450px'">{_ Need more help? _}</a>
	</div>

	<div class="zp-20">
		<div class="form-item clearfix">
			<label for="name_first">{_ First _}</label>
			<input id="name_first" type="text" name="name_first" value="{{ r.name_first }}" style="width: 90%" />
		</div>
	</div>

	<div class="zp-10">
		<div class="form-item clearfix">
			<label for="name_middle">{_ Middle _}</label>
			<input id="name_middle" type="text" name="name_middle" value="{{ r.name_middle }}" style="width: 80%" />
		</div>
	</div>

	<div class="zp-10">
		<div class="form-item clearfix">
			<label for="name_surname_prefix">{_ Sur. prefix _}</label>
			<input id="name_surname_prefix" type="text" name="name_surname_prefix" value="{{ r.name_surname_prefix }}" style="width: 80%" />
		</div>
	</div>

	<div class="zp-40">
		<div class="form-item clearfix">
			<label for="name_surname">{_ Surname _}</label>
			<input id="name_surname" type="text" name="name_surname" value="{{ r.name_surname }}" style="width: 80%" />
		</div>
	</div>

</fieldset>


{% endwith %}
{% endblock %}

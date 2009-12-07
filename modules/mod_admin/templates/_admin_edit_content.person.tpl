{# Show the edit fields to edit the name of a person #}
{% with m.rsc[id] as r %}
<div class="item-wrapper">
	<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
		<span class="title">Person name</span>
		<span class="arrow">make smaller</span>
	</h3>
	
	<div class="item">
		<fieldset class="admin-form">
			<div class="notification notice">
				Here you can edit the person's name.  Use the title of the base content for the display name of this person.
				<a href="javascript:void(0)" class="do_dialog {title: 'Help about person name.', text: '<strong>First</strong> also known as given name, forename or Christen name.<br/><strong>Middle</strong> often shortened to an initial like in <em>John D. Rockefeller</em>.<br/><strong>Surname prefix</strong> like the Dutch <em>van, van de, der</em>.<br/><strong>Surname</strong> also known as family name or last name.', width: '450px'}">Need more help?</a>
			</div>
			
			<div class="zp-20">
				<div class="form-item clearfix">
					<label for="name_first">First</label>
					<input id="name_first" type="text" name="name_first" value="{{ r.name_first }}" style="width: 90%" />
				</div>
			</div>

			<div class="zp-10">
				<div class="form-item clearfix">
					<label for="name_middle">Middle</label>
					<input id="name_middle" type="text" name="name_middle" value="{{ r.name_middle }}" style="width: 80%" />
				</div>
			</div>

			<div class="zp-10">
				<div class="form-item clearfix">
					<label for="name_surname_prefix">Sur. prefix</label>
					<input id="name_surname_prefix" type="text" name="name_surname_prefix" value="{{ r.name_surname_prefix }}" style="width: 80%" />
				</div>
			</div>

			<div class="zp-40">
				<div class="form-item clearfix">
					<label for="name_surname">Surname</label>
					<input id="name_surname" type="text" name="name_surname" value="{{ r.name_surname }}" style="width: 80%" />
				</div>
			</div>
			
		</fieldset>
	</div>
</div>
{% endwith %}
{# A person also has an address #}
{% include "_admin_edit_content_address.tpl" id=id %}

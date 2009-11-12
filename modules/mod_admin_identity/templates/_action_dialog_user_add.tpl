
{% wire id=#form type="submit" postback={user_add on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">
	<div class="new-user-wrapper">

		<h3>Name and e-mail address</h3>
		<p>
			Give the name and the e-mail address of the new user. A <em>person</em> page will be created for this user.
		</p>

		<div class="form-item">
			<label for="{{ #name_first }}" style="color:white">First</label>
			<input type="text" id="{{ #name_first }}" name="name_first" value="" />
			{% validate id=#name_first name="name_first" type={presence} %}
		</div>

		<div class="form-item">
			<label for="{{ #name_surname_prefix }}" style="color:white">Sur. prefix</label>
			{# below we do not use name_surename_prefix because it confuses the autofill of browsers #}
			<input type="text" id="{{ #name_surname_prefix }}" name="surprefix" value="" style="width: 50px"/>
		</div>

		<div class="form-item">
			<label for="{{ #name_surname }}" style="color:white">Surname</label>
			<input type="text" id="{{ #name_surname }}" name="name_surname" value="" />
			{% validate id=#name_surname name="name_surname" type={presence} %}
		</div>

		<div class="form-item">
			<label for="{{ #email }}" style="color:white">E-mail</label>
			<input type="text" id="{{ #email }}" name="email" value="" />
			{% validate id=#email name="email" type={presence} type={email} %}
		</div>

		<hr />

		<h3>Username and password</h3>
		<p>
			Give an unique username and a password. Usernames and passwords are case sensitive, so be careful when entering them.
		</p>

		
		<div class="form-item">
			<label for="new_username" style="color:white">Username</label>
			<input type="text" id="new_username" name="new_username" value="" />
			{% validate id="new_username" type={presence} %}
		</div>

		<div class="form-item">
			<label for="new_password" style="color:white">Password</label>
			<input type="text" id="new_password" name="new_password" value="" />
			{% validate id="new_password" type={presence} %}
		</div>

		<hr />

		{% with m.search[{all_bytitle cat="group"}] as all_groups %}

			<h3>Group for person page</h3>
			<p>This defines who will be able to edit the person page of this user.</p>

			<div class="form-item">
				<label for="{{ #group_id }}">Group</label>
				<select id="{{ #group_id }}" name="group_id">
				{% for title, group_id in all_groups %}
					<option value="{{ group_id }}">{{ title }}</option>
				{% endfor %}
				</select>
			</div>

			<hr />

			<h3>Member of</h3>
			<p>This defines the access rights the user will obtain. When you want to add the new user to more groups then click on the <em>groups</em> menu item.</p>

			<div class="form-item">
				<label for="{{ #member_id }}">Group</label>
				<select id="{{ #member_id }}" name="member_id">
				{% for title, member_id in all_groups %}
					<option value="{{ member_id }}">{{ title }}</option>
				{% endfor %}
				</select>
			</div>

		{% endwith %}

		<hr/>

		<button type="submit">Add user</button>
		{% button action={dialog_close} text="Cancel" %}
	</div>
</form>


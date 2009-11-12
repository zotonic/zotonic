
<p>Select the role of “{{ m.rsc[member_id].title }}”.</p>

{% wire id=#form type="submit" postback="group_member_edit" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<input type="hidden" name="id" value="{{ id }}" />
	<input type="hidden" name="member_id" value="{{ member_id }}" />

	<div class="new-predicate-wrapper">

		<div class="form-item clearfix">
			<input id="{{ #leader }}" type="radio" class="xdo_fieldreplace" name="member" value="leader"  />
			<label for="{{ #leader }}">Leader</label>
		</div>

		<div class="form-item clearfix">
			<input id="{{ #observer }}" type="radio" class="xdo_fieldreplace" name="member" value="observer"  />
			<label for="{{ #observer }}">Observer</label>
		</div>

		<div class="form-item clearfix">
			<input id="{{ #member }}" type="radio" class="xdo_fieldreplace" name="member" value="member" />
			<label for="{{ #member }}">Member</label>
		</div>

		<div class="form-item clearfix">
			{% button text="Save" %}
			{% button text="Cancel" action={dialog_close} %}
		</div>		
	</div>
</form>


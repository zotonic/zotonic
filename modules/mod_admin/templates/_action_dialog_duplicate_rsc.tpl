<p>
	You are going to duplicate the page “{{ m.rsc[id].title }}”<br/>
	Please fill in the title of the new page.
</p>

{% wire id=#form type="submit" postback={duplicate_page id=id} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<div class="new-rsc-wrapper">
		<div class="form-item clearfix">
			<label for="new_rsc_title" style="color:white">Page title</label>
			<input type="text" id="new_rsc_title" name="new_rsc_title" value="{{ m.rsc[id].title }}" />
			{% validate id="new_rsc_title" type={presence} %}
		</div>

		<div class="form-item clearfix">
			<input type="checkbox" id="{{ #published }}" name="is_published" value="1" />
			<label for="{{ #published }}" class="left">Published</label>
		</div>
		
		<button type="submit">Duplicate page</button>
		
		{% button action={dialog_close} text="Cancel" %}
	</div>
</form>


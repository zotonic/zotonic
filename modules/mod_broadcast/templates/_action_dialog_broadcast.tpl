<p>This will send a broadcast to everybody on the site.</p>

{% wire id=#form type="submit" postback="broadcast" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<div class="new-rsc-wrapper">
		<div class="form-item clearfix">
			<label for="{{ #title }}">Title</label>
			<input type="text" id="{{ #title }}" name="title" value="Alert" />
			{% validate id=#title name="title" type={presence} %}
		</div>

		<div class="form-item clearfix">
			<label for="{{ #type }}">Type</label>
			<select id="{{ #type }}" name="type">
                <option value="notice" selected="selected">Notice</option>
                <option value="error">Error / warning</option>
            </select>
			{% validate id=#type name="type" type={presence} %}
        </div>

		<div class="form-item clearfix">
			<label for="{{ #message }}">Message</label>
			<textarea id="{{ #message }}" name="message"></textarea>
			{% validate id=#message name="message" type={presence} %}
		</div>

		<div class="form-item clearfix">
			<button type="submit">Send</button>
			{% button action={dialog_close} text="Cancel" %}
		</div>
	</div>
</form>


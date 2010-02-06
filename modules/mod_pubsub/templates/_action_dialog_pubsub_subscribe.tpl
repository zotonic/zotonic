
<p>Enter the URL of the page you wish to subscribe to.</p>

{% wire id=#form type="submit" postback="subscribe_new"  %}
<form id="{{ #form }}" method="POST" action="postback">

	<div class="new-predicate-wrapper">
		<p>
			<label for="subscribe_url" style="color:white">URL</label>
			<input type="text" id="subscribe_url" name="subscribe_url" value="" />
			{# {% validate id="subscribe_url" type={presence} %} #}
		</p>
		
		<button type="submit">Subscribe</button>
		
		{% button action={dialog_close} text="Cancel" %}
	</div>
</form>


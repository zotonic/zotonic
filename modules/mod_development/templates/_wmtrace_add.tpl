
<p>Please specify the new trace rule.</p>

{% wire id=#form type="submit" postback={add} delegate="resource_wmtrace_conf" %}
<form id="{{ #form }}" method="POST" action="postback">

	<div>

		<div class="form-item clearfix">
			<label for="{{ #resource }}">Resource</label>
			<select id="{{ #resource }}" name="resource">
			{% for res in resources %}
				<option value="{{res}}">
					{{ indent }}{{ res }}
				</option>
			{% endfor %}
			</select>
		</div>

		<div class="form-item clearfix">
			<button type="submit">Add trace rule</button>
			{% button text="Cancel" action={dialog_close} tag="a" %}
		</div>

	</div>
</form>


{% if new %}
	<h3>{_ Add a new language _}</h3>
{% else %}
	<h3>{_ Edit _} {{ lang.language }} ({{ code }})</h3>
{% endif %}

<p>{_ Enter the iso code (for example <em>en</em> or <em>nl</em>), and the title of the language. Use the native form of the language, for example <em>English</em>, <em>Türkçe</em> or <em>Français</em>. _}</p>

{% wire id=#form type="submit" postback={language_edit code=code} delegate="mod_translation" %}
<form id="{{ #form }}" method="POST" action="postback">
	<div class="form-item clearfix">
		<label for="{{ #code }}">{_ ISO Code _}</label>
		<input type="text" style="width: 50px" id="{{ #code }}" name="code" value="{{ code }}" />
		{% validate id=#code name="code" type={presence} %}
	</div>

	<div class="form-item clearfix">
		<label for="{{ #language }}">{_ Language _}</label>
		<input type="text" id="{{ #language }}" name="language" value="{{ lang.language }}" />
		{% validate id=#language name="language" type={presence} %}
	</div>
	
	<div class="form-item clearfix">
		<label for="{{ #enabled }}">{_ Show in menu _}</label>
		<input type="checkbox" id="{{ #enabled }}" name="is_enabled" value="1"
			{% if new or lang.is_enabled %}checked="checked"{% endif %} />
	</div>

	<button type="submit">{_ Save _}</button>
	{% button action={dialog_close} text=_"Cancel" %}
</form>


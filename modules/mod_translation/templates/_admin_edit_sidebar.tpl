{% with m.rsc[id].language as r_lang %}
<div class="item-wrapper">
	<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
		<span class="title">{_ Translations _}</span>
		<span class="arrow">{_ make smaller _}</span>
	</h3>
	<div class="item clearfix admin-form">
		<div class="notification notice">
			{_ Translate this page in other languages. _} <a href="javascript:void(0)" class="do_dialog {title: '{_ Help about translations. _}', text: '{_ The title, body and other texts can be translated in different languages. Here you can select which languages will be used. _}', width: '450px'}">{_ Need more help? _}</a>
		</div>

		<ul id="rsc_languages">
		{% for code, lang in languages %}
			{% if lang.is_enabled %}
			<li>
				<label>
					<input type="checkbox" id="{{ #language.code }}" name="language" value="{{ code }}"
						{% if code|member:r_lang or (not r_lang and z_language == code) %}checked="checked"{% endif %}> {{ lang.language }}
				</label>
				{% wire id=#language.code 
					action={toggle selector=[".tab-",code|make_list]}
				%}
			</li>
			{% endif %}
		{% empty %}
			<li><label><input type="checkbox" checked="checked" disabled="disabled"> {{ z_language }}</label></li>
		{% endfor %}
		</ul>
		
		<div class="clear">&nbsp;</div>
	</div>
</div>
{% endwith %}

{% extends "admin_edit_widget_std.tpl" %}

{# Sidebar widget for editing rsc transalations #}

{% block widget_title %}{_ Translations _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}


{% block widget_content %}
{% with m.rsc[id].language as r_lang %}
<div class="notification notice">
	{_ Translate this page in other languages. _}
	<a href="javascript:void(0)" class="do_dialog" data-dialog="title: '{_ Help about translations. _}', text: '{_ The title, body and other texts can be translated in different languages. Here you can select which languages will be used. _}', width: '450px'">{_ Need more help? _}</a>
</div>

<ul id="rsc_languages">
	{% for code, lang in languages %}
		{% if lang.is_enabled %}
			<li>
				<label>
					<input type="checkbox" id="{{ #language.code }}" name="language" value="{{ code }}"
						{% if code|member:r_lang or (not r_lang and z_language == code) %}checked="checked"{% endif %}> 
						<span {% include "_language_attrs.tpl" language=code %}>{{ lang.language }}</span>
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
{% endwith %}
{% endblock %}

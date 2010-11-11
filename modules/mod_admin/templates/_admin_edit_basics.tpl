{% with m.rsc[id] as r %}
<div class="item-wrapper">
	{% with r.language|default:[z_language] as r_language %}
		<div class="translations ui-tabs" id="{{ #tabs }}">
			{% include "_admin_translation_tabs.tpl" prefix=#prefix r_language=r_language %}
			
			{% for code,lang in m.config.i18n.language_list.list %}
				<div id="{{ #prefix }}-{{ code }}" class="language-{{ code }} item ui-tabs-hide">
					<fieldset class="admin-form">
						<div class="form-item clearfix">
							<label for="field-title${{ code }}">{_ Title _} ({{ code }})</label>
							<input type="text" id="field-title${{ code }}" name="title${{ code }}" value="{{ r.translation[code].title }}" {% if not is_editable %}disabled="disabled"{% endif %}/>
						</div>

						<div class="form-item clearfix">
							<label for="field-summary${{ code }}">{_ Summary _} ({{ code }})</label>
							<textarea rows="2" cols="10" id="field-summary${{ code }}" name="summary${{ code }}" class="intro" {% if not is_editable %}disabled="disabled"{% endif %}>{{ r.translation[code].summary }}</textarea>
						</div>

						<div class="form-item clearfix">
							<label for="field-short-title${{ code }}">{_ Short title _} ({{ code }})</label>
							<input type="text" style="width: 50%" id="field-short-title${{ code }}" name="short_title${{ code }}" value="{{ r.translation[code].short_title }}" {% if not is_editable %}disabled="disabled"{% endif %} />
						</div>
					</fieldset>
				</div>
			{% endfor %}
		</div>
	{% endwith %}
</div>
{% endwith %}

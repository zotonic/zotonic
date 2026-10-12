{% with #translation as target %}
<div id="{{ target }}">
    {% if id.is_editable and not p|match:"_(unsafe|id|url|uri|json)$" and (not is_block or block_name) and (p != "title_slug" or id.custom_slug) %}
        <div id="{{ #edit }}" role="button" tabindex="0" class="translation-edit-text"
             title="{_ Edit translation _}" aria-label="{_ Edit translation _}: {{ p|escape }} ({{ code|escape }})">
            {% if text|translation:code %}
                {{ text|translation:code }}
            {% else %}
                <span class="text-muted">{_ (empty) _}</span>
            {% endif %}
        </div>
        {% wire id=#edit postback={translation_edit id=id property=p block=block_name language=code target=target} delegate=`mod_translation` %}
    {% else %}
        {{ text|translation:code }}
    {% endif %}
</div>
{% endwith %}

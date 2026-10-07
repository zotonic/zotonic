{% if ids %}
    <hr>
    <div style="display: flex; align-items: center; gap: 15px;">
        {% button class="btn btn-default"
            text=_"Manage translations"
            postback={bulk_dialog ids=ids}
            delegate=`m_translation_tree`
        %}
        <span class="text-muted">
            {_ Add, copy, or remove translations for the selected pages. _}
        </span>
    </div>
{% endif %}

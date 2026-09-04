{% with m.rsc[id].id as survey_id %}
    {% if survey_id and m.survey.is_allowed_results_download[survey_id] and m.modules.active.mod_export %}
        <button id="{{ #download }}"
                type="button"
                class="btn {% if is_small %}btn-xs{% else %}btn-default{% endif %}"
                title="{_ Download survey results _}">
            {_ Download... _}
        </button>
        {% wire id=#download
                action={dialog_open
                    title=_"Download survey results"
                    template="_dialog_survey_results_download.tpl"
                    id=survey_id}
        %}
    {% endif %}
{% endwith %}

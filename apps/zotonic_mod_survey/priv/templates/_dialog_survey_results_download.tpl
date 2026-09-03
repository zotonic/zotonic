{% with m.rsc[id].id as survey_id %}
    {% if survey_id and m.survey.is_allowed_results_download[survey_id] and m.modules.active.mod_export %}
        <form id="{{ #download_form }}" action="#" method="get">
            <div class="form-group">
                <label class="checkbox">
                    <input type="checkbox" name="anonymous" value="1"
                           {% if survey_id.survey_anonymous %}checked{% endif %}>
                    {_ Anonymous _}
                </label>
                <p class="help-block">
                    {_ Remove respondent names, user ids, and other respondent identity columns. _}
                </p>
            </div>

            {% if survey_id.is_editable %}
                <div class="form-group">
                    <label class="checkbox">
                        <input type="checkbox" name="status" value="1">
                        {_ Status _}
                    </label>
                    <p class="help-block">
                        {_ Include the status, status date, and note. The status modifier is omitted for anonymous downloads. _}
                    </p>
                </div>
            {% endif %}

            <div class="form-group">
                <label class="checkbox">
                    <input type="checkbox" name="prompts" value="1" checked>
                    {_ Include prompts as the first row _}
                </label>
            </div>

            <div class="form-group">
                <label for="{{ #download_type }}">{_ Type _}</label>
                <select id="{{ #download_type }}" name="type" class="form-control">
                    <option value="{% url survey_results_download type='xlsx' id=survey_id %}">{_ Excel (XLSX) _}</option>
                    <option value="{% url survey_results_download type='csv' id=survey_id %}">{_ CSV _}</option>
                    <option value="{% url survey_results_download type='json' id=survey_id %}">{_ JSON _}</option>
                </select>
            </div>

            <div class="modal-footer">
                {% button tag="a" class="btn btn-default" text=_"Cancel" action={dialog_close} %}
                <button type="submit" class="btn btn-primary">{_ Download _}</button>
            </div>
        </form>

        {% javascript %}
            $("#{{ #download_form }}").on("submit", function(event) {
                event.preventDefault();

                const form = event.currentTarget;
                const url = new URL(form.elements.type.value, window.location.href);

                if (form.elements.anonymous.checked) {
                    url.searchParams.set("anonymous", "1");
                }
                if (form.elements.status && form.elements.status.checked) {
                    url.searchParams.set("status", "1");
                }
                url.searchParams.set("prompts", form.elements.prompts.checked ? "1" : "0");

                window.open(url.toString(), "_blank", "noopener");
                z_dialog_close();
                z_growl_add("{_ The download has started. Please check your download window. _}");
            });
        {% endjavascript %}
    {% else %}
        <p class="alert alert-danger">{_ You are not allowed to download the survey results. _}</p>
    {% endif %}
{% endwith %}

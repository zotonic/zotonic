{% if m.acl.is_admin %}
{% with m.analytics.configuration as a %}
{% wire id=#create type="submit" postback="create_recording" delegate="mod_analytics" %}
<form class="analytics-recording-wizard" id="{{ #create }}" method="post" action="postback">
    <p class="text-muted" data-step-label aria-live="polite">{_ Step 1 of 2: Choose which activity to measure _}</p>
    <fieldset data-step="1">
        <div class="form-group"><label for="{{ #name }}">{_ Name _}</label><input id="{{ #name }}" class="form-control" name="name" required maxlength="255" autofocus /></div>
        <div class="form-group"><label for="{{ #description }}">{_ Short description _}</label><textarea id="{{ #description }}" class="form-control" name="description" rows="2" maxlength="250" placeholder="{_ Describe the activity this analytics measurement tracks _}"></textarea><p class="help-block">{_ Optional. Shown with the analytics measurement on the dashboard. _}</p></div>
        <div class="form-group"><label for="{{ #event }}">{_ Analytics event type _}</label><select id="{{ #event }}" class="form-control" name="event">{% include "_admin_analytics_event_options.tpl" events=a.registry %}</select></div>
        <p>{_ Next, choose a breakdown and filters supported by this event type. _}</p>
    </fieldset>
    <fieldset data-step="2" hidden disabled>
        <p><strong data-event-label></strong></p>
        <div class="form-group"><label for="{{ #dimension }}">{_ Break down by _}</label><select id="{{ #dimension }}" class="form-control" name="dimension"><option value="">{_ No breakdown (total) _}</option></select></div>
        <div class="form-group" data-groups><label for="{{ #groups }}">{_ Group limit _}</label><input id="{{ #groups }}" class="form-control" name="groups" type="number" min="1" max="100" value="32" /><p class="help-block">{_ Each group reserves 3,578 time slots. Page breakdowns use candidates with approximate counts; other breakdowns reserve a group for excess values. _}</p></div>
        {% for n in [1,2] %}
        <div data-filter>
            <div class="form-group"><label for="{{ #filter }}-{{ n }}">{_ Fixed filter _} {{ n }}</label><select id="{{ #filter }}-{{ n }}" class="form-control" name="filter{{ n }}"><option value="">{_ No filter — include all values _}</option></select></div>
            <div class="form-group"><label for="{{ #value }}-{{ n }}">{_ Allowed values (comma separated) _}</label><input id="{{ #value }}-{{ n }}" class="form-control" name="value{{ n }}" maxlength="1000" autocomplete="off" disabled /><p class="help-block" data-values="{{ n }}" aria-live="polite"></p></div>
        </div>
        {% endfor %}
        <p data-request-types hidden>{_ Request types found in this site's dispatch rules: _} {% for t in a.request_types %}<code>{{ t|escape }}</code> {% endfor %}</p>
        <p>{_ Events must satisfy both filters to be counted. New analytics measurements start without historical data. _}</p>
    </fieldset>
    <div class="modal-footer">
        {% button text=_"Cancel" class="btn btn-default" action={dialog_close} %}
        <button type="button" class="btn btn-default" data-back hidden>{_ Back _}</button>
        <button type="button" class="btn btn-primary" data-next>{_ Next _}</button>
        <button type="submit" class="btn btn-primary" data-start hidden>{_ Start analytics measurement _}</button>
    </div>
    {% for e in a.registry %}
        <template data-event="{{ e.name|escape }}">{% include "_admin_analytics_dimension_options.tpl" dimensions=e.dimension_keys choices=e.choices %}</template>
    {% endfor %}
    <span hidden data-step-two>{_ Step 2 of 2: Breakdown and filters _}</span>
    <span hidden data-values-label>{_ Allowed values: _}</span>
    <span hidden data-free-values>{_ Enter one or more values, separated by commas. _}</span>
</form>
{% javascript %}
(() => {
    const form = document.getElementById("{{ #create }}");
    const event = form.elements.event;
    const dimension = form.elements.dimension;
    const first = form.querySelector('[data-step="1"]');
    const second = form.querySelector('[data-step="2"]');
    const heading = form.querySelector('[data-step-label]');
    const firstHeading = heading.textContent;
    const filters = [1, 2].map(n => ({
        select: form.elements['filter' + n],
        input: form.elements['value' + n],
        hint: form.querySelector('[data-values="' + n + '"]')
    }));
    const updateFilter = row => {
        const {select, input} = row;
        input.disabled = !select.value;
        input.required = !!select.value;
        const values = select.selectedOptions[0].dataset.values;
        row.hint.textContent = !select.value ? '' : values
            ? form.querySelector('[data-values-label]').textContent + ' ' + values
            : form.querySelector('[data-free-values]').textContent;
    };
    const updateGroups = () => {
        form.querySelector('[data-groups]').hidden = !dimension.value;
        form.elements.groups.disabled = !dimension.value;
    };
    const updateType = () => {
        const source = Array.from(form.querySelectorAll('template[data-event]'))
            .find(t => t.dataset.event === event.value);
        [dimension, ...filters.map(row => row.select)].forEach(select => {
            while (select.options.length > 1) select.remove(1);
            if (source) select.append(source.content.cloneNode(true));
            select.value = '';
        });
        filters.forEach(row => { row.input.value = ''; updateFilter(row); });
        form.querySelector('[data-event-label]').textContent = event.selectedOptions[0].textContent;
        form.querySelector('[data-request-types]').hidden = event.value !== 'request';
        updateGroups();
    };
    const showStep = next => {
        if (next && !form.reportValidity()) return;
        first.hidden = next;
        second.hidden = !next;
        second.disabled = !next;
        form.querySelector('[data-next]').hidden = next;
        form.querySelector('[data-back]').hidden = !next;
        form.querySelector('[data-start]').hidden = !next;
        heading.textContent = next ? form.querySelector('[data-step-two]').textContent : firstHeading;
        (next ? dimension : form.elements.name).focus();
    };
    event.addEventListener('change', updateType);
    dimension.addEventListener('change', updateGroups);
    filters.forEach(row => row.select.addEventListener('change', () => {
        row.input.value = '';
        updateFilter(row);
    }));
    form.querySelector('[data-next]').addEventListener('click', () => showStep(true));
    form.querySelector('[data-back]').addEventListener('click', () => showStep(false));
    form.addEventListener('submit', e => {
        if (!first.hidden) { e.preventDefault(); e.stopImmediatePropagation(); showStep(true); }
    }, true);
    updateType();
})();
{% endjavascript %}
{% endwith %}
{% endif %}

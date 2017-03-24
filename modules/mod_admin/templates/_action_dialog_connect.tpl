{#
params:
- callback (optional)
- language (optional)
- actions (optional)
- stay (optional)
- subject_id or object_id
- tabs_enabled (optional): list of tab names: ["new", "depiction", "find",  "upload", "url"]
- tab (optional)
- autoclose (optional - defaults to false)
- nocatselect (optional - defaults to false)

find params:
- predicate (optional) (atom)
- delegate (optional) (atom)
- category (optional) (string/id) preselect the category dropdown
#}
{% with
    callback|default:q.callback,
    language|default:q.language|default:z_language,
    actions|default:[],
    tab|default:q.tab|default:(tabs_enabled|first)|default:"find",
    m.rsc[q.category|default:category].id|default:(m.predicate.object_category[predicate]|first|element:1)
    as
    callback,
    language,
    actions,
    tab,
    cat
%}
{% with stay or callback or subject_id as stay %}
{% if not (tabs_enabled and tabs_enabled|length == 1) %}
    <ul class="nav nav-pills">
        {% block tabs %}
            {% if in_sorter == "category" %}
                {% if "new"|member:tabs_enabled %}
                    <li class="active">
                        <a data-toggle="tab" href="#{{ #tab }}-new">{_ Create Page _}</a>
                    </li>
                {% endif %}
            {% else %}
                {% if not tabs_enabled or "depiction"|member:tabs_enabled %}
                    {% if q.is_zmedia %}
                        <li {% if tab == "depiction" %}class="active"{% endif %}>
                            <a data-toggle="tab" href="#{{ #tab }}-depiction">{_ Attached media _}</a>
                        </li>
                    {% endif %}
                {% endif %}
                {% if not tabs_enabled or "find"|member:tabs_enabled %}
                    <li {% if tab == "find" %}class="active"{% endif %}>
                        <a data-toggle="tab" href="#{{ #tab }}-find">{_ Find Page _}</a>
                    </li>
                {% endif %}
                {% if not tabs_enabled or "new"|member:tabs_enabled %}
                    {% if predicate.name /= "depiction" %}
                    <li {% if tab == "new" %}class="active"{% endif %}>
                        <a data-toggle="tab" href="#{{ #tab }}-new">{_ Create Page _}</a>
                    </li>
                    {% endif %}
                {% endif %}
                {% if not tabs_enabled or "upload"|member:tabs_enabled %}
                    <li {% if tab == "upload" %}class="active"{% endif %}>
                        <a data-toggle="tab" href="#{{ #tab }}-upload">{_ Upload File _}</a>
                    </li>
                {% endif %}
                {% if not tabs_enabled or "url"|member:tabs_enabled %}
                    <li {% if tab == "url" %}class="active"{% endif %}>
                        <a data-toggle="tab" href="#{{ #tab }}-url">{_ Website or Embed _}</a>
                    </li>
                {% endif %}
                {% all include "_media_upload_tab.tpl" tab=#tab %}
            {% endif %}
            {% javascript %}
                $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
                    var tabRef = e.target.getAttribute('href');
                    if (!tabRef) return;
                    var tabId = tabRef.substr(1);
                    var tabEl = document.getElementById(tabId);
                    if (!tabEl) return;
                    var focusEl = tabEl.querySelector('.do_autofocus');
                    if (!focusEl) return;
                    focusEl.focus();
                });
            {% endjavascript %}
        {% endblock %}
    </ul>
{% endif %}
<div class="tab-content" id="dialog-connect-panels">
    {% block tabs_content %}
        {% if in_sorter == "category" %}
            {# only one tab, so no conditional #}
                {% include "_action_dialog_connect_tab_new.tpl"
                    tab=#tab
                    predicate=predicate
                    delegate=delegate
                    subject_id=subject_id
                    object_id=object_id
                    is_active
                    title=""
                    cat=cat
                    nocatselect
                    autoclose
                %}
        {% else %}
            {% if not tabs_enabled or "depiction"|member:tabs_enabled %}
                {% if q.is_zmedia %}
                    {% include "_action_dialog_connect_tab_depictions.tpl"
                        tab=#tab
                        predicate=predicate
                        delegate=delegate
                        subject_id=subject_id
                        object_id=object_id
                        is_active=(tab == "depiction")
                        title=""
                    %}
                {% endif %}
            {% endif %}
            {% if not tabs_enabled or "find"|member:tabs_enabled %}
                {% include "_action_dialog_connect_tab_find.tpl"
                    tab=#tab
                    predicate=predicate
                    delegate=delegate
                    subject_id=subject_id
                    object_id=object_id
                    is_active=(tab == "find")
                    title=""
                    cat=cat
                %}
            {% endif %}
            {% if not tabs_enabled or "new"|member:tabs_enabled %}
                {% if predicate.name /= "depiction" %}
                    {% include "_action_dialog_connect_tab_new.tpl"
                        tab=#tab
                        predicate=predicate
                        delegate=delegate
                        subject_id=subject_id
                        object_id=object_id
                        title=""
                        is_active=(tab == "new")
                        cat=cat
                    %}
                {% endif %}
            {% endif %}
            {% with "action_admin_dialog_media_upload" as delegate %}
                {% if not tabs_enabled or "upload"|member:tabs_enabled %}
                    {% include "_action_dialog_media_upload_tab_upload.tpl"
                        tab=#tab
                        predicate=predicate
                        delegate=delegate
                        subject_id=subject_id
                        object_id=object_id
                        title=""
                        is_active=(tab == "upload")
                    %}
                {% endif %}
                {% if not tabs_enabled or "url"|member:tabs_enabled %}
                    {% include "_action_dialog_media_upload_tab_url.tpl"
                        tab=#tab
                        predicate=predicate
                        subject_id=subject_id
                        object_id=object_id
                        is_active=(tab == "url")
                        title=""
                    %}
                {% endif %}
                {% all include "_media_upload_panel.tpl"
                    tab=#tab
                    predicate=predicate
                    delegate=delegate
                    subject_id=subject_id
                    object_id=object_id
                    title=""
                    delegate=delegate
                %}
            {% endwith %}
        {% endif %}
    {% endblock %}
</div>
{% endwith %}
{% endwith %}

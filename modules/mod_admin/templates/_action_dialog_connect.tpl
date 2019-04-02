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
- is_zlink (optional) set by the tinyMCE 'zlink' plugin
- is_zmedia (optional) set by the tinyMCE 'zmedia' plugin

find params:
- predicate (optional) (atom)
- delegate (optional) (atom)
- category (optional) (string/id) preselect the category dropdown
- content_group (optional) can also be the string "me" to search on user created content
#}
{% with
    callback|default:q.callback|default:"window.zAdminConnectDone",
    language|default:(q.language|escape)|default:z_language,
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
                {% if not tabs_enabled or "new"|member:tabs_enabled %}
                    <li class="active">
                        <a data-toggle="tab" href="#{{ #tab }}-findnew">
                            {% if predicate and (subject_id or object_id) %}
                                {_ Page _}
                            {% else %}
                                {_ Create Page _}
                            {% endif %}
                        </a>
                    </li>
                {% elseif tabs_enabled and "find"|member:tabs_enabled %}
                    <li {% if tab == "find" %}class="active"{% endif %}>
                        <a data-toggle="tab" href="#{{ #tab }}-find">{_ Find Page _}</a>
                    </li>
                {% endif %}
                {% if not tabs_enabled or "url"|member:tabs_enabled %}
                    <li {% if tab == "url" %}class="active"{% endif %}>
                        <a data-toggle="tab" href="#{{ #tab }}-url">{_ Upload by URL _}</a>
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
            {% if not tabs_enabled or "new"|member:tabs_enabled %}
                {% include "_action_dialog_connect_tab_findnew.tpl"
                    tab=#tab
                    predicate=predicate
                    delegate=delegate
                    subject_id=subject_id
                    object_id=object_id
                    is_active=`true`
                    title=""
                    cat=cat
                    content_group=content_group
                %}
            {% elseif tabs_enabled and "find"|member:tabs_enabled %}
                {% include "_action_dialog_connect_tab_find.tpl"
                    tab=#tab
                    predicate=predicate
                    delegate=delegate
                    subject_id=subject_id
                    object_id=object_id
                    is_active=`true`
                    title=""
                    cat=cat
                    content_group=content_group
                %}
            {% endif %}

            {% with "action_admin_dialog_media_upload" as delegate %}
                {% if not tabs_enabled or "url"|member:tabs_enabled %}
                    {% include "_action_dialog_media_upload_tab_url.tpl"
                        tab=#tab
                        predicate=predicate
                        delegate=delegate
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

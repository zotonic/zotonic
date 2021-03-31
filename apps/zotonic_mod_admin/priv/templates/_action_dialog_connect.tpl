{#
params:
- intent: the purpose of this dialog, one of: "select", "create", "connect" (and "update" for media dialog)
- callback (optional)
- language (optional)
- actions (optional)
- stay (optional)
- subject_id or object_id
- tabs_enabled (optional): list of tab names: ["new", "depiction", "find",  "upload", "url"]
- tabs_disabled (optional): list of tab names that should be hidden
- tab (optional)
- autoclose (optional - defaults to false)
- dependent (optional - defaults to false)
- nocatselect (optional - defaults to false)
- is_zlink (optional) set by the tinyMCE 'zlink' plugin
- is_zmedia (optional) set by the tinyMCE 'zmedia' plugin

find params:
- predicate (optional) (atom)
- delegate (optional) (atom)
- category (optional) (string/id) preselect the category dropdown
- content_group (optional) can also be the string "me" to search on user created content
#}

{% block dialog %}

{% if not intent|member:[ 'select', 'create', 'connect' ] %}
    <p class="alert alert-danger">
        Please specify the <b>intent</b> argument when using the <b>_action_dialog_connect.tpl</b>.<br>
        It should be one of: <b>select</b>, <b>create</b> or <b>connect</b>
    </p>
{% else %}

{% with
    callback|default:q.callback,
    language|default:(q.language|escape)|default:z_language,
    actions|default:[],
    tab|default:q.tab|default:(tabs_enabled|first)|default:"find",
    m.rsc[q.category|default:category].id|default:(m.predicate.object_category[predicate]|first|element:1),
    dependent|if_undefined:m.admin.rsc_dialog_is_dependent

    as

    callback,
    language,
    actions,
    tab,
    cat,
    dependent
%}

{% with stay or callback or subject_id as stay %}
{% with (not tabs_enabled or "depiction"|member:tabs_enabled) and is_zmedia as has_depiction_tab %}
{% with (tab == "depiction" and not m.rsc[subject_id].o.depiction)|if:"upload":tab as tab %}
{% with tabs_disabled|default:[] as tabs_disabled %}
{% if not (tabs_enabled and tabs_enabled|length == 1) %}
    <ul class="nav nav-pills">
        {% block tabs %}
            {% if in_sorter == "category" %}
                <li class="active">
                    <a data-toggle="tab" href="#{{ #tab }}-new">{_ Create _}</a>
                </li>
            {% else %}
                {% if has_depiction_tab %}
                    <li {% if tab == "depiction" %}class="active"{% endif %}>
                        <a data-toggle="tab" href="#{{ #tab }}-depiction">{_ Attached media _}</a>
                    </li>
                {% endif %}
                {% if (not tabs_enabled or "find"|member:tabs_enabled) and not "find"|member:tabs_disabled %}
                    <li {% if tab == "find" %}class="active"{% endif %}>
                        <a data-toggle="tab" href="#{{ #tab }}-find">
                            {% if predicate == 'depiction' %}{_ Find Media _}
                            {% else %}{_ Find Page _}
                            {% endif %}
                        </a>
                    </li>
                {% endif %}
                {% if (not tabs_enabled or "new"|member:tabs_enabled) and not "new"|member:tabs_disabled %}
                    <li {% if not tab or tab == "new" %}class="active"{% endif %}>
                        <a data-toggle="tab" href="#{{ #tab }}-findnew">{_ New Page _}</a>
                    </li>
                {% endif %}
                {% if (not tabs_enabled or "upload"|member:tabs_enabled) and not "upload"|member:tabs_disabled %}
                    <li {% if tab == "upload" %}class="active"{% endif %}>
                        <a data-toggle="tab" href="#{{ #tab }}-upload">{_ Upload _}</a>
                    </li>
                {% endif %}
                {% if (not tabs_enabled or "url"|member:tabs_enabled) and not "url"|member:tabs_disabled %}
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
            {% if has_depiction_tab %}
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
            {% if (not tabs_enabled or "find"|member:tabs_enabled) and not "find"|member:tabs_disabled %}
                {% include "_action_dialog_connect_tab_find.tpl"
                    tab=#tab
                    predicate=predicate
                    delegate=delegate
                    subject_id=subject_id
                    object_id=object_id
                    is_active=(tab == "find")
                    title=""
                    cat=cat
                    content_group=content_group
                %}
            {% endif %}
            {% if (not tabs_enabled or "new"|member:tabs_enabled) and not "new"|member:tabs_disabled %}
                {% include "_action_dialog_connect_tab_findnew.tpl"
                    tab=#tab
                    predicate=predicate
                    delegate=delegate
                    subject_id=subject_id
                    object_id=object_id
                    is_active=(not tab or tab == "new")
                    title=""
                    cat=cat
                    content_group=content_group
                %}
            {% endif %}
            {% if (not tabs_enabled or "upload"|member:tabs_enabled) and not "upload"|member:tabs_disabled %}
                {% include "_action_dialog_media_upload_tab_upload.tpl"
                    tab=#tab
                    predicate=predicate
                    delegate=delegate
                    subject_id=subject_id
                    object_id=object_id
                    is_active=(tab == "upload")
                    title=""
                    cat=cat
                    content_group=content_group
                %}
            {% endif %}
            {% with "action_admin_dialog_media_upload" as delegate %}
                {% if (not tabs_enabled or "url"|member:tabs_enabled) and not "url"|member:tabs_disabled %}
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
{% endwith %}
{% endwith %}
{% endwith %}

{% endif %}

{% endblock %}

{% include "_admin_edit_floating_buttons.tpl" %}

{% include "_admin_edit_content_publish.tpl" noheader %}

{% include "_admin_edit_content_note.tpl" %}

{% if id.is_a.meta %}
    {% include "_admin_edit_meta_features.tpl" %}
{% endif %}

{% if not id.is_a.meta %}
    {% include "_admin_edit_content_date_range.tpl" show_header %}
{% endif %}

<ul class="nav nav-tabs z-nav-tabs" id="myTab" role="tablist">
    <li class="nav-item" role="presentation">
        <button class="nav-link active" id="connections-tab" data-bs-toggle="tab" data-bs-target="#connections-tab-pane" type="button" role="tab" aria-controls="connections-tab-pane" aria-selected="true">{_ Connected to _}</button>
    </li>

    <li class="nav-item" role="presentation">
        <button class="nav-link" id="referrers-tab" data-bs-toggle="tab" data-bs-target="#referrers-tab-pane" type="button" role="tab" aria-controls="referrers-tab-pane" aria-selected="false">{_ Connected from _}</button>
    </li>

    <li class="nav-item" role="presentation">
        <button class="nav-link" id="settings-tab" data-bs-toggle="tab" data-bs-target="#settings-tab-pane" type="button" role="tab" aria-controls="settings-tab-pane" aria-selected="false"><i class="fa-solid fa-cog"></i> {_ Settings &amp; more _}</button>
    </li>
</ul>
<div class="tab-content">
    <div class="tab-pane show active" id="connections-tab-pane" role="tabpanel" aria-labelledby="connections-tab">
        {% include "_admin_edit_content_page_connections.tpl" noheader %}
    </div>

    <div class="tab-pane" id="referrers-tab-pane" role="tabpanel" aria-labelledby="referrers-tab">
        {% include "_admin_edit_content_page_referrers.tpl" noheader %}
    </div>

    <div class="tab-pane" id="settings-tab-pane" role="tabpanel" aria-labelledby="settings-tab">
        {% include "_admin_edit_content_acl.tpl" noheader %}

        {% if not id.is_a.meta %}
            {% include "_admin_edit_content_pub_period.tpl" %}
        {% endif %}

        {% optional include "_admin_edit_content_timezone.tpl" %}
        {% all catinclude "_admin_edit_sidebar.tpl" id languages=languages %}
    </div>
</div>

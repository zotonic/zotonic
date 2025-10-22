{% include "_admin_edit_floating_buttons.tpl" %}

{% include "_admin_edit_content_publish.tpl" noheader %}

<div> {# also sidebar #}
    {% all include "_admin_edit_content_sidebar_extra.tpl" %}

    {% include "_admin_edit_content_note.tpl" %}

    {% if id.is_a.meta %}
        {% include "_admin_edit_meta_features.tpl" %}
    {% endif %}

    {% if not id.is_a.meta %}
        {% include "_admin_edit_content_date_range.tpl" show_header %}
    {% endif %}

    <div class="minimalTabs">
        <ul class="nav nav-tabs" role="tablist">
            <li role="presentation" class="active">
                <a href="#connections" aria-controls="connections" role="tab" data-toggle="tab">
                    {_ Connected to _}
                </a>
            </li>
            <li role="presentation">
                <a href="#referrers" aria-controls="referrers" role="tab" data-toggle="tab">
                    {_ Connected from _}
                </a>
            </li>
            <li role="presentation">
                <a href="#tools" aria-controls="tools" role="tab" data-toggle="tab">
                    <i class="glyphicon glyphicon-cog"></i> {_ Settings &amp; more _}
                </a>
            </li>
        </ul>
        <div class="tab-content">
            <div role="tabpanel" class="tab-pane active" id="connections">
                {% include "_admin_edit_content_page_connections.tpl" noheader %}
            </div>
            <div role="tabpanel" class="tab-pane" id="referrers">
                {% include "_admin_edit_content_page_referrers.tpl" noheader %}
            </div>
            <div role="tabpanel" class="tab-pane" id="tools">
                {% include "_admin_edit_content_acl.tpl" noheader %}

                {% if not id.is_a.meta %}
                    {% include "_admin_edit_content_pub_period.tpl" %}
                {% endif %}
                {% optional include "_admin_edit_content_timezone.tpl" %}
                {% all catinclude "_admin_edit_sidebar.tpl" id languages=languages %}
            </div>
        </div>
    </div>
</div>

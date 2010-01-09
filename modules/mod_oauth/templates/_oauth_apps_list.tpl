<h3 class="above-list ">Registered applications</h3>

<ul class="short-list">
    <li class="headers clearfix">
        <span class="zp-15">Title</span>
        <span class="zp-30">Access to</span>
        <span class="zp-20">Consumer key</span>
        <span class="zp-20">Consumer secret</span>
        <span class="zp-15">&nbsp;</span>
    </li>

    {% for app in m.oauth_app %}
    <li title="{{ app.application_descr }}">
        <a class="clearfix">
        <span class="zp-15">{{ app.application_title }}</span>
        <span class="zp-30">
            {% for perm in m.oauth_perms.humanreadable[app.id] %}
            {{ perm.desc }}{% if not forloop.last %},{% endif %}
            {% empty %}
            &nbsp;
            {% endfor %}
        </span>
        <span class="zp-20"><tt style="font-size: 80%">{{ app.consumer_key }}</tt></span>
        <span class="zp-20"><tt style="font-size: 80%">{{ app.consumer_secret }}</tt></span>
        <span class="zp-15">
            {% button text="Edit" postback={start_edit_app id=app.id} title="Change the title, description and access permissions of this application." %}
            {% button text="Users" postback={start_tokens id=app.id} title="Show applications/users that are using this key" %}
            {% button text="Delete" postback={start_del_app id=app.id} %}
        </span>
        </a>
    </li>
    {% endfor %}
</ul>

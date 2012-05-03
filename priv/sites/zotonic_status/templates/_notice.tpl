<p class="alert alert-info" id="{{ #id }}">
    <a style="float:right" id="{{ #close }}" href="#" title="{_ close _}"><strong>[x]</strong></a>

    <strong>{{ site }}</strong><br/>
    <br/>
    {{ notice|force_escape|linebreaksbr }}
</p>
{% wire id=#close action={hide target=#id} %}

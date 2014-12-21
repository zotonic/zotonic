<div class="alert alert-info" id="{{ #id }}">
    <button type="button" class="close" data-dismiss="alert">
        <span aria-hidden="true"><em>&times;</em></span>
        <span class="sr-only">Close</span>
    </button>

    <strong>{{ site }}</strong>
    <p>
        {{ notice|force_escape|linebreaksbr }}
    </p>
</div>
{% wire id=" .close" action={hide target=#id} %}

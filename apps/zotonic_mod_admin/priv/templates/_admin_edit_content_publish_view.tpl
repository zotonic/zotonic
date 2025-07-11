<div class="btn-group">
    <a id="{{ #view }}" href="{{ page_url }}" class="btn btn-default">{_ View _}</a>
    {% wire id=#view
            postback={view id=id}
            delegate="controller_admin_edit"
    %}
    <button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
        <span class="caret"></span>
        <span class="sr-only">{_ Toggle dropdown _}</span>
    </button>

    <ul class="dropdown-menu">
        {% catinclude "_admin_view_types.tpl" id %}
    </ul>
</div>

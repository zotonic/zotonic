<div class="btn-group">
    <a href="{{ page_url }}" class="btn btn-default">{_ View _}</a></button>
    <button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
        <span class="caret"></span>
        <span class="sr-only">{_ Toggle dropdown _}</span>
    </button>

    <ul class="dropdown-menu">
        {% catinclude "_admin_view_types.tpl" id %}
    </ul>
</div>

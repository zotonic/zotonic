{% if is_editable %}
<div class="btn-group pull-right block-add-block">
    <a class="btn btn-mini dropdown-toggle" data-toggle="dropdown" href="#">
        {_ + add block _}
        <span class="caret"></span>
    </a>
    <ul class="dropdown-menu">
        <li><a href="#" data-block-type="header">{_ Header _}</li></a>
        <li><a href="#" data-block-type="text">{_ Text _}</li></a>
        {# <li><a href="#" data-block-type="media">{_ Media _}</li></a> #}
    </ul>
</div>
{% endif %}

<span id="{{ #leaderimage }}">
    {% if id.depiction %}
        {% image id width=48 height=48 crop class="leader pull-left" title=id.o.depiction[1].title %}
        {% wire id=" .leader" target=undefined action={dialog_edit_basics id=id.o.depiction[1]} %}
    {% endif %}
</span>

{% if m.zotonic_status_vcs.vcs_zotonic %}
    <a id="vcs" href="#" class="list-group-item">
        <h4 class="list-group-item-heading">{_ Update Zotonic _}</h4>
        <p class="list-group-item-text text-muted">
            {_ Perform a version update of Zotonic (through git) _}
        </p>
    </a>
    {% wire id="vcs" delegate=`mod_zotonic_status_vcs` postback={vcs_up zotonic} %}
{% endif %}

<a id="rebuild" href="#" class="list-group-item">
    <h4 class="list-group-item-heading">{_ Rebuild Zotonic _}</h4>
    <p class="list-group-item-text text-muted">
        {_ Recompile changed Erlang files in the background _}
    </p>
</a>
{% wire id="rebuild" delegate="mod_zotonic_status_vcs" postback="make" %}

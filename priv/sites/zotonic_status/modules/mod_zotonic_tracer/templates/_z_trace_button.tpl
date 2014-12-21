<a id="tracing" href="#" class="list-group-item">
    <h4 class="list-group-item-heading">{_ Toggle tracing _}</h4>
    <p class="list-group-item-text text-muted">
        {_ Enables/disables real-time tracing on Zotonic core. _}
    </p>
</a>
{% wire
    id="tracing"
    delegate="mod_zotonic_tracer"
    postback={trace}
%}

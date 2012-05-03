<div class="control-group">
    <div class="controls">
    {% button text=_"Toggle tracing" 
        class="btn"
        title=_"Enables/disables real-time tracing on Zotonic core." 
        delegate="mod_zotonic_tracer"
        postback={trace}
        %}
    </div>
</div>

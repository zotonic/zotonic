<div class="alert alert-warning">
    {% if m.admin_note.rsc[id] as note %}
        <p class="text-muted">
            <small>
                {{ note.modified|date:"Y-m-d H:i" }}
                {% if note.modifier_id %}
                    {_ by _}
                    {% include "_name.tpl" id=note.modifier_id %}
                {% endif %}
                <span class="pull-right">
                    <button id="rsc-note-btn" class="btn btn-default btn-xs">
                        {_ Edit note _}
                    </button>
                    {% wire id="rsc-note-btn"
                            action={dialog_open
                                        title=_"Edit note"
                                        template="_dialog_admin_rsc_note.tpl"
                                        id=id
                                    }
                    %}
                </span>
            </small>
        </p>
        <hr>

        <p>
            {{ note.note|escape_link }}
        </p>
    {% else %}
        <p class="text-muted">
            <small>
                <span class="glyphicon glyphicon-info-sign"></span> {_ Add a note about this page. _}
                <span class="pull-right">
                    <button id="rsc-note-btn" class="btn btn-default btn-xs">
                        {_ Add note _}
                    </button>
                    {% wire id="rsc-note-btn"
                            action={dialog_open
                                        title=_"Add note"
                                        template="_dialog_admin_rsc_note.tpl"
                                        id=id
                                    }
                    %}
                </span>
            </small>
        </p>
    {% endif %}
</div>

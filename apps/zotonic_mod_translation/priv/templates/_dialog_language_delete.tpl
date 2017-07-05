<p class="mod_translation-warning">{_ Are you sure you want to remove this language? _}</p>

<p>{_ Heads up! Removing this language will <strong>not</strong> delete any of the existing translations. But until you add (and activate) the language again, you will not be able to select or edit content written in this language. _}</p>

<div class="modal-footer">
    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
    {% button class="btn btn-primary" postback={language_delete code=code} delegate="mod_translation" text=_"Remove" %}
</div>

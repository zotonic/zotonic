<p style="color: #D84315">{_ Are you sure you want to deactivate this language? _}</p>

<p>{_ Heads up! Deactivating this language will <strong>not</strong> delete any of the existing translations. But until you activate the language again, you will not be able to select or edit content written in this language. _}</p>

<div class="modal-footer">
    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
    {% button class="btn btn-primary" postback={language_delete code=code} delegate="mod_translation" text=_"Deactivate" %}
</div>

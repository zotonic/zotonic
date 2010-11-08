<p>{_ Are you sure you want to delete the language _} <em>{{ lang.language }}</em> ({{code}})?</p>

<p>{_ This will not affect any translations in the database or .po files. This only removes the reference to the language, making it impossible to select or edit content written in the delete language. _}</p>

{% button postback={language_delete code=code} delegate="mod_translation" text=_"Delete language" %}
{% button action={dialog_close} text=_"Cancel" %}

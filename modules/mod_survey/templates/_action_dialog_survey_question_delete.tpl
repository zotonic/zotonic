<p>{_ Are you sure you want to delete this question? _}</p>

<div class="modal-footer">
    {% button class="btn" text=_"Cancel" action={dialog_close} %}
    {% button class="btn btn-primary" text="Delete" delegate=delegate postback={survey_delete_question id=id question_id=question_id} action={dialog_close} %}
</div>


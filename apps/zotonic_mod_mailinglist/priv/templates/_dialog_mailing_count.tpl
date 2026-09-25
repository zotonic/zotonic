<div id="{{ #count }}" aria-live="polite">
    <p><span class="fa fa-spinner fa-spin" aria-hidden="true"></span> <strong>{_ Counting eligible recipients… _}</strong></p>
    <p>{_ This can take a while for large mailing lists. Keep this dialog open to see the results. Nothing will be sent until you confirm. _}</p>
    <p>{_ Going back stops the count and keeps your selections. _}</p>
    {% include "_dialog_mailing_count_back.tpl" %}
</div>
{% wire action={postback postback={mailing_review_count target=#count draft=draft count_pid=count_pid} delegate="action_mailinglist_dialog_mailing_page"} %}

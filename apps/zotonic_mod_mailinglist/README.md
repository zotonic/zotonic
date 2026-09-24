# Mailing runs

Each submission creates a durable run, identified independently of the page and
mailing list. Multiple languages and repeated sends can coexist. The admin
separates **Mailings** (progress and history), **Mailing lists** (subscriptions),
and the page's **Send mailing** flow (configuration, estimate, confirmation).
Both resource editors display live run summaries.

## Persistence and delivery

- `mailinglist_run`: schedule, sender context, language policy and lifecycle.
- `mailinglist_run_recipient`: normalized, deduplicated audience snapshot and
  authoritative current outcome for each recipient.
- `mailinglist_run_message`: message IDs, retries, finality and diagnostics.
- `mailinglist_run_stats`: transactional counters by run, language and state.
  `m_mailinglist_run:rebuild_stats/2` repairs summaries from recipient rows.

Schema version 5 imports waiting schedules once. It does not reconstruct older
runs from email logs. Older history cannot be used for duplicate detection;
the send dialog explicitly warns about this. Deleting a page or list cascades
its run records. Delivery history otherwise remains intact, including retries.

The scheduler atomically claims a run. Recipient preparation freezes the
current eligible/omitted audience; submission proceeds in batches of 100.
Preview counts are estimates because membership and translations can change
before the scheduled time. Content is rendered when the email is processed,
using the resolved language recorded with the recipient.

`z_email_server:send_queued/3` acknowledges the queue transaction. A message ID
is persisted before submission. Duplicate notifications cannot inflate counts
or revert a successful delivery to queued/retrying; late bounces update the
original run. “Sent” means accepted by the mail server, not inbox delivery.
There is no open/click tracking.

The worker monitor marks stale preparation or unconfirmed handoffs interrupted.
Resume submits only pending recipients; it never resubmits an unconfirmed
handoff. Cancellation stops pending recipients; emails already handed to the
email queue may still arrive. SMTP retries continue through the email server.

New-recipient sends exclude previous successful or pending deliveries of the
same page/list/actual language. Retry sends select prior failures and recheck
current subscriptions/suppression. Explicit “send again to everyone” creates a
new run without clearing history. Language selection applies equally to
resource and email-only subscribers. Missing translations are reported as
skipped; recipients without a language use the selected fallback.

## Verification

`m_mailinglist_run_tests` contains state-ordering and language tests. Its opt-in
integration tests use `MAILINGLIST_TEST_DB` to connect to a local PostgreSQL
database as `$USER`, create an isolated schema inside a rolled-back transaction,
and mock email delivery. They exercise the real schema migration, accounting,
worker, query filters, cancellation, recovery and permissions. A separate
queue test uses a temporary Mnesia directory and does not start SMTP. Run these
opt-in tests only in a standalone Erlang VM, never in a running Zotonic node.

## Editor workflow

Choose a list and language, preview or send a single-address test without losing
these choices, review the estimated audience and timing, then confirm. Back
preserves the draft, including scheduled dates. Resends use the same review.
A review with no eligible recipients cannot be confirmed.

Results distinguish finished, nothing sent, stopped and needs attention. Skipped
recipients are excluded from sending progress. Cancellation leaves queued mail
running and the result displays those counts. Continue is offered only for
pending work or incomplete preparation; uncertain deliveries require administrator
investigation. Late delivery confirmations can resolve an interrupted run.
Resource edit panels show three recent/active mailings with links to full history.
Single-address test resends retain their address; failed-only tests honor failures.

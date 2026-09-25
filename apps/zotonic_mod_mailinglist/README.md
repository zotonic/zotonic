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

The worker monitor marks stale preparation, pending submissions or unconfirmed handoffs interrupted.
Resume submits only pending recipients; it never resubmits an unconfirmed
handoff. Delivery notifications preserve a resumed run's scheduled state until
the scheduler claims it. Cancellation stops pending recipients; emails already handed to the
email queue may still arrive. SMTP retries continue through the email server.

New-recipient sends exclude previous successful or pending deliveries of the
same page/list/actual language. Retry sends select prior failures and recheck
current subscriptions/suppression. Explicit “send again to everyone” creates a
new run without clearing history. Language selection applies equally to
resource and email-only subscribers. Automatic language selection offers two
explicit choices: send to everyone using their preferred translation (or its
base language), falling back to the selected language when necessary; or send
only when the recipient has a matching language preference. The latter skips
unavailable, unrecognized and unset preferences. Subscription, suppression and
delivery-history checks still apply in both modes. The review shows the policy
and selected counts per language. The choice survives Back, tests and resends.
Existing runs without a language policy keep their original behavior: missing
translations are skipped, while an unset preference uses the fallback.

For a specific email language, editors can select matching preferences only,
matching preferences plus recipients without a preference, or everyone. The
middle choice sends the selected language to recipients without a preference,
independently of the automatic-language fallback. Unknown or different preferences
do not count as unset. The review states which audience will receive the mailing.

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
Eligible recipients are counted in a supervised background job, so a large
list does not hold open the dialog request. The review shows a counting message
until the result arrives. Back cancels the worker and preserves the draft;
late results cannot replace a different dialog. Query failures and server
capacity limits show an error without enabling confirmation. Database queries
keep their normal timeout limits.
A review with no eligible recipients cannot be confirmed.

The immediate-send choice for regular mailings waits until the page is published
and its publication start date is reached. Future dates are shown in the dialog
and review; later publication-date changes are respected by the scheduler.
Test mailings remain immediate. An explicitly chosen mailing date remains an
independent schedule.

Results distinguish finished, nothing sent, stopped and needs attention. Skipped
recipients are excluded from sending progress. Cancellation leaves queued mail
running and the result displays those counts. Continue is offered only for
pending work or incomplete preparation; uncertain deliveries require administrator
investigation. Late delivery confirmations can resolve an interrupted run.
Resource edit panels show three recent/active mailings with links to full history.
History pagination uses a separate next-page offset, so inaccessible runs do not
hide navigation to older results, even when a page has no visible runs.
Single-address test resends retain their address; failed-only tests honor failures.

## Retention

Hourly cleanup expires recipient details three months after a run finishes.
Abandoned unfinished runs with no updates for three months also expire; future and publication-triggered
mailings are kept so delivery remains possible. Inactive date schedules more than
three months overdue also expire. Each pass marks up to 100
runs expired and deletes up to 10,000 recipient rows (message rows cascade).
Expired details are hidden immediately while any deletion backlog drains.

Run totals and language aggregates remain. Recipient email addresses, message
identifiers and diagnostics, run errors, test addresses and saved sending contexts
are removed. The first submission timestamp is retained without message details.
Late notifications cannot update expired runs, and rebuilding statistics skips them.

When any history for a page/list has expired, new-recipient and failed-only sends
are blocked. Editors must explicitly review an all-recipient mailing, with a
warning about possible duplicates. Expired runs cannot be used as resend parents,
including single-address tests, whose original address is no longer known.

## Saved content

Before submitting emails, each run saves a rendered `mailing_page.tpl` for every
language with selected recipients. Copies omit recipient-specific variables and
are not regenerated on resume. They capture the content at sending preparation;
individual emails still use their normal personalized rendering. Linked images
remain external references, and attachments are not copied into this HTML archive.

The status page links to the saved language copies, with capture timestamps.
Viewing requires permission to view the run. Archived HTML is served without
cache under a restrictive sandbox policy. These unpersonalized copies are kept
indefinitely, independently of recipient-detail retention, and are deleted only
when the associated run/page/list is deleted. Older runs are not backfilled from
current page content, as that would misrepresent what was sent.

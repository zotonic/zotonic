# Admin keyword analysis

`mod_admin_keyword` adds a **Structure → Keywords** page to the Zotonic
administration interface. Enable the module for a site and grant its `use`
permission to the roles that may inspect aggregate keyword usage.

That permission exposes aggregate counts across all matching resources,
including unpublished resources when selected. The dashboard does not expose
individual content titles; normal resource ACL checks still apply to keyword
edit links and to the linked content overview.

The dashboard analyzes `subject` edges whose object is a resource in the
`keyword` category or one of its descendants. Its three tabbed views provide:

- a horizontal usage bar chart, including unused keywords;
- a pairwise overlap matrix for the most-used keywords;
- a ranked table of all observed pairs in the displayed keyword set;
- the number of resources included by the content-category and publication
  filters;
- filters for keyword category, content category, publication flag, minimum
  usage, matrix size, and overlap measure.

## Overlap measures

For keyword resource sets `A` and `B`:

- **Common content count** is `|A ∩ B|`.
- **Jaccard similarity** is `|A ∩ B| / |A ∪ B|`. It measures general
  similarity without automatically favoring popular keywords.
- **Smaller-set overlap** is `|A ∩ B| / min(|A|, |B|)`. It reveals that a
  narrow keyword is largely contained in another keyword and is useful when
  looking for redundant or overly specific terms.

Venn diagrams are intentionally not used. They are effective for two or three
preselected sets, but do not provide a readable overview of a vocabulary with
many keywords. The matrix retains direct pairwise comparison, while the ranked
table provides an accessible and narrow-screen alternative.

The charts are server rendered and have no charting-library or custom rendering
JavaScript dependency. The tabs use the standard admin Bootstrap behavior.
Filter state is represented by the page query string, so filtered views can be
bookmarked and shared.

Guide diagrams
==============

The guides embed PNGs because zotonicwww2 imports repository-local PNG images as
ordinary media, preserves their alternative text, and detects content changes.
No browser-side diagram renderer or importer extension is required.

Use Mermaid sequence diagrams for communication between systems. Edit
mediarunner-job.mmd and regenerate its PNG with Mermaid CLI:

    npx -p @mermaid-js/mermaid-cli@12.2.0 mmdc -i doc/guides/images/mediarunner-job.mmd -o doc/guides/images/mediarunner-job.png -b white -s 2

Mermaid CLI installation and browser configuration:
https://github.com/mermaid-js/mermaid-cli

The other diagrams describe architecture and storage lifecycle. Edit their
Graphviz .dot sources, then regenerate from the repository root:

    for source in doc/guides/images/*.dot; do
        dot -Tpng "$source" -o "${source%.dot}.png"
    done

Keep the exported PNGs in version control: importing documentation must not
depend on diagram tools being installed on the documentation server. Only PNGs
are imported as media; .mmd and .dot sources are not documentation pages or images.

---
keywords:
  - reference
  - frontend_developer
  - template
  - image_management
  - url
  - render
---

::: aside
See also

- [Media](/id/doc_developerguide_media#guide-media) developer guide.
- [Media classes](/id/doc_developerguide_media#guide-media-classes) for options that are only available in mediaclass files.
- `tag#image`, `tag#image_data_url` and `tag#media` tags.
:::

Generate the url of a still image.

The `{% image_url %}` tag is used generate the url of an image. `{% image_url %}` accepts all parameters of the `{% image %}` tag but only outputs the url and not the `<img\>` element to display it.

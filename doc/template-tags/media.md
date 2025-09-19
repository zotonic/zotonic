See also

[Media](/id/doc_developerguide_media#guide-media) developer guide.

See also

[Media classes](/id/doc_developerguide_media#guide-media-classes) for some options that are only available in mediaclass files.

See also

[image](/id/doc_template_tag_tag_image), [image\_url](/id/doc_template_tag_tag_image_url) and [image\_data\_url](/id/doc_template_tag_tag_image_data_url) tags.

Show embed, video or audio media.

The `{% media %}` tag is similar to the [image](/id/doc_template_tag_tag_image) tag. It accepts the same arguments but where `{% image %}` is guaranteed to give an still image, `{% media %}` can also generate embed, video or audio elements.

The `{% media %}` tag is not implemented in the core of Zotonic. It depends on modules implementing the tag which arguments and media formats are accepted.

An example of a module using `{% media %}` is [mod\_video\_embed](/id/doc_module_mod_video_embed) which enables the use of embed code from sites as youtube and vimeo. Mod\_video\_embed will echo the embed code for a `{% media %}` tag and output a still image for an `{% image %}` tag.
See also

[Media](/id/doc_developerguide_media#guide-media) developer guide.

See also

[Media classes](/id/doc_developerguide_media#guide-media-classes) for some options that are only available in mediaclass files.

See also

[image](/id/doc_template_tag_tag_image), [image\_url](/id/doc_template_tag_tag_image_url) and [media](/id/doc_template_tag_tag_media) tags.

Generate a `data:` url of a still image.

The `{% image_data_url %}` tag is used generate a data url with the image data.

`{% image_data_url %}` accepts all parameters of the `{% image %}` tag but only outputs a data url and not the `<img\>` element to display it.

Example:


```django
{% image_data_url "lib/images/trans.gif" %}
```

Generates:


```erlang
data:image/gif;base64,R0lGODlhAQABAJAAAAAAAAAAACH5BAEUAAAALAAAAAABAAEAAAICRAEAOw==
```

The `image_data_url` tag can be used in image tags or in css:


```erlang
<img src="{% image_data_url 'lib/images/trans.gif' %}">
```
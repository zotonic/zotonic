%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc A collection of icons and other art work.

%% Copyright 2012 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_artwork).
-moduledoc("
This module contains many useful icons and images.



Included CSS icons
------------------

`lib/material-design/`

[Material Design Iconic Font](http://zavoloklom.github.io/material-design-iconic-font/).

`lib/font-awesome-4/`

Font Awesome 4: [fontawesome.io](http://fontawesome.io/).

`lib/font-awesome/`

Font Awesome 3.2.1.



### How to use Material Design icons in your templates

*   Include the CSS file: `{% lib \"material-design/css/material-design-iconic-font.css\" %}`
*   Follow the [examples](http://zavoloklom.github.io/material-design-iconic-font/examples.html)
*   Find the class names in the [icons overview](http://zavoloklom.github.io/material-design-iconic-font/icons.html)



### How to use Font Awesome icons in your templates

*   Include the CSS file: `{% lib \"font-awesome-4/css/font-awesome.css\" %}`
*   Follow the [Font Awesome examples](http://fontawesome.io/examples)
*   Find the class names in the [cheatsheet](http://fontawesome.io/cheatsheet/)



Included images
---------------

`lib/images/emotes/`

A collection of emoticons. These are from the Tango Icon Library and in the Public Domain.

`lib/images/flags/`

A collection of country flags. They are coded using the two letter ISO-3166 code of the country. For example
`lib/images/flags/flag-nl.png` refers to the Dutch flag. This collection is from Wikimedia Commons and in the Public
Domain. The PNG files are created by the Open Icon Library.

`lib/images/zotonic/`

Some Zotonic logos, with space for more.

`lib/images/mimeicons/`

Zotonic can’t generate a preview of all files. If it can’t generate a preview then an icon representing the mime
type of the file is used. This is a collection of images for some mime types. This collection is from the Tango Icon
Library and in the Public Domain.

`lib/images/noun/`

A selection of icons from The Noun Project. There are many black&white icons in this directory. This collection is
licensed with the Creative Commons Attribution 3.0 Unported (CC BY 3.0) License, though about half of the icons are in
the Public Domain or licensed using CC 0. When you use an icon in your project, check the license on
<http://thenounproject.com/> and the proper attribution as described in <http://thenounproject.com/using-symbols/>

`lib/images/social/round/`

Round icons for various social sites. Think of Youtube, Twitter, Facebook etc. This collection is made by Veodesign
(<http://veodesign.com/>) It is licensed under the Creative Commons Attribution Share-Alike 3.0 Unported License. This
means you are not allowed to sell these icons and you need to properly attribute Veodesign.

`lib/images/social/square/`

Square icons for various social sites. This collection comes from the Open Icon Library (homemade) and is in the Public
Domain. These icons are only 64x64 pixels (all others are 256x256).



### How to use images in your templates

Most of the icons are in 256x256 PNG format. That is too large for normal usage. Best is to resize the images in your
templates using the [image](/id/doc_template_tag_tag_image).

For example, to display a 64x64 pixel image:


```erlang
{% image \"lib/images/social/round/email.png\" width=64 %}
```
").

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Icons and Images").
-mod_description("Art work for your site. Social icons, file icons etc.").
-mod_prio(800).
-mod_depends([]).
-mod_provides([icons, social_icons]).

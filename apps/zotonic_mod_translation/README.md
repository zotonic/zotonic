Zotonic Translation module
==========================

Manages language selection, and recognition.
Used to serve pages in multiple languages, and add language codes to the URLs.


Language recognition
--------------------

If a resource is added without giving a language, then this module will try to guess the language using n-grams.
This is done by extracting n-grams from a text and then comparing with the statistics of n-grams in the
various language specific corpuses.

The module currently has profiles for the following languages:

| Code | Language |
| ---- | -------- |
| `af` | Afrikaans|
| `ar` | Arabic   |
| `be` | Belarusian |
| `bg` | Bulgarian|
| `bs` | Bosnian  |
| `cs` | Czech    |
| `da` | Danish   |
| `de` | German   |
| `el` | Greek    |
| `en` | English  |
| `es` | Spanish  |
| `et` | Estonian |
| `fa` | Persian  |
| `fi` | Finnish  |
| `fr` | French   |
| `fy` | Frisian  |
| `ga` | Irish    |
| `he` | Hebrew   |
| `hi` | Hindi    |
| `hr` | Croatian |
| `hu` | Hungarian|
| `id` | Indonesian |
| `is` | Icelandic|
| `it` | Italian  |
| `ja` | Japanese |
| `ko` | Korean   |
| `lt` | Lithuanian |
| `ms` | Malay    |
| `nl` | Dutch    |
| `no` | Norwegian|
| `pl` | Polish   |
| `pt` | Portuguese |
| `ro` | Romanian |
| `ru` | Russian  |
| `si` | Sinhalese |
| `sk` | Slovak   |
| `sl` | Slovenian|
| `sv` | Swedish  |
| `sr` | Serbian  |
| `sq` | Albanian |
| `ta` | Tamil    |
| `th` | Thai     |
| `tl` | Tagalog  |
| `tr` | Turkish  |
| `uk` | Ukranian |
| `vi` | Vietnamese |
| `xh` | Xhosa    |
| `zh-hant` | Cantonese |
| `zh` | Chinese |

The n-gram data is copied from Wikipedia. Typically texts about the country or the language is used.
Using texts about the country has as an advantage that typical names for the language are also
included in the statistics.


Adding a new language
---------------------

If you want to add a new language then:

 1. Add a text file with the correct language code to `priv/data/texts`
 2. Run from the Erlang command line: `translation_detect:generate_profile_data().`
 3. A new file is generated in `priv/data/profiles`, check this new file in into git.
 4. The new profiles can be loaded with: `translation_detect:load_profile_data().`

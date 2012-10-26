Page-specific dynamic backgrounds
=================================

Use edges (`page connections`) to associate backgrounds with pages.

Contributed by: Dmitrii Dimandt

Why
---

This is a small use case with a lot of words pouring into it.

I'm doing a very small project for a couple of friends. It currently resides on http://rusukr.dmitriid.com/ and will soon move to a proper domain.

What the project needed was different backgrounds for different pages. Compare, for instance, http://rusukr.dmitriid.com/products and http://rusukr.dmitriid.com/investments

Assumptions
-----------

Readers are expected to be familiar with where templates reside and how things like conditional tags work.  To benefit most you should also be comfortable getting around and editing items in Zotonic's CMS admin interface.

How
---

Now, the most obvious way to implement these backgrounds would be to
attach a media item to a page and something along the lines of::

  {% if id.medium %} 
    {% media id.medium %} 
  {% endif %}

Actually, if you create your site using the blog skeleton, you will
see similar code in `_body_media.tpl`.  Obviously, such an approach
has a severe downside. What if you actually want to attach other media
to the page? What if you want to display images and video in your
text, not just a background?

This is where custom predicates come into play. Don't fret! It's much
easier than you think

:term:`Predicate` is nothing but a fancy way to say
"relationship". Predicates define relationships between various places
(objects, entities) within Zotonic. If you are into graphs and graph
theory, a predicate is a directed labeled edge from one node to
another (see Marko Rodriguez's amazing presentations on (among oher
things) graph theory and graph algebra here:
http://www.slideshare.net/slidarko/presentations ).

What a predicate does, it defines how objects relate to other
objects. Since predicates are (usually) one-way only, you helps to
think about them as follows:

if I have a parent, how do I discern between its children? Or how do I define its children?
if I have an object, how can I have different collections of objects related to it?
So, if you have an article A, then you can have:

- a number of tags describing the article
- several authors who collaborated on it
- a few images to go with the article
    
Let's rephrase that:

- an article has tags
- an article has authors
- an article has images

Once you can make such a "has" connection, you can use a predicate::

  article -----tag predicate---> tag1, tag2, tag3, etc.
  article -----author predicate---> author1, author2, author3, etc.
  article -----image predicate---> image1, image2, image3, etc.

Easy, isn't it?

The greatest thing about predicates is that you can define your own. Here's how yu do it.

- Go to the "Predicates" section (under `Structure`) and click "Make a new predicate".
- Name it "Background", since its used to signify a dynamic background on a page.
- Click "Make predicate".
    
On the following screen we have to choose the direction of our
predicate. "From" is the entity that can have relations to other
objects, and "To" is the entity that can be had . So, our Text has an
Image as a background. This means that "From" is Text and "To" is
Image. Click the corresponding checkboxes and then click save.
  
You're done!

Now if you create a new text item (an article or news) you will see that in the Page Connections section there's now a new option, "Background". Let's try and make a new background:

- Go to Pages
- Click "Make a new media item"
- Name this item, for example, "test"
- Select an image to upload from your computer
- Click "Upload file"
- The page that opens is irreleveant to us right now. Just go back to "Pages".
- Click "Make a new page"
- Name this page, for example, "test page"
- Click on "Background" in "Page connections section"
- Type in "test"
- Select "test" media item
- Click "Save and view"

You should now see your test page... with no background on it. This
is ok, since we haven't told Zotonic how we want to display our
background.  To do this we have to edit a template. Let's try and
display the background image first. First, we can make a checklist to
see how we should proceed:

- We need to make sure the background exists
- We need to retrieve the resource associated with the background
- We need to retrieve path to the image stored on the server
- We need to make sure that this path is actually accessible from a web browser
    
The last step is especially important since Zotonic stores uploaded
files in a directory that is not directly accessible from the
web. However, for images we can use the :ref:`tag-image_url` tag to
circumvent that.

So, the code to go with our checklist is as follows::

  <div{% if id.background %}{# we check to see if background exists #}
      style="background: url( {% image_url id.background.medium.filename %}{# output web accessible URL to the image #} ) no-repeat"
      {% endif %}>
    &nbsp;
  </div>

Now that we know how to retrieve the background image we can use it to
our advantage. Our dynamic background will now look something like
this::

  <div{% if id.background %} style="background: url({% image_url id.background.medium.filename %})"{% endif %}>
    &nbsp;
  </div>

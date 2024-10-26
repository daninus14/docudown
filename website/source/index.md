{include resources/header.md}
{set-property title "docudown - Documentation for the rest of us"}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][3]
  * [Getting it][4]
  * [Documentation][5]
  * [News][6]
  * [Test results][tr]
  * [Changelog][7]

   [3]: #mailing-lists
   [4]: #downloads
   [5]: documentation/ "documentation link"
   [6]: #news
   [7]: changelog.html
   [tr]: test-report.html
   
</div>
<div class="system-description">

### What it is

Docudown is a Lisp documentation tool built on top of 
[CL-Markdown][].

{anchor mailing-lists}

### Mailing Lists

  * [docudown-devel][devel-list]: A list for questions,
    patches, bug reports, and so on.

{anchor downloads}

### Where is it

The easiest way to get setup with CL-Markdown is by using
[ASDF-Install][]. If that doesn't float your boat, there is
a handy [gzipped tar file][gzip] and a [darcs][] repository.
The darcs commands to retrieve the `docudown` source is:

    darcs get "http://common-lisp.net/project/docudown"

(note that this won't let you build `docudown` unless you
also get all of its dependencies...)

{anchor news}

### What is happening

<dl>
    <dt>27 May 2008</dt>
    <dd>Separated from CL-Markdown as part of the general 
        2008 'spring-cleaning' madness.
    </dd>
</dl>

</div>
</div>

{include resources/footer.md}

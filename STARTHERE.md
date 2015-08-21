<!-- -*- eval: (auto-fill-mode 0); eval: (visual-line-mode 1); -*- -->

This file provides a brief description of what you will find in this repository and how to create certain documents.

Make sure to read BEFORECLONING.md before proceeding with changes!

# Slides

The slides are written in latex.  There are actually several different ways to compile latex files.  The slides use a special package that enables one to pick the font---a non-trivial task for latex as of 2014.  In order to use that package one must compile the files using xelatex.  Some other packages are incompatible with xelatex, such as inputenc.  So take care when reading error messages to look for such behavior.


# Webpage

## Compiling the website

The webpage is built using Jekyll.  Jekyll is kind of like a webpage compiler.

To see how this works, start by changing to the website directory and then looking at `_compile.yml`, which is a configuration file.  It is written in YAML.  It lays out the structure of the website.  From there you can get an idea of the different directories for the site.  In `_compile.yml` you will need to edit `baseurl`, `author name`, and `author url`.

The pages are written in markdown, which is relatively easy to learn.  For instance, you can look at `syllabus/index.md` do see one such markdown file.

A webpage always looks for `index.html` if given a directory instead of a file name.  This is why inside the various directories you find different `index.md` files.  Jekyll (using pandoc or some such program) will convert the `index.md` file to an `index.html` file.

The `_site` directory is important.  This is where the compiled website wil go.

<Sta721_Fall15_web/Makefile> shows how to compile the webpage.  In particular, do
```
make build
```
to build the website.  You can then preview the website by doing
```
make serve
```.
This will run a small webserver that let's you view the website on port 4000 (or perhaps some other port if so specified).  Look at the output to see the correct address.  It should be something like `http://127.0.0.1:4000/` or `http://localhost:4000/`.  Jekyll will automatically update that site as you edit files.

To transfer the files you need to set the approprate variables in `Makefile` and then do `make push`, which syncs the `_site` directory via `rsync`.

## Jekyll

For instructions on installing jekyll, go here: <http://jekyllrb.com/docs/installation/>.

Briefly, to install Jekyll do
```
sudo gem install jekyll
```.

You may need to restart your shell for bash to see jekyll.

You will need to install Ruby (I added through MacPorts with no difficulty)

## Schedule

The schedule is created using a .csv file.  Go to `_include/`.  It will have a .csv file you can edit to create your schedule.  Jekyll will use this .csv file to create a corresponding html table.

# Grading

Do as much of the grading as possible on Sakai.  You should be able to do
everything except the Clicker, Individual Readiness Assessment grades, and peer
evaluations on Sakai.


# Student Requests

Sometimes students will make special requests about missing class or something else.

First, ask yourself if you could grant the request to everyone in class.  If you could not, then say no.

Second, tell the student that any requests they make need to be passed through their academic dean.  Their academic dean is supposed to be apprised of personal reaons that might justify a student missing class.  If they are unwilling to go through their dean, then it isn't a legitimate request.

* * *

# Advanced Topics

## Text Editors

If multiple people are contributing to the repository it is helpful to use the same text editor.  The tex files in this repository were created using emacs.  TeXShop wraps lines.

Emacs can deal with long lines in two ways.  It breaks them or wraps them.  You can toggle these behaviors by doing M-X and auto-fill-mode or visual-line-mode respectively.  Most files in this repository are created using the visual-line-mode.

To get Emacs to run XeLaTeX see <http://tex.stackexchange.com/questions/21200/auctex-and-xetex>.

## Merging

You can use Emacs to manually merge two files.  This might be useful if you have a custom file but want to incorporate changes made to the default file.

This
[page](https://www.gnu.org/software/emacs/manual/html_node/emacs/Overview-of-Emerge.html#Overview-of-Emerge)
shows how to invoke emerge while this
[page](https://www.gnu.org/software/emacs/manual/html_node/emacs/Merge-Commands.html#Merge-Commands)
shows how to control the merging process.

This [page](http://stackoverflow.com/questions/278081/resolving-a-git-conflict-with-binary-files) shows you how to resolve conflicts for binary files while this [page](https://help.github.com/articles/resolving-a-merge-conflict-from-the-command-line/) shows you how to resolve conflicts for text files.

# Emacs Configuration

Andreas Santucci  
santucci@stanford.edu

With full credit to:
https://github.com/jhamrick/emacs/blob/master/.emacs

http://blog.smalleycreative.com/tutorials/using-git-and-github-to-manage-your-dotfiles/

## Installing

Clone Hamrick's repository, linked above, and then run the `bootstrap.sh` script. This
will copy all of the necessary files to `~/.emacs` and
`~/.emacs.d`. 

## Emacs plugins

We use the plugins provided by Hamrick's .emacs file. In addition, we include support for R and ESS. These additions are installed manually. For instructions on installing ESS, see http://ess.r-project.org/Manual/ess.html#Installation.

## Gotchas

Here are some issues I or others have run into when installing this configuration.

### Emacs version

Ubuntu currently (December, 2015) supports emacs 23. For some of the plug-in's to work, we need emacs 24. This is not yet deemed a "stable" release by Ubuntu, so it's not available through the aptitude repository. For instructions on installing the latest version, see http://askubuntu.com/questions/437255/how-to-install-emacs-24-4-on-ubuntu.


### Plugins

We must install the following version-control systems: `hg`, `git`, `bzr`, and `cvs`.

I encountered errors automatically installing Auctex, and followed instructions [https://github.com/jhamrick/emacs/blob/master/README.md#trouble-building-auctex](here) to work around the issue; see section on 


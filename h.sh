#! /bin/sh

# this script is called after each git command, after 
# the 'bundle' and 'remove' command which have no git equivalent, and after the
# extraction of a bundle.
# The following environment variables are set:
#     - HSH_ROOT: current hsh root;
#     - HSH_REPOSITORY: current repository name;
#     - HSH_ACTION: current git or hsh action, for "bundle" command, HSH_ACTION
#       is set to 'bundle-in', when extracting a bundle the action is set to
#       'bundle-out'.
# Additionally, for bundle-in and bundle-out actions the HSH_BUNDLE_ROOT is set
# to the bundle content root.

case "$HSH_ACTION" in
	clone|bundle-out)
		yes | pk install emacs
		yes | pk install git
		[ -e "$HSH_ROOT/.emacs.d" ] && rm -rf "$HSH_ROOT/.emacs.d"
		git clone --depth 1 https://github.com/hlissner/doom-emacs $HSH_ROOT/.emacs.d
		yes | "$HSH_ROOT/.emacs.d/bin/doom" -p ~/.config/doom install

		# if command -v systemd >/dev/null
		# then
		#     systemctl --user daemon-reload
		#     systemctl --user enable emacs
		#     systemctl --user start emacs
		# fi
		yes | "$HSH_ROOT/bin/pk" install aspell
		yes | "$HSH_ROOT/bin/pk" install aspell-fr
		yes | "$HSH_ROOT/bin/pk" install aspell-en
		;;
esac

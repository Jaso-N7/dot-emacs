{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "jason";
  home.homeDirectory = "/home/jason";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    
    # Utils
      brave
      tree
      wmctrl
      xorg.xmodmap
      nixos-generators

      # XFCE4
      xarchiver
      xfce.thunar-archive-plugin

      # Desktop Themes
      nordic
      numix-solarized-gtk-theme
      paper-gtk-theme
      paper-icon-theme
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
    
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/jason/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  
  # EMACS
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.xah-fly-keys
      epkgs.nix-mode
      epkgs.lispy
      epkgs.paren-face
      epkgs.erlang
      epkgs.lfe-mode
      epkgs.web-mode
      epkgs.which-key
      epkgs.company
      epkgs.tron-legacy-theme
    ];
    extraConfig = ''
    ;; Initial window and default window
    (setq inhibit-startup-screen t)
    ;; Don't use messages that you don't read
    (setq initial-scratch-message "")
    (setq inhibit-startup-message t)
    (setq inhibit-startup-echo-area-message "Welcome, User!")

    ;; Graphical User interface settings
    (if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
    (tool-bar-mode -1)
    (column-number-mode 1)
    (blink-cursor-mode 0)
    ;; Turn off Audio Bell
    (setq visible-bell t)
    ;; Who uses the bar to scroll?
    (scroll-bar-mode 0)
    ;; Display time
    (display-time)
    ;; Save backups to a central directory
    (setq backup-directory-alist `(("." . "~/.emacs.saves")))

    ;; See http://bzg.fr/emacs-hide-mode-line.html
    (defvar-local hidden-mode-line-mode nil)
    (defvar-local hide-mode-line nil)

    (define-minor-mode hidden-mode-line-mode
    "Minor mode to hide the mode-line in the current buffer."
    :init-value nil
    :global nil
    :variable hidden-mode-line-mode
    :group 'editing-basics
    (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
          (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

	;; Activate hidden-mode-line-mode
	(hidden-mode-line-mode 1)

	;; If you want to hide the mode-line in all new buffers
	(add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

	;; Command to toggle the display of the mode-line as a header
	(defvar-local header-line-format nil)
	(defun mode-line-in-header ()
  	(interactive)
  	(if (not header-line-format)
    	  (setq header-line-format mode-line-format
    	        mode-line-format nil)
    		(setq mode-line-format header-line-format
    	      header-line-format nil))
  	(set-window-buffer nil (current-buffer)))
	(global-set-key (kbd "C-s-SPC") 'mode-line-in-header)

;;; XAH FLY KEYS
    ;; put this BEFORE loading Xah Fly Keys
    (setq xah-fly-use-meta-key nil)
    (require 'xah-fly-keys)
		;; specify a layout.
		(xah-fly-keys-set-layout "koy")
    ;; make esc key do cancel. works only in gui emacs
    (define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
    (setq xah-fly-command-mode-indicator
          (propertize "⦿" 'face '(:foreground "red")))   
		(xah-fly-keys 1)

;;; NIX
    (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
		
;;; LISPY
		(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
		(add-hook 'lfe-mode-hook (lambda () (lispy-mode 1)))
		(add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))
		
;;; PAREN FACE
		(global-paren-face-mode 1)
		
;;; WHICH KEY
		(which-key-mode)
		
;;; WEB MODE
		(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
		
;;; COMPANY MODE
		(add-hook 'after-init-hook 'global-company-mode)
		
;;; THEME
;		(load-theme 'tron-legacy t)
;		(setq tron-legacy-theme-vivid-cursor t)
    '';
  };
}

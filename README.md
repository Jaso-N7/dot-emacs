# .Emacs

Personal `.emacs` configuration, mostly system agnostic. Since I rarely use Windows for development, this is mainly biased towards a Unix Programming Environment.

Tested on BSD, Linux & MacOS.

### Main Focus

Listed in order of decreasing importance:

- Common Lisp (CCL, ABCL, Allegro, CMUCL)
- Web Development
- General editing tasks
- Clojure

## Installation

Back up your current `.emacs` & `.emacs.d/init.el`, then `cp`, `mv` or `rm` the originals, prior to using this setup:
```bash
cd ~ # run all commands from home directory
git clone https://github.com/Jaso-N7/dot-emacs.git
ln -s dot-emacs/.emacs .emacs
ln -s dot-emacs/.emacs.d/init.el .emacs.d/init.el
```

## Usage

All configuration options are in `.emacs.d/init.el`. Prior to launching Emacs, edit `init.el` and (de)activate / tweak options as needed.

`use-package` is the main method of installing, maintaining and configuring packages.

```bash
# ensure everything is okay
emacs --debug-init &
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[BSD 3-Clause](https://choosealicense.com/licenses/bsd-3-clause/)
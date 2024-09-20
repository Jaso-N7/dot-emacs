# .Emacs

This is a NixOS configuration managed by `home-manager`

Personal `.emacs` configuration, mostly system agnostic. Since I rarely use Windows for development, this is mainly biased towards a Unix Programming Environment.


### Main Focus

Listed in order of decreasing importance:

- Lisp Flavoured Erlang (LFE)
- Web Development
- General editing tasks

## Installation

- Complete `home-manager`'s [Standalone installation](https://nix-community.github.io/home-manager/index.xhtml#sec-install-standalone).
- Then:
```bash
cd ~ # run all commands from home directory
git clone https://github.com/Jaso-N7/dot-emacs.git 
for file in dot-emacs/*.nix 
do 
  echo $file ": copying..."
  ln -s dot-emacs/$file .config/home-manager/$file
done
```

## Usage

All configuration options are in `emacs.nix`. Prior to launching Emacs, edit `emacs.nix` and (de)activate / tweak options as needed. `home.nix` will have the basic Emacs configurations.

`emacs.nix` implements `use-package` as the main method of installing, maintaining and configuring packages.

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
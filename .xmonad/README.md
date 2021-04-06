Requires:

- libx11-dev
- libxrandr-dev


Among others.

### Useful things:

Check if built with xinerama support:

```
stack exec -- ghc -e Graphics.X11.Xinerama.compiledWithXinerama
```

If not, make it so! I've just deleted my `~/.stack` folder to make it rebuild.

- arandr, to arrange layouts:

```
sudo apt install arandr
```

# OM-CKN

These are some tools used in my compositional process. It is yet under development so I am changing a lot of things, name of objects and this kind of thing, so I will not provide documentation yet!.

# [Download](https://bit.ly/3eqkPBK)

## MacOS
Install FluidSynth MacOS.

* Run `/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"` on Terminal.
* Then run `brew install fluidsynth`
* Go to preferences and in External->OM-CKN->Pure Data Executable and look, in Applications Folders, for `PD{VERSION}/Contents/Resources/bin/pd`.
* In `PD OSC Player->SoundFont Folder` choose the path where are located all or SoundFonts (it must be `.sf2` or `sf3`)
* After restart OM-Sharp and choose the standart SoundFont that will be used to play voices and chords in `Preferences->External-SoundFont->PD OSC Player->SoundFont`.
* If you do not want to use PD OSC Player uncheck the box `PureData Player`.

# massmove

This is a proof of idea that Haskell can be used for routine systems administration tasks, like moving millions of files from a single directory into a manageable tree-like directory structure.

## Usage

Call this tool like this:

```
shell$ massmove-exe treeDepth::Int src::FilePath dst::FilePath
```

This will copy all the files (files only, not recursively) from a given directory `src` into a subtree of directories it will create under `dst`. `src` and `dst` must exist.

`dst` will end up containing up to 255 subdirectories, each of which will contain up to 255 subdirectories, and so on. The actual files will be located at the deepst level of the sub-directories tree. The depth of this tree should be specified as the first parameter `treeDepth` (an integer, 1..9).

The sub-directories tree suffix is created from a hexadimical `SHA256` hash of a filename, so that it can always be recreated from the filename itself, so that a file could always be adressed directly by it's name.

The application can run for quite a long time if the `src` directory has a lot of files. Like, 10 hours for 2.000.000 files (depends on hardware storage device IO throughput). 

Memory usage is constant. Limiting factor usually will be IO, not CPU.

The application will print some performance statistics every second. The application can be stopped at any time by pressing `Ctrl-c`.

## Caution

Currently this application does no resource management.

## TODO

Feel free to make pull requests and/or other suggestions :-)


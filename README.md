Lighthouse is a builder, project manager and package manager for [SOPL](https://github.com/Dimitar85898/SOPL)

## Requirements
```
sopl compiler

Additional things to make life easier:
any nasm version
any gcc version   
```

## Getting started
To get started just build the project with cargo or rustc for your desired platform and then use the executable that gets generated.
```
cargo build --release 
```

To initalize a basic project use:
```
lighthouse init (Name => defaults to current directory name)
```
Then to run your project simply do:
```
lighthouse run
```
To verify that everything is correct you can use:
```
lighthouse verify
```
## Customization
You can customize where you want to put things in your project by editing the lighthouse.cfg file
When you initialize a project, the config file usually looks something like this:
```cfg
## [NOTE] configs for lighthouse do support comments
name="{PROJECT NAME}"
intpath="{PATH TO CURRENT FOLDER}\output\int"
binpath="{PATH TO CURRENT FOLDER}\output\bin"
entry="{PATH TO CURRENT FOLDER}\src\main.spl"
```
Some things you may want to change:
```cfg
target="nasm_x86_64"  ## You can change the target (Check sopl documentation for currently supported targets)
```
## What does it do?
It makes it easier to manage your sopl code. SOPL is supposed to be the core part of the manager, with its purpose being - easier testing, running, etc.

### Initialization
By default when you initialize a new lighthouse project, lighthouse will generate the following things in the current folder:
```
src\
    main.spl
output\
    bin\
    int\
lighthouse.cfg
.gitignore
```
[NOTE] If you already have a .gitignore in your current folder, lighthouse will append "output/" to it.

### Building
By default when you build your project, the resulting .asm file will be put inside the output\int directory under the same name as the source file. For example:
```
src\main.spl -> output\int\main.asm
```
### Running
By running the program you are essentially building it, but also using things such as nasm and gcc to run it.
The object files will be stored in the output\int directory whilst the executable/s will be stored in the output\bin directory.
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
lighthouse init 
```
Then to run your project simply do:
```
lighthouse run
```
To verify that everything is correct you can use:
```
lighthouse verify
```
## Flags
```
lighthouse (command) [flags]
   init                  // Initializes an new project
      --type [bin|lib]   // Sets the type of project - library or program
      --name [name]      // Sets the name of the project
   build                 // Builds the assembly with sopl
   run                   // runs the current project
   verify                // verify everything is correct with the current project
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

build=[]              ## You can build certain parts of your project to then be linked together with the linker

arch="linux_x86_64"   ## You can also make it "custom" which would require you to also have arch_path as a variable that points to the json file (in the same folder you have to also have a cfg file with the same name as the json - checkout examples/arcs in SOPL repository)

linker="gcc"          ## Specify which linker to use

local_dependencies=[] ## What things do you depend on? With this you can shorten your including from ``` include "../libs/strlib/strlib.spl" ``` to just ``` include "strlib/strlib.spl" ```. For right now it just adds the -i flag but in the future it may add more

mode="release"        ## Change for which mode of sopl you are building -release or none
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
By running the program you are essentially building it, but also using things such as nasm and gcc to compile it, and then at the end running it.
The object files will be stored in the output\int directory whilst the executable/s will be stored in the output\bin directory.
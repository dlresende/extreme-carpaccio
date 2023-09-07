# Extreme Carpaccio C++ starting code

By default, the kata's CMake file is referencing C++ version 17.


## Getting ready

### 1 - Clone the kata repository

### 2 - Go to the kata's `cpp` directory

### 3 - Run the build setup script

The kata comes with a setup script to simplify initializing the project.

This setup script does the following:

- Create a build directory: ***cpp/build***. All build-related files are generated under this directory.
- Download a copy of cmake compatible with your platform.
- Download the dependencies required to build and test the kata (such as GoogleTest).
- Generate the solution file ***Extreme-Carpaccio.sln*** for **Visual Studio 2017** on Windows,
 or the project file ***Extreme-Carpaccio.xcodeproj*** for **Xcode** on macOS.
- Run an initial build and test of the kata to ensure that everything is set up properly.

```shell
./cpp_easy_setup.sh
```

## Project organization

C++ HTTP client is implemented in "Client" folder. HTTP client executable, once built, can be found under build\bin folder.

Order handling related code is under "Order Management" folder.


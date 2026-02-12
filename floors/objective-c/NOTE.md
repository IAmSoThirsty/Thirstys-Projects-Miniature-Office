# Build Note

This Objective-C floor requires either:
1. **macOS** with Xcode Command Line Tools (recommended)
2. **Linux** with GNUstep runtime

## Linux Build Issues
The GitHub Actions Ubuntu environment does not have GNUstep installed by default.
To build on Linux, first install GNUstep:

```bash
sudo apt-get update
sudo apt-get install -y gnustep-devel gobjc libobjc-4-dev clang libgnustep-base-dev libblocksruntime-dev
```

Or run:
```bash
make install-deps-linux
```

Note: GNUstep uses the legacy Objective-C runtime and does not support ARC.
The code is designed to work with both ARC (macOS) and manual reference counting (Linux/GNUstep).

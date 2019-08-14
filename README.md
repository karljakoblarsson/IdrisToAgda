# IdrisToAgda
A master thesis project implementing a Idris to Agda transpiler.

This project uses git submodules init them with:

```
git submodule init
git submodule update
```

This project uses Stack for dependency management and building.

Build with:
```
stack build
```

Run with:
```
stack run -- ita -h
```

Run stats tool with:
```
stack run -- sts -h
```

## Issues
This project uses Idris' custom Setup.hs to build Idris as a library. However
that setup script rebuild all of Idris' std library and run-time files for
every rebuild. This takes a long time. Those parts can be commented out while
developing. In Setup.hs: lines 299-301 and 320-325.


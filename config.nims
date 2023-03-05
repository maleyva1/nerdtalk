switch("gc", "orc")

if not defined(release):
    switch("debugger", "native")

when findExe("mold").len > 0 and defined(linux):
    switch("passL", "-fuse-ld=mold")

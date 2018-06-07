export SBT_OPTS="-Xmx10G"
export LD_LIBRARY_PATH=:${PWD}/lib
sbt -Djava.library.path=lib -J-Xmx10G "project viz" "run"

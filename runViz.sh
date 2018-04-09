export SBT_OPTS="-Xmx6G"
export LD_LIBRARY_PATH=:${PWD}/lib
sbt -Djava.library.path=lib -J-Xmx6G "project viz" "run"
